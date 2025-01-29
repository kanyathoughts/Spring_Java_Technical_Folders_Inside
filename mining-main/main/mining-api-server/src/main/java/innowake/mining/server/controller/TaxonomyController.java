/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import graphql.com.google.common.collect.Streams;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.Logging;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.job.TaxonomyAssignmentJob;
import innowake.mining.server.job.TaxonomyImportJob;
import innowake.mining.server.job.TaxonomyValidationJob;
import innowake.mining.server.job.identification.IdentifyTechnicalTaxonomiesJob;
import innowake.mining.server.service.TaxonomyModelService;
import innowake.mining.server.service.TryLock;
import innowake.mining.server.util.CSVReaderHeaderAware;
import innowake.mining.shared.PatternConverter;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.model.TaxonomyFieldName;
import innowake.mining.shared.model.TaxonomyImportValidationResult;
import innowake.mining.shared.model.TaxonomyImportValidationResult.Marker;
import innowake.mining.shared.model.TaxonomyImportValidationResult.MarkerType;
import innowake.mining.shared.model.TaxonomyReport;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsGetRequest;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsGetResponse;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for Taxonomy requests.
 * 
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class TaxonomyController extends BaseController {

	/**
	 * URL pattern for Taxonomies for Project.
	 */
	public static final String TAXONOMIES_COLLECTION_URL = "/v1/projects/{projectId}/taxonomies";

	/**
	 * URL pattern for Taxonomy by ID.
	 */
	public static final String TAXONOMY_BY_ID_URL = "/v1/projects/{projectId}/taxonomies/{taxonomyId}";

	/**
	 * URL pattern for Taxonomy Reports for Project.
	 */
	public static final String TAXONOMY_REPORTS_URL = "/v1/projects/{projectId}/taxonomies/reports";

	/**
	 * URL pattern for Taxonomy Identification for Project.
	 */
	public static final String IDENTIFY_TAXONOMIES_URL = "/v1/projects/{projectId}/identify-technical-taxonomies";
	
	/**
	 * URL pattern to validate the Taxonomy assignment list before importing.
	 */
	public static final String TAXONOMY_IMPORT_VALIDATE_URL = "/v1/projects/{projectId}/taxonomies/import/validate";

	/**
	 * URL pattern to assign the taxonomy data to the modules of specified project.
	 */
	public static final String TAXONOMY_IMPORT_URL = "/v1/projects/{projectId}/taxonomies/import";
	
	/**
	 * URL pattern to assign taxonomy to modules.
	 */
	public static final String ASSIGN_TAXONOMY_URL = "/v1/projects/{projectId}/taxonomies/assignments";
	
	/**
	 * URL pattern to assign taxonomy to modules via Job.
	 */
	public static final String ASSIGN_TAXONOMY_JOB_URL = "/v1/projects/{projectId}/taxonomies/bulk-assignment";
	
	/**
	 * URL pattern for aggregated values 
	 */
	public static final String AGGREGATIONS_URL = "/v1/projects/{projectId}/taxonomies/aggregations";
	
	/**
	 * URL pattern for Sum of Line of Codes values 
	 */
	public static final String SLOC_URL = "/v1/projects/{projectId}/taxonomies/aggregations/sloc-by-type";

	/**
	 * Parameter name for importing the file.
	 */
	private static final String FILE = "file";
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);
	
	private final JobManager jobManager;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	protected TaxonomyModelService taxonomyModelService;
	
	/**
	 * Creates a new controller instance.
	 * 
	 * @param jobManager the {@link JobManager} to use
	 */
	public TaxonomyController(@Autowired final JobManager jobManager) {
		this.jobManager = jobManager;
	}
	
	/**
	 * Returns all available {@linkplain TaxonomyPojo taxonomies} for a given project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param type the type of the Taxonomy
	 * @param countOnlyModulesWithTaxonomyId the list of ID of the taxonomy to consider for the reference count
	 * @return a list of {@link TaxonomyPojo}
	 */
	@GetMapping(value=TAXONOMIES_COLLECTION_URL)
	@Operation(summary = "Get all taxonomies for one module", operationId = "findAllTaxonomies")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<TaxonomyPojo> findAll(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "restricts the type of the returned taxonomies", required = false)
			@RequestParam (required = false) final String type,
			@Parameter(description = "includes only modules that have a certain taxonomy in the reference count", required = false)
			@RequestParam (required = false) final List<EntityId> countOnlyModulesWithTaxonomyId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return taxonomyService.find(q -> {
				q.ofProject(projectId).sortId(SortDirection.ASCENDING);
				if (! StringUtils.isEmpty(type)) {
					q.withTypeName(type);
				}

				if (countOnlyModulesWithTaxonomyId != null && ! countOnlyModulesWithTaxonomyId.isEmpty()) {
					q.withModuleCountsReferencingTaxonomies(countOnlyModulesWithTaxonomyId);
				}
			});
	}

	/**
	 * Find an existing {@link TaxonomyPojo} by id.
	 *
	 * @param request access to the request
	 * @param response access to the response.
	 * @param projectId the project id of the {@link TaxonomyPojo}
	 * @param taxonomyId the ID of the {@link TaxonomyPojo}.
	 * @return the {@link TaxonomyPojo} with the specified record id.
	 */
	@GetMapping(value=TAXONOMY_BY_ID_URL)
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Operation(summary = "Find a taxonomy by ID", operationId = "findTaxonomyById")
	@Nature({MINING})
	@Role({VIEWER})
	public TaxonomyPojo findById(
	        final HttpServletRequest request,
	        final HttpServletResponse response,
	        @Parameter(description = "the project ID", required = true, example = "0")
	        @PathVariable final EntityId projectId,
	        @Parameter(description = "the ID to search for", required = true)
	        @PathVariable final EntityId taxonomyId) {
	    validate(request);
	    response.setStatus(HttpStatus.OK.value());
	    return taxonomyService.findAny(q -> q.ofProject(projectId).byId(taxonomyId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find taxonomy for id (" + taxonomyId + ") and project (" + projectId + ")"));
	}
	
	/**
	 * Creates a new Taxonomy.
	 *
	 * @param request access to the request
	 * @param response access to the response.
	 * @param projectId the project id of the taxonomy to create.
	 * @param taxonomy the {@link TaxonomyPojoPrototype} to create .
	 * @return the new {@link TaxonomyPojo}.
	 */
	@PostMapping(value=TAXONOMIES_COLLECTION_URL)
	@Operation(summary = "Create a new taxonomy", operationId = "createTaxonomy")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given taxonomy is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.TAXONOMIES, reasonPhrase = "Applied Lock on Create new taxonomy")
	public TaxonomyPojo create(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The taxonomy to create", required = true) 
			@RequestBody final TaxonomyPojoPrototype taxonomy) {
		validate(request);
		taxonomy.setProject(projectId);
		final EntityId newTaxonomy = taxonomyService.create(taxonomy);
		response.setStatus(HttpStatus.CREATED.value());
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		return taxonomyService.get(newTaxonomy);
	}
	
	/**
	 * Updates an existing Taxonomy.
	 *
	 * @param request access to the request
	 * @param response access to the response.
	 * @param projectId the project id of the Taxonomy to update.
	 * @param taxonomyId the id of the Taxonomy to update.
	 * @param taxonomy the updated {@link TaxonomyPojoPrototype}.
	 * @return the updated {@link TaxonomyPojo}.
	 */
	@PutMapping(value=TAXONOMY_BY_ID_URL)
	@Operation(summary = "Update a taxonomy", operationId = "updateTaxonomy")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given taxonomy is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or taxonomy ID does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.TAXONOMIES, reasonPhrase = "Applied Lock on Update taxonomy")
	public TaxonomyPojo update(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the taxonomy to update", required = true, example = "0")
			@PathVariable final EntityId taxonomyId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated taxonomy", required = true) 
			@RequestBody final TaxonomyPojoPrototype taxonomy) {
		validate(request);
		projectId.matchOrApply(taxonomy.project.orElseNonNull(EntityId.VOID), taxonomy::setProject);
		taxonomyId.matchOrApply(taxonomy.identityProvisional(), taxonomy::withId);
		
		response.setStatus(HttpStatus.OK.value());
		taxonomyService.update(taxonomy);
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		return taxonomyService.get(taxonomy.identityProvisional());
	}
	
	/**
	 * Delete an existing Taxonomy.
	 *
	 * @param request access to the request
	 * @param response access to the response.
	 * @param projectId the project id of the Taxonomy to delete.
	 * @param taxonomyId the id of the Taxonomy to delete.
	 */
	@DeleteMapping(value=TAXONOMY_BY_ID_URL)
	@Operation(summary = "Delete a taxonomy", operationId = "deleteTaxonomy")
	@ApiResponse(responseCode = "204", description = "regardless of the existence of the taxonomy")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.TAXONOMIES, reasonPhrase = "Applied Lock on Delete Taxonomy")
	public void delete(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the taxonomy ID", required = true, example = "0")
			@PathVariable final EntityId taxonomyId) {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		taxonomyService.delete(q -> q.byId(taxonomyId));
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
	}
	
	/**
	 * Returns all available {@link TaxonomyReport taxonomyReports} for a given project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response.
	 * @param projectId the ID of the project.
	 * @return a list of taxonomyReports.
	 */
	@GetMapping(value=TAXONOMY_REPORTS_URL)
	@Operation(summary = "Gets all taxonomies for one project", operationId = "getTaxonomyReport")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<TaxonomyReport> getTaxonomyReport(
			final HttpServletRequest request, 
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		
		return taxonomyModelService.findReports(q -> q.ofProject(projectId));
	}
	
	/**
	 * Starts an {@link IdentifyTechnicalTaxonomiesJob} that identifies all Technical Taxonomies for the given modules. 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleMatcher the paths of the modules to identify the Technical Taxonomies for
	 * @return the Id of the {@link IdentifyTechnicalTaxonomiesJob}
	 */
	@PostMapping(value=IDENTIFY_TAXONOMIES_URL)
	@Operation(summary = "Starts a job that identifies Technical Taxonomies for the given modules and returns the job Id. "
			+ "See the job status at '/v1/jobs/{jobId}/info'", operationId = "identifyTechnicalTaxonomies")
	@ApiResponse(responseCode = "202", description = "the job has successfully been submitted")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] identifyTechnicalTaxonomies(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "List of id's and/or the file paths of the modules to identify the candidates "
					+ "for example = {1}, {src/file.cbl}", required = true)
			@RequestBody final ModuleMatcher moduleMatcher) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		final var job = new IdentifyTechnicalTaxonomiesJob(projectId, moduleMatcher);
		final var jobMonitor = jobManager.submit(job);
		/* Explicitly returning the job Id as a char array, as a plain String would require a wrapper object for valid JSON. */
		return jobMonitor.getJobId().toCharArray();
	}
	
	/**
	 * Validates the taxonomy assignments.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param file the CSV file to import
	 * @return the submitted job ID
	 */
	@PostMapping(value=TAXONOMY_IMPORT_VALIDATE_URL, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@ApiResponse(responseCode = "200", description = "if the request was successful -- "
				+ "If IOException occures sending response with MarkerType - Error with empty result.")
	@Role({EDITOR})
	@Nature({MINING})
	public ResponseEntity<?> validateImportTaxonomy(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "the CSV file with taxonomies to import", required = true)
			@RequestParam(name = FILE, required = true)
			final MultipartFile file) {

		validate(request);
		try (final InputStream inputStream = file.getInputStream()) {
			final List<Map<String, String>> importLines = CSVReaderHeaderAware.linesFromCsv(inputStream);
			final TaxonomyValidationJob job = new TaxonomyValidationJob(projectId, importLines);
			return ResponseEntity.ok().body(jobManager.submit(job).getJobId().toCharArray());
		}
		catch (final IOException e) {
			final String message = String.format("Error while parsing file: %s", e.getMessage());
			LOG.error(() -> message, e);
			final TaxonomyImportValidationResult taxonomyImportValidationResult = new TaxonomyImportValidationResult(MarkerType.ERROR,
					Collections.singletonList(new Marker(1, Collections.emptyList(), Collections.emptyList(), MarkerType.ERROR, message)));
			return ResponseEntity.ok().body(taxonomyImportValidationResult);
		}
	}

	/**
	 * Imports the taxonomy assignments to the modules as specified in CSV file for a specified project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param file the CSV file to import
	 * @return the submitted job ID
	 * @throws IOException if an error occurs while reading from the file
	 */
	@PostMapping(value=TAXONOMY_IMPORT_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Import the taxonomy assignments from csv file.")
	@ApiResponse(responseCode = "202", description = "if the import was successful")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Role({EDITOR})
	@Nature({MINING})
	public char[] importTaxonomy(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "the CSV file with taxonomy data to import", required = true)
			@RequestParam(name = FILE, required = true)
			final MultipartFile file) throws IOException {

		validate(request);

		try (final InputStream inputStream = file.getInputStream()) {
			final List<Map<String, String>> importLines = CSVReaderHeaderAware.linesFromCsv(inputStream);
			final TaxonomyImportJob job = new TaxonomyImportJob(projectId, importLines);
			response.setStatus(HttpStatus.ACCEPTED.value());
			return jobManager.submit(job).getJobId().toCharArray();
		} catch (final IOException e) {
			final String message = String.format("Error while parsing file: %s.", StringUtils.isBlank(e.getMessage()) ? "Reason unknown" : e.getMessage());
			LOG.error(() -> message, e);
			response.sendError(HttpStatus.BAD_REQUEST.value(), message);
		}
		return new char[0];
	}
	
	/**
	 * Retrieves TaxonomyPojos assigned to provided {@link Module} IDs
	 *
	 * @param request access to the request
	 * @param response response the HTTP response
	 * @param projectId the ID of the project
	 * @param taxonomyAssignmentsGetRequest request body containing module IDs
	 * @return response with retrieved {@link TaxonomyPojo}s
	 */
	@PostMapping(value = ASSIGN_TAXONOMY_URL)
	@Operation(summary = "Retrieve assigned taxonomies to modules", operationId = "getAssignedTaxonomyByModule")
	@ApiResponse(responseCode = "200", description = "If the retrieval was successful")
	@ApiResponse(responseCode = "404", description = "If the given project does not exist")
	@Role({VIEWER})
	@Nature({MINING})
	public TaxonomyAssignmentsGetResponse getTaxonomyAssignments(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The module IDs", required = true)
			@RequestBody final TaxonomyAssignmentsGetRequest taxonomyAssignmentsGetRequest) {
		validate(request);
		
		final var moduleMatcher = taxonomyAssignmentsGetRequest.getModules();
		final var matchingModules = Streams.concat(
												moduleMatcher.getIds().isEmpty() ? Stream.empty() :
																					moduleService.findModuleUids(q -> q.ofProject(projectId)
																														.byIds(moduleMatcher.getIds()))
																								.stream(),
												moduleMatcher.getPathPatterns().isEmpty() ? Stream.empty() :
																					moduleService.findModuleUids(q -> q.ofProject(projectId)
																														.withPathPatternsSelfOrContaining(projectId, moduleMatcher.getPathPatterns().stream()
																																										.map(PatternConverter::convertAntToRegexPattern)
																																										.toList()))
																								.stream())
										.collect(Collectors.toSet());
		final int moduleCount = matchingModules.size();
		final Map<UUID, Long> taxonomyModuleCounts = taxonomyService.getTaxonomyModuleCounts(projectId, matchingModules);
		
		final List<TaxonomyAssignmentsGetResponse.TaxonomyGetAssignment> taxonomies = new LinkedList<>(); 
		taxonomyService.find(q -> q.ofProject(projectId)).forEach(taxonomy -> {
			final Long cnt = taxonomyModuleCounts.get(taxonomy.getUid());
			final AssignmentState state;
			if (cnt == 0) {
				state = AssignmentState.NONE;
			} else if (cnt == moduleCount) {
				state = AssignmentState.ALL;
			} else {
				state = AssignmentState.SOME;
			}
			taxonomies.add(new TaxonomyAssignmentsGetResponse.TaxonomyGetAssignment(taxonomy, state));
		});
		
		return new TaxonomyAssignmentsGetResponse(moduleCount, taxonomies);
	}
	
	/**
	 * Modifies Taxonomies assigned to provided {@link Module} IDs
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param taxonomyAssignmentsSetRequest request body for update assignment request
	 * @throws IOException if request body is invalid
	 */
	@PutMapping(value = ASSIGN_TAXONOMY_URL)
	@Operation(summary = "Assign taxonomies to modules", operationId = "updateAssignedTaxonomyByModule")
	@ApiResponse(responseCode = "202", description = "If the assignment modification was successful")
	@ApiResponse(responseCode = "400", description = "If id or state is not set or invalid for taxonomies")
	@ApiResponse(responseCode = "404", description = "If the given project does not exist")
	@Role({EDITOR})
	@Nature({MINING})
	@TryLock(lockCategory = ProjectLockCategory.TAXONOMIES, reasonPhrase = "Applied Lock on Update Taxonomy Assignments")
	public void updateTaxonomyAssignments(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "Module IDs along with Taxonomies ID and states", required = true)
			@RequestBody final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest) throws IOException {
		validate(request);
		taxonomyModelService.updateAssignments(projectId, taxonomyAssignmentsSetRequest);
		response.setStatus(HttpStatus.ACCEPTED.value());
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
	}

	/**
	 * Updates Taxonomies for provided {@link Module} IDs via job
	 * 
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param taxonomyAssignmentsSetRequest request body for update assignment request
	 * @return the submitted job ID
	 * @throws IOException if request body is invalid
	 */
	@PutMapping(value = ASSIGN_TAXONOMY_JOB_URL)
	@Operation(summary = "Starts a job that Assign taxonomies to given modules and returns the job Id. "
			+"See the job status at '/v1/jobs/{jobId}/info'", operationId = "bulkUpdateTaxonomiesToModules")
	@ApiResponse(responseCode = "202", description = "The job has successfully been submitted")
	@ApiResponse(responseCode = "404", description = "If the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] updateTaxonomyAssignmentJob(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "Module IDs along with Taxonomies ID and states", required = true)
			@RequestBody final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest) throws IOException {
		validate(request);
		final var job = new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest);
		return jobManager.submit(job).getJobId().toCharArray();
	}
	
	/**
	 * Endpoint to retrieve aggregated values for Taxonomies
	 * 
	 * @param request Access to the request
	 * @param response The HTTP Response
	 * @param projectId The ID of the Project
	 * @param aggregationRequest the request for aggregated Taxonomy fields
	 * @return {List<AggregationResult<TaxonomyFieldName>>} Returns a list of aggregated value for the requested Taxonomy fields and Project Id
	 */
	@PostMapping(AGGREGATIONS_URL)
	@Operation(summary = "Get aggregated values over a number of taxonomies", operationId = "getAggregatedValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AggregationResult<TaxonomyFieldName>> getAggregatedValues(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true)
			@RequestBody final AggregationRequest<TaxonomyFieldName> aggregationRequest) {
		validate(request);
		validateAggregationRequest(aggregationRequest, TaxonomyFieldName.PROJECT_ID, projectId);
		return taxonomyService.getAggregations(projectId, aggregationRequest);
	}
	
	/**
	 * Endpoint to retrieve Map that contains the Taxonomy type names and sum of SLOC of all Modules that have a Taxonomy of that type assigned.
	 * 
	 * @param request Access to the request
	 * @param response The HTTP Response
	 * @param projectId The ID of the Project
	 * @return {Map<String, Long>} a map of Taxonomy type names and sum of SLOC
	 */
	@GetMapping(SLOC_URL)
	@Operation(summary = "Get map contains the Taxonomy type names and sum of SLOC of all Modules that have a Taxonomy of that type assigned.",
			operationId = "getSlocValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given sloc request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Map<String, Long> getSlocValues(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return taxonomyService.countSourceMetricsCodeLinesByTypeName(projectId);
	}

}
