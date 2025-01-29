/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller.job;

import static innowake.mining.server.service.UserNameService.SYSTEM_USER_ID;
import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.MimeResult;
import innowake.lib.job.api.management.JobLog;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagation;
import innowake.mining.server.Logging;
import innowake.mining.server.config.security.ManualSecurityWithoutProjectAssociation;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.controller.BaseController;
import innowake.mining.server.controller.MiningRestController;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.ResultContainer;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;
import innowake.mining.shared.service.UserRoleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.Explode;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for the job API.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class JobController extends BaseController {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);

	private static final String URI_VAR_JOB_ID = "jobId";
	private static final String URI_VAR_JOB_EXTENSION_ID = "extensionId";

	/**
	 * URL pattern for the job collection.
	 */
	public static final String JOB_COLLECTION_URL = "/v1/jobs";
	public static final String JOB_URL = "/v1/jobs/{jobId}";

	/**
	 * URL pattern for the job info.
	 */
	public static final String JOB_INFO_URL = "/v1/jobs/{jobId}/info";

	/**
	 * URL pattern for the job log.
	 */
	public static final String JOB_LOG_URL = "/v1/jobs/{jobId}/log";

	/**
	 * URL pattern for the job cancel.
	 */
	public static final String JOB_CANCEL_URL = "/v1/jobs/{jobId}/cancel";

	/**
	 * URL pattern for the job result.
	 */
	public static final String JOB_RESULT_URL = "/v1/jobs/{jobId}/result";

	/**
	 * URL pattern for submitting job extensions
	 */
	public static final String JOB_EXTENSION_URL = "/v1/projects/{projectId}/job-extensions/{extensionId}";

	/**
	 * URL pattern for submitting job extensions - V2 version supporting request body
	 */
	public static final String JOB_EXTENSION_V2_URL = "/v2/projects/{projectId}/job-extensions/{extensionId}";

	/**
	 * URL pattern for taxonomy propagation
	 */
	public static final String TAXONOMY_PROPAGATION_URL = "/v1/projects/{projectId}/job-extensions/taxonomyPropagation";

	/**
	 * URL pattern for the job log streamed
	 */
	public static final String JOB_LOG_URL_V2 = "/v2/jobs/{jobId}/log";

	@Autowired
	private MiningJobService miningJobService;

	@Autowired
	private UserRoleService userRoleService;

	@Autowired
	private TaxonomyPropagation taxonomyPropagation;

	/**
	 * Returns a page of {@link JobInformation} for all jobs that match the provided {@code query} and paginated and sorted based on condition provided,
	 * if the user is authorized to access the jobs.
	 * 
	 * @param request access to the request
	 * @param response access to the response
	 * @param query Map of type {@code Map<JobInfoFieldName, Map<String, Object>} containing the filter criteria. The names of the filters must be present in
	 * {@link JobInfoFieldName}. The used operators must be present in {@link FilterOperators}. Time values have to be in ISO-8601 format.
	 * @param page the page number to be fetched
	 * @param size  the size of each page
	 * @param sortBy the sort conditions as list of sort strings. The used sort field names must be present in {@link JobInfoFieldName}
	 * @return a page of {@link JobInformation}
	 */
	@GetMapping(value = JOB_COLLECTION_URL)
	@Operation(summary = "Requests the information of all jobs if the user is authorized to access the jobs", operationId = "getJobInformations")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given sort by column does not exist")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public Paged<JobInformation> getJobInformations(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "The filter object. Time values have to be in ISO-8601 format.", required = false,
			examples = { @ExampleObject("{ \"ID\": { \"in\": [<jobId1>,<jobId2>]}, \"SUBMIT_TIME\": { \"lte\": Instant time as string} }") })
			@RequestParam final Optional<String> query,
			@Parameter(description = "the page number to be fetched", required = false, example = "0")
			@RequestParam(value = "page", required = false)
			@Nullable final Integer page,
			@Parameter(description = "the size of each page", required = false, example = "10",  schema = @Schema(type="integer", format = "int32", 
			minimum = "1", maximum = "1073741823"))
			@Nullable final Integer size,
			@Parameter(description = "the sort conditions as array of sortString", required = false, example = "[ { \"NAME\": \"ASCENDING\" }, { \"SUBMIT_TIME\": \"DESCENDING\" } ] ]")
			@RequestParam(value = "sortBy", required = false, defaultValue = "[ { \"SUBMIT_TIME\": \"DESCENDING\" } ]")
			@Nullable final String sortBy) {

		validate(request);
		response.setStatus(HttpStatus.OK.value());

		final var pageable = Pagination.at(page == null ? 0 : page.intValue(), size == null ? Integer.MAX_VALUE : size.intValue());
		return miningJobService.getAll(pageable, q -> {
			if ( ! userRoleService.isAdmin()) {
				q.withCreatedByUserId(authentication.getUserId());
			}

			if (query.isPresent() && ! query.get().isBlank()) {
				try {
					final Map<JobInfoFieldName, Map<String, Object>> filterObject = 
							PojoMapper.jsonReaderFor(new TypeReference<Map<JobInfoFieldName, Map<String, Object>>>() {}).readValue(query.get());
					q.withFilterObject(filterObject);
				} catch (final JsonProcessingException e) {
					LOG.error("Parameter 'query' is not a valid json filter object", e);
					throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Parameter 'query' is not a valid JSON filter object: " + e.getMessage());
				}
			}
			if (sortBy != null && ! sortBy.isBlank()) {
				try {
					final List<Map<JobInfoFieldName, SortDirection>> sortObject = 
							PojoMapper.jsonReaderFor(new TypeReference<List<Map<JobInfoFieldName, SortDirection>>>() {}).readValue(sortBy);
					q.withSortObject(sortObject);
				} catch (final IOException e) {
					LOG.error("Parameter 'sortBy' is not a valid json sort object", e);
					throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Parameter 'sortBy' is not a valid JSON sort object: " + e.getMessage());
				}
			} else {
				q.sortBy(JobInfoFieldName.SCHEDULED_START_TIME, SortDirection.DESCENDING);
			}
		});
	}

	@DeleteMapping(value = JOB_URL)
	@Operation(summary = "Deletes the job based on id", operationId = "deleteJob")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public int deleteJobs(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the job id", required = true, example = "123e4567-e89b-42d3-a456-556642440000") @PathVariable final UUID jobId) {
		validate(request, URI_VAR_JOB_ID);
		response.setStatus(HttpStatus.OK.value());

		verifyAccess(jobId, false);

		return miningJobService.deleteByJobId(jobId);
	}

	@DeleteMapping(value = JOB_COLLECTION_URL)
	@Operation(summary = "Delete all the jobs submitted by the user. In case of an admin user, it deletes all the jobs", operationId = "deleteJob")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public int deleteAllJobs(final HttpServletRequest request, final HttpServletResponse response) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		if (userRoleService.isAdmin()) {
			return miningJobService.deleteAll();
		}
		return miningJobService.deleteByUserName();
	}

	/**
	 * Returns the {@link JobInformation} that matches the provided {@code jobId} if the user is authorized to access the job.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param jobId the UUID of the job
	 * @return the {@link JobInformation} or {@code null}
	 */
	@GetMapping(value = JOB_INFO_URL)
	@Operation(summary = "Requests the info of the job", operationId = "getJobInformation")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@Nullable
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public JobInformation getJobInformation(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the job id", required = true, example = "123e4567-e89b-42d3-a456-556642440000") @PathVariable final UUID jobId) {
		validate(request, URI_VAR_JOB_ID);
		response.setStatus(HttpStatus.OK.value());

		return miningJobService.mapJobInfo(verifyAccess(jobId, true));
	}

	/**
	 * Returns the String representation of {@link JobLog} that matches the provided {@code jobId} if the user is authorized to access the job.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param jobId the UUID of the job
	 * @return the {@link JobLog} string representation or {@code null}
	 */
	@GetMapping(value = JOB_LOG_URL)
	@Operation(summary = "Requests the job log", operationId = "getJobLog")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@Nullable
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public Map<String, String> getJobLog(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the job id", required = true, example = "123e4567-e89b-42d3-a456-556642440000") @PathVariable final UUID jobId) {
		validate(request, URI_VAR_JOB_ID);

		try {
			verifyAccess(jobId, true);
			return miningJobService.getJobLog(jobId.toString());
		} catch (final IOException e) {
			throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	/**
	 * Requests to cancel the job with the provided {@code jobId} if the user is authorized to access the job.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param jobId the UUID of the job
	 */
	@PutMapping(value = JOB_CANCEL_URL)
	@Operation(summary = "Requests the job to cancel", operationId = "cancelJob")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@ApiResponse(responseCode = "400", description = "if the job cannot be cancelled")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public void cancel(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the job id", required = true, example = "123e4567-e89b-42d3-a456-556642440000") @PathVariable final UUID jobId) {
		validate(request, URI_VAR_JOB_ID);
		response.setStatus(HttpStatus.OK.value());

		final innowake.lib.job.api.management.JobInformation jobInfo = verifyAccess(jobId, false);
		miningJobService.cancelJob(jobId, jobInfo.getStatus());
	}

	/**
	 * Returns the actual result of the job with the provided {@code jobId} if available and the user is authorized to access the job . 
	 * If the result is of type {@link MimeResult}, then
	 * the response will use the information and data provided by the {@link MimeResult} to set the response header and data. In all other
	 * cases the result is serialized as JSON wrapped in {@link ResultContainer}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param jobId the UUID of the job
	 */
	@GetMapping(value = JOB_RESULT_URL)
	@Operation(summary = "Gets the result of a job", operationId = "getJobResult")
	@ApiResponse(responseCode = "204", description = "if the job has no result")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public void getResult(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the job id", required = true, example = "123e4567-e89b-42d3-a456-556642440000") @PathVariable final UUID jobId) {
		validate(request, URI_VAR_JOB_ID);
		verifyAccess(jobId, true);

		response.setStatus(HttpStatus.OK.value());

		final Optional<JobResult> jobResultOpt = miningJobService.getJobResult(jobId);
		if (jobResultOpt.isPresent()) {
			try {
				final JobResult jobResult = jobResultOpt.get();
				jobResult.getFileName().ifPresent(name -> {
					response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment;filename=" + name);
					response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
				});

				response.setContentType(jobResult.getContentType());
				IOUtils.copy(jobResult.getContent(), response.getOutputStream());
				response.flushBuffer();
			} catch (final IOException e) {
				throw new IllegalStateException("Unable to serialize result object.", e);
			}
		} else {
			response.setStatus(HttpStatus.NO_CONTENT.value());
		}
	}
	
	/**
	 * Returns the ZIP byte[] representation of {@link JobLog} that matches the provided {@code jobId} if the user is authorized to access the job.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param jobId the UUID of the job
	 * @return Returns a ResponseEntity containing the byte array with the logs
	 * @throws IOException Gets thrown when there are errors with the I/O on server side
	 */
	@GetMapping(value = JOB_LOG_URL_V2)
	@Operation(summary = "Requests the job log streamed", operationId = "getJobLogStreamed")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given job does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the job")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({EDITOR})
	@ManualSecurityWithoutProjectAssociation
	public ResponseEntity<byte[]> getJobLogZipped(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the job id", required = true, example = "123e4567-e89b-42d3-a456-556642440000") @PathVariable final UUID jobId) throws IOException {

		validate(request, URI_VAR_JOB_ID);
		verifyAccess(jobId, true);

		final Map<String, File> jobLogs = miningJobService.getJobLogFile(jobId.toString());

		final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		final BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(byteArrayOutputStream);
		final ZipOutputStream zipOutputStream = new ZipOutputStream(bufferedOutputStream);

		for(final Entry<String, File> e : jobLogs.entrySet()) {
			zipOutputStream.putNextEntry(new ZipEntry(e.getKey()));
			try (final InputStream is = new FileInputStream(e.getValue())) {
				IOUtils.copy(is, zipOutputStream);
			}
			
			zipOutputStream.closeEntry();
		}

		zipOutputStream.finish();
		zipOutputStream.flush();
		IOUtils.closeQuietly(zipOutputStream);
		IOUtils.closeQuietly(bufferedOutputStream);
		IOUtils.closeQuietly(byteArrayOutputStream);

		return ResponseEntity.ok()
				.contentType(MediaType.APPLICATION_OCTET_STREAM)
				.body(byteArrayOutputStream.toByteArray());
	}


	/**
	 * Submits a job contributed by a job extension.
	 *
	 * @param request access to the HTTP request
	 * @param projectId id of the project for which the job is submitted
	 * @param extensionId the id of the extension that contributes the job
	 * @param parameters map of parameters for the job
	 * @return the id of the submitted job
	 */
	@PostMapping(value = JOB_EXTENSION_URL)
	@Operation(summary = "Submits a job contributed by a job extension", operationId = "submitJobExtension")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given extension or project does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the project")
	@Nature({MINING, DISCOVERY})
	@Role({VIEWER})
	public char[] submitJobExtension(final HttpServletRequest request,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "The job extension id", required = true) @PathVariable final String extensionId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The parameters of the job", required = true) 
			@RequestBody final Map<String, List<String>> parameters) {
		validate(request, URI_VAR_JOB_EXTENSION_ID);

		return miningJobService.submitJob(projectId, extensionId, parameters);
	}

	/**
	 * Submits a job contributed by a job extension. In this "V2" variant the parameters are passed via the query string,
	 * allowing additional (binary) input data to be passed to the job via the request body.
	 *
	 * @param request access to the HTTP request
	 * @param projectId id of the project for which the job is submitted
	 * @param extensionId the id of the extension that contributes the job
	 * @param parameters map of parameters for the job
	 * @param file input data for the job passed in the request body
	 * @return the id of the submitted job
	 * @throws IOException when reading the uploaded data fails
	 */
	@PostMapping(value = JOB_EXTENSION_V2_URL, consumes = { MediaType.MULTIPART_FORM_DATA_VALUE, MediaType.ALL_VALUE})
	@Operation(summary = "Submits a job contributed by a job extension", operationId = "submitJobExtensionV2")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given extension or project does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the project")
	@Nature({MINING, DISCOVERY})
	@Role({VIEWER})
	public char[] submitJobExtensionV2(final HttpServletRequest request,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the job extension id", required = true)
			@PathVariable final String extensionId,
			@Parameter(description = "parameters for the job", required = false, explode = Explode.TRUE, schema = @Schema(type = "object"))
			@RequestParam final MultiValueMap<String, String> parameters,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "input data for the job", required = false)
			@RequestParam(name = "file", required = false)
			@Nullable
			final MultipartFile file) throws IOException {

		validate(request, URI_VAR_JOB_EXTENSION_ID);

		return miningJobService.submitJobV2(projectId, extensionId, parameters, file);
	}
	
	/**
	 * Submits a job for Taxonomy Propagation.
	 *
	 * @param projectId id of the project for which the taxonomy propagation job is submitted
	 * @param propagationData the propagationData
	 * @return the id of the submitted job
	 */
	@PostMapping(value = TAXONOMY_PROPAGATION_URL)
	@Operation(summary = "Submits a job for Taxonomy Propagation", operationId = "submitTaxonomyPropagation")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given or project does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the project")
	@Nature({MINING, DISCOVERY})
	@Role({VIEWER})
	public char[] submitTaxonomyPropagation(@Parameter(description = "the ID of the project", required = true, example = "0")
		@PathVariable
		final EntityId projectId,
		@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "the propagationData", required = true)
		@RequestBody final List<PropagationData> propagationData) {
			taxonomyPropagation.setPropagationData(propagationData);
			return miningJobService.submitJob(projectId, "taxonomyPropagation", new HashMap<>());
	}

	private void isUserAuthorizedToAccessJob(final innowake.lib.job.api.management.JobInformation jobInfo, final boolean isReadOnlyAccess) {
		if ( ! (jobInfo.getUserName().equals(authentication.getUserId()) || (isReadOnlyAccess && jobInfo.getUserName().equals(SYSTEM_USER_ID)))) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN);
		}
	}

	private innowake.lib.job.api.management.JobInformation verifyAccess(final UUID jobId, final boolean isReadOnlyAccess) {
		final innowake.lib.job.api.management.JobInformation jobInfo = miningJobService.getJobInfoForId(jobId);
		if ( ! userRoleService.isAdmin()) {
			isUserAuthorizedToAccessJob(jobInfo, isReadOnlyAccess);
		}
		return jobInfo;
	}

}
