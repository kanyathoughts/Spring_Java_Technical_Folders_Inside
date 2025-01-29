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
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.server.event.AnnotationCreatedEvent;
import innowake.mining.server.event.AnnotationDeletedEvent;
import innowake.mining.server.event.AnnotationUpdatedEvent;
import innowake.mining.server.job.AnnotationCsvImportJob;
import innowake.mining.server.util.CSVReaderHeaderAware;
import innowake.mining.shared.model.FeatureId;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.Logging;
import innowake.mining.server.genai.requestresponse.GenAiAnnotationRequest;
import org.apache.tinkerpop.shaded.minlog.Log;
import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonProcessingException;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.job.LinkAnnotationToDataDictionaryJob;
import innowake.mining.server.job.BulkAnnotationsUpdateJob;
import innowake.mining.server.job.deletion.MiningBulkDeletionJob;
import innowake.mining.server.opensearch.OpenSearchService;
import innowake.mining.server.service.GenerativeAnnotationGroupDescriptionService;
import innowake.mining.server.service.GenerativeAnnotationTranslationService;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.AnnotationFieldName;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for Annotation requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class AnnotationController extends BaseController {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);

	/**
	 * URL pattern for the annotation collection by project ID.
	 */
	public static final String ANNOTATION_COLLECTION_BY_PROJECT_URL = "/v1/projects/{projectId}/annotations";

	/**
	 * URL pattern for the annotation collection by module ID.
	 */
	public static final String ANNOTATION_COLLECTION_BY_MODULE_URL = "/v1/projects/{projectId}/modules/{moduleId}/annotations";

	/**
	 * URL pattern for a single annotation by ID.
	 */
	public static final String ANNOTATION_BY_ID_URL = "/v1/projects/{projectId}/annotations/{annotationId}";
	
	/**
	 * URL pattern for updating list of annotations.
	 */
	public static final String UPDATE_CATEGORY_AND_METADATA_OF_ANNOTATIONS = "/v1/projects/{projectId}/annotations";

	/**
	 * URL pattern for a finding linked Business Variables of an annotation by annotation ID.
	 */
	public static final String ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL = "/v1/projects/{projectId}/annotations/{annotationId}/linked-business-variables";

	/**
	 * URL pattern for searching annotations.
	 */
	public static final String ANNOTATION_SEARCH_URL = "/v1/projects/{projectId}/annotations/search";

	/**
	 * URL pattern for importing annotations.
	 */
	public static final String ANNOTATION_IMPORT_URL = "/v1/projects/{projectId}/annotations/import";

	/**
	 * Parameter name for searching annotations by name.
	 */
	public static final String SEARCH_PARAM_NAME = "name";

	/**
	 * URL pattern for a single module by annotation ID.
	 */
	public static final String MODULE_BY_ANNOTATION_ID_URL = "/v1/projects/{projectId}/annotations/{annotationId}/modules";

	/**
	 * URL pattern for the annotation type collection by project ID.
	 */
	public static final String ANNOTATION_TYPE_COLLECTION_URL = "/v1/projects/{projectId}/annotation-types";
	
	/**
	 * URL pattern for finding annotations which has given custom property assigned.
	 */
	public static final String ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL = "/v1/projects/{projectId}/annotation-customProperty";
	
	/**
	 * URL pattern for the AI annotation source code translations by annotation ID.
	 */
	public static final String AI_ANNOTATION_TRANSLATION_URL = "/v1/projects/{projectId}/modules/{moduleId}/annotations/translate";
	
	/**
	 * URL pattern for the annotation aggregations by project ID.
	 */
	private static final String AGGREGATIONS_URL = "/v1/projects/{projectId}/annotations/aggregations";
	
	/**
	 *URL pattern for retrieving the annotations of a single module Based on Offset.
	 */
	public static final String ANNOTATION_COLLECTIONS_BASED_ON_OFFSET = "/v1/projects/{projectId}/modules/{moduleId}/annotations/byOffset";
	
	/**
	 * URL pattern for the AI annotation group description generation.
	 */
	public static final String AI_ANNOTATION_GROUP_DESCRIPTION = "/v1/projects/{projectId}/annotation-groups/translate";
	
	/**
	 * URL pattern for the bulk deletion of annotations or data dictionaries.
	 */
	public static final String ANNOTATION_DATADICTIONARY_BULK_DELETE_URL = "/v1/projects/{projectId}/bulk-delete/{entityType}";

	@Autowired
	private ApplicationEventPublisher eventPublisher;

	@Autowired
	private GenerativeAnnotationTranslationService generativeAnnotationTranslationService;
	
	@Autowired
	private JobManager jobManager;

	@Autowired
	private OpenSearchService openSearchService;
	
	@Autowired
	private FunctionalBlockService functionalBlockService;
	
	@Autowired
	private GenerativeAnnotationGroupDescriptionService  generativeAnnotationGroupDescriptionService;

	@Autowired
	private FF4j ff4j;

	/**
	 * Lists all available Annotations for a project with optional filters.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to find
	 * @param states JSON string representation of the states for the returned annotations, if {@code null} or empty, annotations for all states are returned.
	 * @param categoryIds JSON string representation of the ids of the categories for the returned annotations, 
	 * if {@code null} annotations for all categories are returned.
	 * @param modulename only annotations with corresponding modules that match this wildcard pattern of a module name are returned, 
	 * if {@code null} annotations for all modules are returned.
	 * @param modulepath only annotations with corresponding modules that equals the path are returned, if {@code null} no path restrictions apply.
	 * @return a list of Annotations
	 * @throws JsonProcessingException in case of an invalid parameter
	 */
	@GetMapping(ANNOTATION_COLLECTION_BY_PROJECT_URL)
	@Operation(summary = "Get all available annotations", operationId = "findAllAnnotations")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AnnotationPojo> findAll(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "JSON representation of the states for the returned annotations, omitting returns annotations for all states, "
					+ "e.g. [CANDIDATE, IN_ANALYSIS, APPROVED]", required = false)
			@RequestParam(required=false) final String states,
			@Parameter(description = "JSON representation of the category IDs for the returned annotations, omitting returns annotations for all categories "
					+ "e.g. [1, 5, 30]", required = false)
			@RequestParam(required=false) final String categoryIds,
			@Parameter(description = "restrict returned annotations to those having modules, where the module name matches this wildcard pattern, "
					+ "omitting returns annotations for all modules, e.g. mod?ule*01", required = false)
			@RequestParam(required=false) final String modulename,
			@Parameter(description = "restrict returned annotations to those having modules, where the module path equals the given path", required = false)
			@RequestParam(required=false) final String modulepath) throws JsonProcessingException {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		final List<WorkingState> statesCollection = states != null ? Arrays.asList(PojoMapper.jsonReaderFor(WorkingState[].class).readValue(states)) : null;
		final List<Long> categoryIdsCollection = categoryIds != null ? Arrays.asList(PojoMapper.jsonReaderFor(Long[].class).readValue(categoryIds)) : null;
		return annotationService.find(q -> {
				q.ofProject(projectId);
				if (statesCollection != null) {
					q.withStates(statesCollection);
				}
				if (categoryIdsCollection != null) {
					q.withCategories(categoryIdsCollection);
				}
				if (modulename != null) {
					q.withModuleName(modulename, true);
				}
				if (modulepath != null) {
					q.withModulePath(modulepath);
				}
		});
	}
	
	/**
	 * Lists all available {@linkplain AnnotationType annotation types}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation types to find
	 * @return a list of {@linkplain AnnotationType annotation types}
	 */
	@GetMapping(ANNOTATION_TYPE_COLLECTION_URL)
	@Operation(summary = "List all distinct annotation types for a specific project", operationId = "findAllDistinctAnnotationTypes")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AnnotationType> findAllDistinctAnnotationTypes(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return annotationService.findDeclaredTypes(projectId);
	}
	
	
	/**
	 * Finds Annotations with assigned default values of given custom property
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to find
	 * @param propertyName the custom property name
	 * @return Map of annotation Id with assigned default values of given custom property
	 */
	@GetMapping(ANNOTATIONS_WITH_CUSTOM_PROPERTY_URL)
	@Operation(summary = "find annotations with given custom property assigned for a specific project", operationId = "findAnnotationsWithCustomProperty")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Map<Long, Object> findAnnotationsWithCustomProperty(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the name of the custom property", required = true)
			@RequestParam(name = "propertyName") final String propertyName) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return annotationService.getCustomProperties(projectId, propertyName);
	}
	
	/**
	 * Finds an Annotation by its ID.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to find
	 * @param annotationId the annotation id of the annotation to find
	 * @return an Annotation
	 */
	@GetMapping(ANNOTATION_BY_ID_URL)
	@Operation(summary = "Find an annotation by id", operationId = "findAnnotationById")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given annotation or project ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public AnnotationPojo findById(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the annotation ID to search for", required = true, example = "0")
			@PathVariable final EntityId annotationId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return resolveUserNames(annotationService.get(q -> q.ofProject(projectId).byId(annotationId)));
	}
	
	/**
	 * Finds linked Data Dictionary Business Variables for an Annotation by Annotation id.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to find linked data dictionary entries 
	 * @param annotationId the annotation id of the annotation to find data dictionary entries
	 * @return List of DataDictionaryEntry Business Variables
	 */
	@GetMapping(ANNOTATION_LINKED_BUSINESS_VARIABLES_BY_ID_URL)
	@Operation(summary = "Find linked Business Variables of an annotation by id", operationId = "findLinkedBusinessVariablesById")
	@ApiResponse(responseCode = "404", description = "if the given annotation or project ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<DataDictionaryPojo> findLinkedBusinessVariablesById(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the annotation ID to search for", required = true, example = "0")
			@PathVariable final EntityId annotationId) {
		validate(request);
		return dataDictionaryService.find(q -> q.ofModuleProject(projectId).ofAnnotation(annotationId));
	}
	
	/**
	 * Finds the Module that references the Annotation by the Annotation id.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the {@link Module} to find 
	 * @param annotationId the id of the Annotation belonging to the {@link Module} to find 
	 * @return a {@link Module}
	 */
	@GetMapping(MODULE_BY_ANNOTATION_ID_URL)
	@Operation(summary = "Find a module by an annotation id", operationId = "findModuleOfAnnotation")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given annotation or project ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public ModulePojo findModule(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the annotation ID", required = true, example = "0")
			@PathVariable final EntityId annotationId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return moduleService.findAnyModule(q -> q.ofProject(projectId).withAnnotation(annotationId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with annotation: " + annotationId + " in project: " + projectId));
	}
	
	/**
	 * Searches Annotation by name.
	 * 
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to find
	 * @param name the name of the annotation to find
	 * @return a list of Annotations
	 */
	@GetMapping(value = ANNOTATION_SEARCH_URL, params = SEARCH_PARAM_NAME)
	@Operation(summary = "Find annotations by name", operationId = "findAnnotationsByName")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AnnotationPojo> findByName(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the name to search for", required = true)
			@RequestParam(name = SEARCH_PARAM_NAME) final String name) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return resolveUserNames(annotationService.find(q -> q.ofProject(projectId).withName("%" + name + "%").sortName(SortDirection.ASCENDING)));
	}

	/**
	 * Creates a new Annotation with a reference to a module with the given {@code moduleId}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to create
	 * @param moduleId the ID of the module the annotation is associated with
	 * @param annotation the Annotation to create 
	 * @return the new Annotation
	 */
	@PostMapping(ANNOTATION_COLLECTION_BY_MODULE_URL)
	@Operation(summary = "Create a new annotation", operationId = "createAnnotation")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given annotation is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Create Annotation")
	public AnnotationPojo create(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the module ID", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The annotation to create", required = true) 
			@RequestBody final AnnotationPojoPrototype annotation) {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());
		annotation.setCreatedByUserId(authentication.getUserId());
		moduleId.matchOrApply(annotation.module.orElseNonNull(EntityId.VOID), annotation::setModule);
		validateProject(projectId, annotation);
		final EntityId createdAnnotationId = annotationService.create(annotation);
		eventPublisher.publishEvent(new AnnotationCreatedEvent(projectId, createdAnnotationId));
		final AnnotationPojo createdAnnotation = annotationService.get(createdAnnotationId);
		if ( ! ff4j.getFeature(FeatureId.DISABLE_ANNOTATION_DATA_DICTIONARY_MANUAL_LINKING.getId()).isEnable()) {
			createdAnnotation.getLocation().ifPresent(location -> 
				jobManager.submit(new LinkAnnotationToDataDictionaryJob(createdAnnotationId, location, projectId, moduleId)));
		}
		return createdAnnotation;
	}
	
	/**
	 * Updates an existing Annotation}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to update
	 * @param annotationId the annotation id of the annotation to update
	 * @param annotation the Annotation to update 
	 * @return the updated Annotation
	 */
	@PutMapping(ANNOTATION_BY_ID_URL)
	@Operation(summary = "Update an annotation", operationId = "updateAnnotation")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given annotation is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or annotation ID does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Update an annotation")
	public AnnotationPojo update(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the annotation ID of the annotation to update", required = true, example = "0")
			@PathVariable final EntityId annotationId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated annotation", required = true) 
			@RequestBody final AnnotationPojoPrototype annotation) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		annotationId.matchOrApply(annotation.identityProvisional(), annotation::withId);
		validateProject(projectId, annotation);
		annotation.createdByUserId.unset();
		annotation.setUpdatedByUserId(authentication.getUserId());
		final EntityId id = annotationService.update(annotation);
		final AnnotationPojo updatedAnnotation = annotationService.get(q -> q.byId(id));
		eventPublisher.publishEvent(new AnnotationUpdatedEvent(projectId, id));
		functionalBlockService.updateFunctionBlockDescriptionAndNameOnAnnotationUpdate(id.getNid(), updatedAnnotation);
		return updatedAnnotation;
	}
	
	/**
	 * Delete an existing Annotation.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to delete
	 * @param annotationId the annotation id of the annotation to delete
	 */
	@DeleteMapping(ANNOTATION_BY_ID_URL)
	@Operation(summary = "Delete an annotation", operationId = "deleteAnnotation")
	@ApiResponse(responseCode = "204", description = "if the module was successfully deleted")
	@ApiResponse(responseCode = "404", description = "if the project or annotation does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Delete Annotation")
	public void delete(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the annotation to be deleted", required = true, example = "0")
			@PathVariable final EntityId annotationId) {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		final var deleted = annotationService.delete(projectId, annotationId);
		functionalBlockService.deleteGeneratedFromAnnotation(deleted.getKey().getNid());
		eventPublisher.publishEvent(new AnnotationDeletedEvent(projectId, deleted.getValue(), deleted.getKey()));
	}

	/**
	 * Returns aggregated values for annotation based on {@link AggregationRequest}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to fetch aggregation for
	 * @param aggregationRequest object to hold request body
	 * @return a {@link List} containing {@link AggregationResult} object representing the aggregated result
	 */
	@PostMapping(AGGREGATIONS_URL)
	@Operation(summary = "Get aggregated values over a number of annotations", operationId = "getAggregatedValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AggregationResult<AnnotationFieldName>> getAggregatedValues(
			final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true) 
			@RequestBody final AggregationRequest<AnnotationFieldName> aggregationRequest) {
		validate(request);

		if (openSearchService.isAvailable()) {
			validateAggregationRequest(aggregationRequest);
			try {
				return openSearchService.getAggregatedValues("annotationtable", projectService.getNid(projectId), aggregationRequest);
			} catch (final Exception e) {
				Log.warn("OpenSearch not available, defaulting to standard method");
			}
		}

		validateAggregationRequest(aggregationRequest, AnnotationFieldName.PROJECT_ID, projectId);
		return annotationService.getAggregations(projectId, aggregationRequest);
	}
	
	/**
	 * Provides Annotation with AI translated description for the attached Source Code.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project ID
	 * @param moduleId the ID of the Module
	 * @param genAiAnnotationRequest the annotation information to translate
	 * @return the translated description
	 */
	@PostMapping(AI_ANNOTATION_TRANSLATION_URL)
	@Operation(summary = "Get AI translated English meaning of the Annotation Source Code", operationId = "getGptTranslation")
	@ApiResponse(responseCode = "200", description = "if the translation was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or annotation does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] getGptTranslation(
			final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "The ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "The ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The annotation information to translate", required = true)
			@RequestBody final GenAiAnnotationRequest genAiAnnotationRequest) {
		validate(request);
		LOG.debug(() -> "GenAI Options: " + genAiAnnotationRequest.getOptions());
		AnnotationPojoPrototype annotation = genAiAnnotationRequest.getAnnotation();
		annotation.setModule(moduleId);
		return generativeAnnotationTranslationService.translateUsingGenAI(annotation, projectId, moduleId, genAiAnnotationRequest.getOptions()).toCharArray();
	}

	/**
	 * Returns List of Annotation for a given module Based on Offset.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId  the ID of the module
	 * @param startOffset the Start Offset of Module
	 * @param endOffset the end OffSet of Module
	 * @return a list of Annotation for given moduleId Based on Offset.
	 */
	@GetMapping(value = ANNOTATION_COLLECTIONS_BASED_ON_OFFSET)
	@Operation(summary = "List of Annotation for a given module Based on Offset", operationId = "findAnnotationBasedOnOffset")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AnnotationPojo> findAnnotationBasedOnOffset(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId,
			@Parameter(description = "the Start Offset of Module", required = true, example = "0") @RequestParam final Integer startOffset,
			@Parameter(description = "the End Offset of Module (optional)", required = false)
			@RequestParam(required = false) @Nullable final Integer endOffset) {
		validate(request);
		
		return annotationService.find(q -> {
			q.ofProject(projectId)
			 .ofModule(moduleId);

			if (endOffset != null) {
				q.withOffsetBetween(startOffset, endOffset);
			} else {
				q.withMinOffset(startOffset);
			}
		});
	}
	
	/**
	 * Updates an existing list of {@link AnnotationPojo AnnotationPojo's} categories and metadata.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to update
	 * @param updateType the type of annotation update
	 * @param annotations the list of {@link AnnotationPojo AnnotationPojo's} to update 
	 * @return the updated {@link AnnotationPojo}
	 */
	@PutMapping(UPDATE_CATEGORY_AND_METADATA_OF_ANNOTATIONS)
	@Operation(summary = "Starts a job that updates Annotation categories, metadata and returns the job Id."
			+ "See the job status at '/v1/jobs/{jobId}/info", operationId = "updateCategoryAndMetadataOfAnnotation")
	@ApiResponse(responseCode = "200", description = "the job has successfully been submitted")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] updateCategoryAndMetadataOfAnnotation(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@RequestParam(name = "updateType", required = true, defaultValue = "true") final String updateType,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The annotations to be updated", required = true) 
			@RequestBody final List<AnnotationPojo> annotations) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		final BulkAnnotationsUpdateJob job = new BulkAnnotationsUpdateJob(annotationService, projectId, annotations, updateType);
		return jobManager.submit(job).getJobId().toCharArray();
	}
	
	/**
	 * Generates meta description for a group of Annotations
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project containing the annotations
	 * @param annotationIds the IDs of the annotations
	 * @param generateDescriptions {@code true} if annotation descriptions should be generated automatically. Otherwise {@code false}
	 * @return description of annotation group
	 */
	@PostMapping(value = AI_ANNOTATION_GROUP_DESCRIPTION)
	@Operation(summary = "Generate meta description for a group of Annotations", operationId = "generateGroupDescription")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given annotations don't exist")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] generateAnnotationGroupDescription(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the IDs of the annotations", required = true, example = "[0,1]") @RequestParam final List<EntityId> annotationIds,
			@Parameter(description = "whether descriptions of annotations should be generated automatically", required = false) @RequestParam(defaultValue = "false") final boolean generateDescriptions) {
		validate(request);
		return generativeAnnotationGroupDescriptionService.generateAnnotationGroupDescription(annotationIds, projectId, generateDescriptions).toCharArray();
	}

	/**
	 * Imports {@link AnnotationPojo} from a CSV file.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to import
	 * @param file the {@link AnnotationPojo} to import
	 * @return the job ID of the import job
	 */
	@PostMapping(value = ANNOTATION_IMPORT_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Import annotations from a CSV file", operationId = "importAnnotations")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given annotation is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Import Annotations")
	public char[] importAnnotations(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "the CSV file with annotations to import", required = true)
			@RequestParam(name = "file", required = true)
			final MultipartFile file) {

		validate(request);
		response.setStatus(HttpStatus.CREATED.value());
		try (final InputStream inputStream = file.getInputStream()) {
			final List<Map<String, String>> importLines = CSVReaderHeaderAware.linesFromCsv(inputStream);
			return jobManager.submit(new AnnotationCsvImportJob(projectId, importLines, authentication.getUserId())).getJobId().toCharArray();
		}
		catch (final IOException e) {
			final String message = String.format("Error while parsing file: %s", e.getMessage());
			throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, message);
		}
	}
	
	/**
	 * Submits a job for bulk deletion of annotations or data dictionaries.
	 * 
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId id of the project for which the taxonomy propagation job is submitted
	 * @param idsToBeDeleted the ids to be deleted
	 * @param entityType the entity type either "Annotation" or "Data Dictionary"
	 * @return the id of the submitted job
	 */
	@DeleteMapping(value = ANNOTATION_DATADICTIONARY_BULK_DELETE_URL)
	@Operation(summary = "Submits a job for bulk deletion of annotations or data dictionaries", operationId = "bulkDeletion")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given or project does not exist")
	@ApiResponse(responseCode = "403", description = "if the user is not authorized to access the project")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] submitMiningBulkDeletion(final HttpServletRequest request, final HttpServletResponse response, 
		@Parameter(description = "the ID of the project", required = true, example = "0")
		@PathVariable final EntityId projectId,
		@Parameter(description = "The entity type to be deleted", required = true) @PathVariable final String entityType,
		@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "the ids to be deleted", required = true)
		@RequestBody final List<EntityId> idsToBeDeleted) {
			validate(request, "entityType");
			final String jobName = "Bulk " + entityType + " Delete";
			final MiningBulkDeletionJob job = new MiningBulkDeletionJob(projectId, idsToBeDeleted, jobName);
			return jobManager.submit(job).getJobId().toCharArray();
	}
}
