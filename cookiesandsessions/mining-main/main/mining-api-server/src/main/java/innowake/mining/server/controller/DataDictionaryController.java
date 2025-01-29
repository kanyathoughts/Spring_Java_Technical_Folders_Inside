/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.DataDictionariesModifiedEvent;
import innowake.mining.server.job.LinkDataDictionaryToAnnotationJob;
import innowake.mining.server.service.AstNodeService;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.model.DataDictionaryFieldName;
import innowake.mining.shared.model.DataFieldFormat;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for {@link DataDictionaryPojo} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class DataDictionaryController extends BaseController {
	
	private static final String URI_VAR_DATA_DICTIONARY_ID = "ddeId";

    /**
     * URL pattern for the data dictionary collection.
     */
    public static final String DATA_DICTIONARY_COLLECTION_URL = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary";

    /**
     * URL pattern for the data dictionary count.
     */
    public static final String DATA_DICTIONARY_COUNT_URL = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/count";

    /**
     * URL pattern for the given record id data dictionary.
     */
    public static final String DATA_DICTIONARY_ID_URL = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{ddeId}";

    /**
	 * URL pattern for finding linked Business Rules for a Data Dictionary for a given record ID.
	 */
	public static final String DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL = 
													"/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{ddeId}/linked-annotations";

    /**
     * URL pattern for the data dictionary update.
     */
    public static final String DATA_DICTIONARY_BY_ID_URL = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{ddeId}";

    /**
     * URL pattern for the data dictionary search.
     */
    public static final String DATA_DICTIONARY_SEARCH_URL = "/v1/projects/{projectId}/data-dictionary/search";

    /**
     * URL pattern for the data dictionary other-scopes.
     */
    public static final String DATA_DICTIONARY_OTHER_SCOPES_URL = "/v1/projects/{projectId}/data-dictionary/other-scopes";

    /**
     * URL pattern for the data dictionary validation using offset.
     */
    public static final String DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/offset/{offset}";

    /**
     * Parameter name for searching by description.
     */
    public static final String SEARCH_PARAM_DESCRIPTION = "description";

    /**
     * Parameter name for searching by dataElementName.
     */
    public static final String SEARCH_PARAM_DATA_ELEMENT_NAME = "dataElementName";

    /**
     * URL pattern for the Data dictionary aggregations by project ID.
     */
    public static final String AGGREGATIONS_URL = "/v1/projects/{projectId}/data-dictionary/aggregations";
    
    /**
	 *URL pattern for retrieving the annotations of a single module Based on Offset.
	 */
	public static final String DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/byOffset";

	/**
	 * URL pattern for retrieving data dictionary ids using data flow ids
	 */
	public static final String DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS = "/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/byDataFlowIds";

    @Autowired
    private AstNodeService astNodeService;

	@Autowired
	private FF4j ff4j;
    
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	private JobManager jobManager;

	@Override
	protected void validateUriVars(final UriVarsValidation validation) {
		super.validateUriVars(validation);
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_MODULE_ID, URI_VAR_DATA_DICTIONARY_ID)) {
			validation.pass(URI_VAR_DATA_DICTIONARY_ID);
		}
	}

	/**
     * Returns all available data dictionary entries for given module.
     *
     * @param request   access to the request
     * @param response  the HTTP response
     * @param projectId the ID of the project
     * @param moduleId  the ID of the module
     * @param includeCopyBooks should copy books be included
     * @return a list of data dictionary entries
     */
    @GetMapping(value = DATA_DICTIONARY_COLLECTION_URL)
    @Operation(summary = "List all data dictionary entries from a given module", operationId = "findAllDataDictionaryEntries")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
    @Nature({MINING})
    @Role({VIEWER})
    public List<DataDictionaryPojo> findAll(final HttpServletRequest request, final HttpServletResponse response,
                                             @Parameter(description = "the ID of the project", required = true, example = "0")
                                             @PathVariable final EntityId projectId,
                                             @Parameter(description = "the ID of the module", required = true, example = "0")
                                             @PathVariable final EntityId moduleId,
                                             @RequestParam(name = "includeCopyBooks", required = false, defaultValue = "false") final boolean includeCopyBooks) {
        validate(request);
		if (includeCopyBooks) {
			final var includes = moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT);
			if (moduleId.hasUid()) {
				includes.add(moduleId.getUid());
			} else {
				includes.add(moduleService.getModule(moduleId).identity().getUid());
			}
			return dataDictionaryService.find(q -> q.ofModuleUuids(includes));
		} else {
			return dataDictionaryService.find(q -> q.ofModule(moduleId));
		}
    }

    /**
     * Returns count available data dictionary entries for given module.
     *
     * @param request   access to the request
     * @param response  the HTTP response
     * @param projectId the ID of the project
     * @param moduleId  the ID of the module
     * @param includeCopyBooks should copy books be included
     * @return a list of data dictionary entries
     */
    @GetMapping(value = DATA_DICTIONARY_COUNT_URL)
    @Operation(summary = "Count all data dictionary entries from a given module", operationId = "countAllDataDictionaryEntries")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
    @Nature({MINING})
    @Role({VIEWER})
    public Long countAll(final HttpServletRequest request, final HttpServletResponse response,
                         @Parameter(description = "the ID of the project", required = true, example = "0")
                         @PathVariable final EntityId projectId,
                         @Parameter(description = "the ID of the module", required = true, example = "0")
                         @PathVariable final EntityId moduleId,
                         @RequestParam(name = "includeCopyBooks", required = false, defaultValue = "false")
    						final boolean includeCopyBooks) {
        validate(request);
		if (includeCopyBooks) {
			final var includes = moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.OUT);
			if (moduleId.hasUid()) {
				includes.add(moduleId.getUid());
			} else {
				includes.add(moduleService.getModule(moduleId).identity().getUid());
			}
			return dataDictionaryService.count(q -> q.ofModuleUuids(includes));
		} else {
			return dataDictionaryService.count(q -> q.ofModule(moduleId));
		}
    }

    /**
     * Returns the data dictionary entry for given record id.
     *
     * @param request   access to the request
     * @param response  the HTTP response
     * @param projectId the ID of the project
     * @param moduleId  the ID of the module the data dictionary entry belongs to
     * @param ddeId  the record id of the data dictionary entry
     * @return a list of data dictionary entries
     */
    @GetMapping(value = DATA_DICTIONARY_ID_URL)
    @Operation(summary = "Lists a data dictionary entry from a given record ID", operationId = "findDataDictionaryEntryByRecordId")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "404", description = "if the given record id, project id or module id does not exist")
    @Nature({MINING})
    @Role({VIEWER})
    public DataDictionaryPojo findByRecordId(final HttpServletRequest request, final HttpServletResponse response,
                                              @Parameter(description = "the ID of the project", required = true, example = "0")
                                              @PathVariable final EntityId projectId,
                                              @Parameter(description = "the ID of the module", required = true, example = "0")
                                              @PathVariable final EntityId moduleId,
                                              @Parameter(description = "the ID of the data dictionary entry", required = true)
                                              @PathVariable final EntityId ddeId) {
        validate(request, URI_VAR_DATA_DICTIONARY_ID);
        response.setStatus(HttpStatus.OK.value());
        return dataDictionaryService.get(q -> q.byId(ddeId));
    }
    
	/**
	 * Finds linked {@linkplain AnnotationPojo Rules} linked to a data dictionary.
	 *
	 * @param request access to the request
     * @param response  the HTTP response
     * @param projectId the ID of the project
     * @param moduleId  the ID of the module the data dictionary entry belongs to
     * @param ddeId the ID of the data dictionary
	 * @return List linked {@linkplain DataDictionaryPojo Business Variables} 
	 */
	@GetMapping(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL)
	@Operation(summary = "Find linked Business Variables for a Business Rule", operationId = "findLinkedBusinessVariables")
	@ApiResponse(responseCode = "404", description = "if the given annotation or project ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AnnotationPojo> findLinkedBusinessRulesById(final HttpServletRequest request, final HttpServletResponse response,
            @Parameter(description = "the ID of the project", required = true, example = "0")
            @PathVariable final EntityId projectId,
            @Parameter(description = "the ID of the module", required = true, example = "0")
            @PathVariable final EntityId moduleId,
            @Parameter(description = "the ID of the data dictionary", required = true)
            @PathVariable final EntityId ddeId) {
		validate(request);
		return annotationService.find(q -> q.ofProject(projectId).ofDataDictionaryEntry(ddeId));
	}

    /**
     * Creates a data dictionary entry given a module ID.
     *
     * @param request             the HTTP request
     * @param response            the HTTP response
     * @param projectId           the ID of the project
     * @param moduleId            the ID of the module
     * @param dataDictionaryEntry the data dictionary entry to create
     * @return the newly created data dictionary entry with its database ID
     */
    @PostMapping(value = DATA_DICTIONARY_COLLECTION_URL)
    @Operation(summary = "Create a new data dictionary entry", operationId = "createDataDictionaryEntry")
    @ApiResponse(responseCode = "201", description = "on success")
    @ApiResponse(responseCode = "400", description = "if the given data dictionary entry is not valid")
    @ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
    @Nature({MINING})
    @Role({EDITOR})
    @TryLock(lockCategory = ProjectLockCategory.DATA_DICTIONARIES, reasonPhrase = "Applied Lock on Create new data dictionary entry")
    public DataDictionaryPojo create(final HttpServletRequest request, final HttpServletResponse response,
                                      @Parameter(description = "the ID of the project", required = true, example = "0")
                                      @PathVariable final EntityId projectId,
                                      @Parameter(description = "the ID of the module", required = true, example = "0")
                                      @PathVariable final EntityId moduleId,
                                      @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The data dictionary entry to create", 
                                      required = true) 
    								  @RequestBody final DataDictionaryPojoPrototype dataDictionaryEntry) {
        validate(request);
        response.setStatus(HttpStatus.CREATED.value());
        dataDictionaryEntry.setCreatedByUserId(authentication.getUserId());
        eventPublisher.publishEvent(new DataDictionariesModifiedEvent(projectId));
        final DataDictionaryPojo dde = dataDictionaryService.create(dataDictionaryEntry);
        if ( ! ff4j.getFeature(FeatureId.DISABLE_ANNOTATION_DATA_DICTIONARY_MANUAL_LINKING.getId()).isEnable()) {
            jobManager.submit(new LinkDataDictionaryToAnnotationJob(dde.identity(), projectId, moduleId));
        }
        return dde;
    }

    /**
     * Updates the given data dictionary entry.
     *
     * @param request               access to the request
     * @param response              the HTTP response
     * @param projectId             the ID of the project to search in
     * @param moduleId              the ID of the module to search in
     * @param ddeId the ID of the data dictionary entry to update
     * @param dataDictionaryEntry   the data dictionary with the updated values
     * @return the updated data dictionary entry
     */
    @PutMapping(value = DATA_DICTIONARY_BY_ID_URL)
    @Operation(summary = "Update a data dictionary entry", operationId = "updateDataDictionaryEntry")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "400", description = "if the given data dictionary entry is not valid")
    @ApiResponse(responseCode = "404", description = "if the given project, module or data dictionary entry does not exist")
    @Nature({MINING})
    @Role({EDITOR})
    @TryLock(lockCategory = ProjectLockCategory.DATA_DICTIONARIES, reasonPhrase = "Applied Lock on Update data dictionary entry")
    public DataDictionaryPojo update(final HttpServletRequest request, final HttpServletResponse response,
                                      @Parameter(description = "the ID of the project to search", required = true, example = "0")
                                      @PathVariable final EntityId projectId,
                                      @Parameter(description = "the ID of the module", required = true, example = "0")
                                      @PathVariable final EntityId moduleId,
                                      @Parameter(description = "the ID of the data dictionary entry to be updated", required = true, example = "0")
                                      @PathVariable final EntityId ddeId,
                                      @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated data dictionary entry", required = true) 
    								  @RequestBody final DataDictionaryPojoPrototype dataDictionaryEntry) {
        validate(request);
        response.setStatus(HttpStatus.OK.value());
        ddeId.matchOrApply(dataDictionaryEntry.identityProvisional(), dataDictionaryEntry::withId);
        dataDictionaryEntry.createdByUserId.unset();
        dataDictionaryEntry.setUpdatedByUserId(authentication.getUserId());
        
        dataDictionaryService.updateRelatedAnnotationsEnglishTranslation(projectId, ddeId, dataDictionaryEntry, authentication.getUserId());
        
        eventPublisher.publishEvent(new DataDictionariesModifiedEvent(projectId));
        
        return dataDictionaryService.update(dataDictionaryEntry);
    }

	/**
     * Deletes the given data dictionary entry.
     *
     * @param request               access to the request
     * @param response              the HTTP response
     * @param projectId             the ID of the project to search in
     * @param moduleId              the ID of the module to search in
     * @param ddeId the ID of the data dictionary entry to update
     */
    @DeleteMapping(value = DATA_DICTIONARY_BY_ID_URL)
    @Operation(summary = "Delete a data dictionary entry", operationId = "deleteDataDictionaryEntry")
    @ApiResponse(responseCode = "204", description = "always, regardless of the existence of the data dictionary entry")
    @Nature({MINING})
    @Role({EDITOR})
    @TryLock(lockCategory = ProjectLockCategory.DATA_DICTIONARIES, reasonPhrase = "Applied Lock on Delete data dictionary entry")
    public void delete(final HttpServletRequest request,
                       final HttpServletResponse response,
                       @Parameter(description = "the ID of the project to search", required = true, example = "0")
                       @PathVariable final EntityId projectId,
                       @Parameter(description = "the ID of the module", required = true, example = "0")
                       @PathVariable final EntityId moduleId,
                       @Parameter(description = "the ID of the data dictionary entry to be deleted", required = true, example = "0")
                       @PathVariable final EntityId ddeId) {
        validate(request);
        response.setStatus(HttpStatus.NO_CONTENT.value());
        dataDictionaryService.delete(q -> q.byId(ddeId));
        eventPublisher.publishEvent(new DataDictionariesModifiedEvent(projectId));
    }

    /**
     * Search for data dictionary entries given a description AND/OR data element name.
     *
     * @param request         access to the request
     * @param response        the HTTP response
     * @param projectId       the ID of the project to search in
     * @param description     the description to search for
     * @param dataElementName the data element name to search for
     * @return a list of found {@linkplain DataDictionaryPojo}s
     */
    @GetMapping(value = DATA_DICTIONARY_SEARCH_URL)
    @Operation(summary = "Search data dictionary entries from a given project", operationId = "searchDataDictionary")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "404", description = "if the given project does not exist")
    @Nature({MINING})
    @Role({VIEWER})
    public List<DataDictionaryPojo> search(final HttpServletRequest request,
                                            final HttpServletResponse response,
                                            @Parameter(description = "the ID of the project to search", required = true, example = "0")
                                            @PathVariable final EntityId projectId,
                                            @Parameter(description = "a description to search for, either this or a dataElementName must be provided", 
                                            			required = false)
                                            @RequestParam(name = SEARCH_PARAM_DESCRIPTION, required = false) final String description,
                                            @Parameter(description = "a data element name to search for, either this or a description must be provided", 
                                            			required = false)
                                            @RequestParam(name = SEARCH_PARAM_DATA_ELEMENT_NAME, required = false) final String dataElementName) {
        validate(request);
        if (description == null && dataElementName == null) {
            response.setStatus(HttpStatus.BAD_REQUEST.value());
            throw new ConstraintViolationException(DataDictionaryPojo.class, "'description' AND/OR 'dataElementName' parameter must be set");
        }
        response.setStatus(HttpStatus.OK.value());
        return dataDictionaryService.find(q -> {
			q.ofModuleProject(projectId);
			if (dataElementName != null) {
				q.withName('%' + dataElementName + '%');
			}
			if (description != null) {
				q.withDescription('%' + description + '%');
			}
		});
    }

    /**
     * Lists all available other scopes.
     *
     * @param request   access to the request
     * @param response  the HTTP response
     * @param projectId the ID of the project to search in
     * @return a list of all available other scopes
     */
    @GetMapping(value = DATA_DICTIONARY_OTHER_SCOPES_URL)
    @Operation(summary = "List all other scopes", operationId = "findAllOtherScopes")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "404", description = "if the given project does not exist")
    @Nature({MINING})
    @Role({VIEWER})
    public List<String> otherScopes(final HttpServletRequest request,
                                                      final HttpServletResponse response,
                                                      @Parameter(description = "the ID of the project to list available scopes from", required = true, 
                                                      				example = "0") @PathVariable final EntityId projectId) {
        validate(request);
        response.setStatus(HttpStatus.OK.value());
        return dataDictionaryService.findDataDictionaryOtherScope(projectId);
    }

   /**
	 * Retrieves the format of a data field selected in the code editor.
	 *
	 * @param request   access to the request
	 * @param response  the HTTP response
	 * @param projectId the ID of the Project
	 * @param moduleId  the ID of the Module
	 * @param offset    the offset of the text that has been selected and needs to be validated
	 * @param assembled if the data field is selected in assembled mode
	 * @return format from ASTNode if the text selection is a valid data field
	 */
	@GetMapping(value = DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL)
	@Operation(summary = "validate text selection and return format", operationId = "getFormatIfSelectionIsValid")
	@ApiResponse(responseCode = "200", description = "if selection contained a valid data field")
	@ApiResponse(responseCode = "404", description = "if the given project or module do not exist or the selection offset did not contain a valid data field")
	@Nature({
			MINING
	})
	@Role({
			EDITOR
	})
	public DataFieldFormat getFormatIfSelectionIsValid(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId,
			@Parameter(description = "the offset of the text that has been selected and needs to be validated", required = true, example = "0")
			@PathVariable final int offset,
			@RequestParam(value = "assembled", required = false, defaultValue = "false") final boolean assembled) {
		validate(request, "offset");
		return astNodeService.getFormatIfSelectionIsValid(projectId, moduleId, offset, assembled);
	}

    /**
     * Returns aggregated values for data dictionary based on {@link AggregationRequest}.
     *
     * @param request            access to the request
     * @param response           access to the response
     * @param projectId          the project id of the data dictionary to fetch aggregation for
     * @param aggregationRequest object to hold request body
     * @return {@link List} containing {@link AggregationResult} object representing the aggregated result for the requested Data dictionary fields
     * and Project Id
     */
    @PostMapping(AGGREGATIONS_URL)
    @Operation(summary = "Get aggregated values over a number of data dictionaries", operationId = "getAggregatedValues")
    @ApiResponse(responseCode = "200", description = "if the request was successful")
    @ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
    @ApiResponse(responseCode = "404", description = "if the given project does not exist")
    @Nature({MINING})
    @Role({VIEWER})
	public List<AggregationResult<DataDictionaryFieldName>> getAggregatedValues(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true) 
			@RequestBody final AggregationRequest<DataDictionaryFieldName> aggregationRequest) {
        validate(request);
        validateAggregationRequest(aggregationRequest, DataDictionaryFieldName.PROJECT_ID, projectId);
		return dataDictionaryService.getAggregations(projectId, aggregationRequest);
    }
    
    /**
	 * Returns List of DataDictionary for a given module Based on Offset.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId  the ID of the module
	 * @param startOffset the Start Offset of Module
	 * @param endOffset the end OffSet of Module
	 * @return a list of DataDictionary for given moduleId Based on Offset.
	 */
	@GetMapping(value = DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET)
	@Operation(summary = "List of DataDictionary for a given module Based on Offset", operationId = "findDataDictionaryBasedOnOffset")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<DataDictionaryPojo> findDataDictionaryBasedOnOffset(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId,
			@Parameter(description = "the Start Offset of Module", required = true, example = "0") @RequestParam final Integer startOffset,
			@Parameter(description = "the End Offset of Module (optional)", required = false)
			@RequestParam(required = false) @Nullable final Integer endOffset) {
		validate(request);
		
		return dataDictionaryService.find(q -> {
			q.ofModuleProject(projectId)
			 .ofModule(moduleId);
			if (endOffset != null) {
				q.withOffsetBetween(startOffset, endOffset);
			} else {
				q.withMinOffset(startOffset);
			}
		});
	}

	/**
	 * Returns a list of DataDictionary ids based on data flow ids.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param dataFlowIds the List of Data Flow Ids
	 * @return a list of DataDictionary ids based on data flow ids.
	 */
	@GetMapping(value = DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS)
	@Operation(summary = "List of DataDictionary ids based on data flow ids", operationId = "findDataDictionaryIdsBasedOnDataFlowIds")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<EntityId> findDataDictionaryIdsBasedOnDataFlowIds(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId,
			@RequestParam(required = true) final List<String> dataFlowIds) {
		validate(request);
		return dataDictionaryService.getDataDictionaryIdsFromDataFlowIds(dataFlowIds);
	}
}
