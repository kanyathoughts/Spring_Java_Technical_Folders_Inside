/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.module;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.extensions.export.datalineage.DataFlowGraphQueryJob;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.tinkerpop.shaded.minlog.Log;
import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
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

import cz.jirutka.rsql.parser.RSQLParserException;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.lang.Prototype;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.extensions.export.csv.CSVLineBuilderFactory;
import innowake.mining.extensions.export.table.DataPointTableExportHelper;
import innowake.mining.server.Logging;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.controller.BaseController;
import innowake.mining.server.controller.MiningRestController;
import innowake.mining.server.controller.module.filter.DependencyGraphFilterParser;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.job.identification.IdentifyModuleDescriptionsJob;
import innowake.mining.server.opensearch.OpenSearchService;
import innowake.mining.server.service.ContentAssemblingService;
import innowake.mining.server.service.CodeViewerDataLineageService;
import innowake.mining.server.service.CodeViewerLinkService;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.server.service.GenerativeModuleDescriptionService;
import innowake.mining.server.service.TryLock;
import innowake.mining.server.service.UtilityAggregationService;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.extensions.MiningExportExtension.ExportValue;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.LinkedModule;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleStatisticsResponse;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.model.codeviewer.AssembledContent;
import innowake.mining.shared.model.codeviewer.CodeViewerLinkModel;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.dependency.graph.NodeType;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for {@code module} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class ModuleController extends BaseController {
	
	/**
	 * URL pattern for the module collection.
	 */
	public static final String MODULE_COLLECTION_URL = "/v1/projects/{projectId}/modules";

	/**
	 * URL pattern for a single module by ID.
	 */
	public static final String MODULE_BY_ID_URL = "/v1/projects/{projectId}/modules/{moduleId}";
	
	public static final String MODULE_BY_HASH_URL = "/v1/projects/{projectId}/modules/hash/{linkHash}";

	/**
	 * URL pattern for retrieving the module count.
	 */
	public static final String MODULE_COUNT_URL = "/v1/projects/{projectId}/modules/count";

	/**
	 * URL pattern for modules to review.
	 */
	public static final String MODULE_REVIEW_URL = "/v1/projects/{projectId}/modules/requiresReview";

	/**
	 * Value for the currently allowed export format.
	 */
	public static final String ALLOWED_EXPORT_FORMAT = "csv";

	/**
	 * Parameter name for the export format.
	 */
	public static final String EXPORT_PARAM_FORMAT = "format";

	/**
	 * URL pattern for searching modules.
	 */
	public static final String MODULE_SEARCH_URL = "/v1/projects/{projectId}/modules/search";
	
	/**
	 * URL pattern for searching linked modules.
	 */
	public static final String LINKED_MODULE_SEARCH_URL = "/v1/projects/{projectId}/linkedModules/search";

	/**
	 * URL pattern for retrieving the code viewer links
	 */
	public static final String CODE_VIEWER_LINKS_URL = MODULE_BY_ID_URL + "/code-viewer-links";

	/**
	 * URL pattern for retrieving the code viewer links including the dataFlowNodes for given node at offset
	 */
	public static final String CODE_VIEWER_DATA_FLOW_LINKS_URL = MODULE_BY_ID_URL + "/code-viewer-data-flow-links/{offset}";

	/**
	 * URL pattern for retrieving the Data lineage graph based on the field offset
	 */
	public static final String CODE_VIEWER_DATA_FLOW_GRAPH_URL = MODULE_BY_ID_URL + "/code-viewer-data-flow-graph/{offset}";

	/**
	 * URL pattern for Data lineage Job based on the field offset
	 */
	public static final String CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL = MODULE_BY_ID_URL + "/code-viewer-data-flow-graph-job/{offset}";

	/**
	 * URL pattern for retrieving the assembled content
	 */
	public static final String ASSEMBLED_CONTENT_URL = MODULE_BY_ID_URL + "/assembled-content";

	/**
	 * Parameter name for searching by description.
	 */
	public static final String SEARCH_PARAM_DESCRIPTION = "description";

	/**
	 * Parameter name for searching by name.
	 */
	public static final String SEARCH_PARAM_NAME = "name";

	/**
	 * Parameter name for searching by path.
	 */
	public static final String SEARCH_PARAM_PATH = "path";

	/**
	 * URL pattern for AST storage of a single module.
	 */
	public static final String STORE_AST_URL = "/v1/projects/{projectId}/modules/{moduleId}/storeAst";

	/**
	 * URL pattern for determining if a single module has an AST.
	 */
	public static final String HAS_AST_URL = "/v1/projects/{projectId}/modules/{moduleId}/hasAstNodes";

	/**
	 * URL pattern for identifying module descriptions.
	 */
	public static final String DESCRIPTION_IDENTIFICATION_URL = "/v1/projects/{projectId}/identify-module-descriptions";

	/**
	 * URL pattern for retrieving the dependencies from a module.
	 */
	public static final String DEPENDENCIES_URL = "/v1/projects/{projectId}/modules/{moduleId}/graph/dependencies";

	/**
	 * URL pattern for retrieving the taxonomies of a single module.
	 */
	public static final String TAXONOMY_COLLECTION_URL = "/v1/projects/{projectId}/modules/{moduleId}/taxonomies";

	/**
	 * URL pattern for retrieving the annotations of a single module.
	 */
	public static final String ANNOTATION_COLLECTION_URL = "/v1/projects/{projectId}/modules/{moduleId}/annotations";
	
	/**
	 * URL pattern for retrieving the error markers of a single module.
	 */
	public static final String MODULE_ERROR_MARKER = "/v1/projects/{projectId}/modules/{moduleId}/errorMarkers";

	public static final String MODULE_DESCRIPTION_GENERATOR = "/v1/projects/{projectId}/modules/{moduleId}/generateDescription";

	/**
	 * URL pattern for aggregated values 
	 */
	public static final String AGGREGATIONS_URL = "/v1/projects/{projectId}/modules/aggregations";
	
	/**
	 * URL pattern for aggregated utility values 
	 */
	public static final String UTILITY_AGGREGATIONS_URL = "/v1/projects/{projectId}/modules/utility-aggregations";
	public static final String UTILITY_AGGREGATIONS_CSV_URL = "/v1/projects/{projectId}/modules/utility-aggregations/csv";
	
	/**
	 * URL pattern for checking if Data Lineage is supported for a given module.
	 */
	public static final String CHECK_DATALINEAGE_AVAILABILITY = "/v1/projects/{projectId}/modules/{moduleId}/datalineage-available";
	
	/**
	 * Parameter name for deleting source objects.
	 */
	public static final String DELETE_PARAM_DELETE_SOURCE_OBJECTS = "deleteSourceObjects";
	
	/**
	 * URL pattern for returning statistics of modules associated with the given project.
	 */
	public static final String MODULE_STATISTICS = "/v1/projects/{projectId}/modules/statistics";

	/**
	 * URL pattern for finding included moduleIds for a given module ID.
	 */
	public static final String INCLUDED_MODULES_URL = "/v1/projects/{projectId}/modules/{moduleId}/includedModuleIds";

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);

	@Autowired
	private AstService astService;

	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private ExecutorService executorService;
	
	@Autowired
	private UtilityAggregationService utilityAggregationService;
	
	@Autowired
	private DataLineageCoreService dataLineageCoreService;

	@Autowired
	private CodeViewerLinkService codeViewerLinkService;

	@Autowired
	private CodeViewerDataLineageService codeViewerDataLineageService;

	@Autowired
	private ContentAssemblingService contentAssemblingService;

	@Autowired
	private FF4j ff4j;

	@Autowired
	private OpenSearchService openSearchService;

	@Autowired
	private ApplicationEventPublisher eventPublisher;

	@Autowired
	private GenerativeModuleDescriptionService descriptionService;

	/**
	 * Returns all available modules for a given project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return a list of modules
	 */
	@GetMapping(MODULE_COLLECTION_URL)
	@Operation(summary = "List all modules of a project", operationId = "findAllModules")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModulePojo> findAll(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project to list modules from", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return moduleService.findModules(q -> q.ofProject(projectId));
	}
	
	/**
	 * Finds a {@link ModulePojo} by an ID.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param moduleId the module ID of the module to find
	 * @param includeContent if source code needs to be included
	 * @return a {@link ModulePojo}
	 */
	@GetMapping(MODULE_BY_ID_URL)
	@Operation(summary = "Find a module by its ID", operationId = "findModuleById")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public ModulePojo findById(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the module to search for", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@Parameter(description = "if source code needs to be included")
			@RequestParam(required = false) final boolean includeContent) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findAnyModule(q -> q.ofProject(projectId)
												.byId(moduleId)
												.includeContent(includeContent))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId.toString()));
	}
	
	/**
	 * Finds a {@code module} by a hash value of module.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param linkHash the hash value of the module to find
	 * @param includeContent if source code needs to be included
	 * @return a {@link ModulePojo}
	 */
	@GetMapping(MODULE_BY_HASH_URL)
	@Operation(summary = "Find a module by its linkHash", operationId = "findModuleByLinkHash")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module linkHash does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public ModulePojo findByLinkHash(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the linkHash of the module to search for", required = true, example = "DB8E685EEE30B27AFE3214BA23DDF1E8")
			@PathVariable final String linkHash,
			@Parameter(description = "if source code needs to be included")
			@RequestParam(required = false) final boolean includeContent) {
		validate(request, "linkHash");
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findAnyModule(q -> q.ofProject(projectId)
												.withLinkHash(linkHash)
												.includeContent(includeContent))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with link hash: " + linkHash));
	}
	
	/**
	 * Finds a {@code module} by path.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param path the path of the module to find
	 * @param includeContent if source code needs to be included
	 * @return a {@link ModulePojo}
	 */
	@GetMapping(value = MODULE_SEARCH_URL, params = SEARCH_PARAM_PATH)
	@Operation(summary = "Find one module by providing its path in the project", operationId = "findModuleByPath")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module path does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public ModulePojo findByPath(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the lookup path", required = true)
			@RequestParam(SEARCH_PARAM_PATH) final String path,
			@Parameter(description = "if source code needs to be included")
			@RequestParam(required = false) final boolean includeContent) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		
		return moduleService.findAnyModule(q -> q.ofProject(projectId)
												.withPath(path)
												.includeContent(includeContent))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: " + path));
	}
	
	/**
	 * Find linked modules for the current given module
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param path the path of the module to find
	 * @return Returns a list of all linkedModules
	 */
	@GetMapping(value = LINKED_MODULE_SEARCH_URL, params = SEARCH_PARAM_PATH)
	@Operation(summary = "Find linked modules for the current given module", operationId = "findLinkedModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module path does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public List<LinkedModule> findLinkedModule(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the lookup path", required = true)
			@RequestParam(SEARCH_PARAM_PATH) final String path) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return moduleService.findLinkedModules(q -> q.ofProject(projectId)
														.withSourcePath(path)
														.withSourceStorage(Storage.FILE)
														.withRelationships(List.of(RelationshipType.CALLS, RelationshipType.REFERENCES)));
	}

	/**
	 * Get code viewer links for given module
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param moduleId The id of the module
	 * @param assembled Whether the codeViewer is assembled or not
	 * @return Returns the CodeVieweLinkModel
	 */
	@GetMapping(value = CODE_VIEWER_LINKS_URL)
	@Operation(summary = "Get code viewer links for given module", operationId = "getCodeViewerLinks")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public CodeViewerLinkModel getCodeViewerLinks(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true)
			@PathVariable final EntityId moduleId,
			@RequestParam(value = "assembled", required = false, defaultValue = "false") final boolean assembled) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return codeViewerLinkService.getLinkModel(projectId, moduleId, assembled);
	}

	/**
	 * Get assembled content for given module
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param moduleId The id of the module
	 * @return Returns the assembledContent
	 */
	@GetMapping(value = ASSEMBLED_CONTENT_URL)
	@Operation(summary = "Get assembled content for given module", operationId = "getAssembledContent")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public AssembledContent getAssembledContent(final HttpServletRequest request, final HttpServletResponse response,
												@Parameter(description = "the ID of the project", required = true, example = "0")
												@PathVariable final EntityId projectId,
												@Parameter(description = "the ID of the module", required = true)
												@PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return contentAssemblingService.getAssembledContent(projectId, moduleId);
	}

	/**
	 * Get code viewer data flow links for given module and field
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module to find
	 * @param moduleId The id of the module
	 * @param offset The offset of the node to look at
	 * @param assembled Whether the codeViewer is assembled or not
	 * @return Returns the CodeVieweLinkModel
	 */
	@GetMapping(value = CODE_VIEWER_DATA_FLOW_LINKS_URL)
	@Operation(summary = "Get code viewer data flow links for given module and field", operationId = "getCodeViewerDataFlowLinks")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public CodeViewerLinkModel getCodeViewerDataFlowLinks(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true)
			@PathVariable final EntityId moduleId,
			@Parameter(description = "the offset of the field within the module", required = true)
			@PathVariable final Integer offset,
			@RequestParam(value = "assembled", required = false, defaultValue = "false") final boolean assembled) {
		validate(request, "offset");
		response.setStatus(HttpStatus.OK.value());
		return codeViewerDataLineageService.getDataFlowLinksForField(projectId, moduleId, offset, assembled);
	}

	/**
	 * Gets the data flow graph nodes for a given module and field offset.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module
	 * @param moduleId the id of the module
	 * @param offset the offset of the field
	 * @param assembled whether the codeViewer is assembled or not
	 * @param includingModule the module link hash of the including module
	 * @return Returns the {@link DataFlowGraph}
	 */
	@GetMapping(value = CODE_VIEWER_DATA_FLOW_GRAPH_URL)
	@Operation(summary = "Get data flow graph for given module and field", operationId = "getCodeViewerDataFlowGraph")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({
			MINING
	})
	@Role({
			VIEWER
	})
	public DataFlowGraph getDataFlowGraphForField(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true) @PathVariable final EntityId moduleId,
			@Parameter(description = "the offset of the field within the module", required = true) @PathVariable final Integer offset,
			@RequestParam(value = "assembled", required = false, defaultValue = "false") final boolean assembled,
			@RequestParam(value = "includingModule", required = false, defaultValue = "") final String includingModule) {

		validate(request, "offset");

		final EntityId includingModuleId = populateIncludingModuleId(projectId, includingModule);

		response.setStatus(HttpStatus.OK.value());
		return codeViewerDataLineageService.getDataFlowGraphForField(projectId, moduleId, offset, assembled, includingModuleId);
	}

	/**
	 * Runs the data flow graph query job for a given module and field offset.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the module
	 * @param moduleId the id of the module
	 * @param offset the offset of the field
	 * @param assembled whether the codeViewer is assembled or not
	 * @param includingModule the module link hash of the including module
	 * @param columnName the column name associated with a table module
	 * @param containerType the proxy container type
	 * @return Returns the ID of the job
	 */
	@GetMapping(value = CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL)
	@Operation(summary = "Runs the data flow graph query job for a given module and field", operationId = "getCodeViewerDataFlowGraphJob")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({
			MINING
	})
	@Role({
			VIEWER
	})
	public char[] getDataFlowGraphForFieldJob(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true) @PathVariable final EntityId moduleId,
			@Parameter(description = "the offset of the field within the module", required = false) @PathVariable final Integer offset,
			@RequestParam(value = "assembled", required = false, defaultValue = "false") final boolean assembled,
			@RequestParam(value = "includingModule", required = false, defaultValue = "") final String includingModule,
			@RequestParam(value = "name", required = false, defaultValue = "") final String columnName,
			@RequestParam(value = "type", required = false, defaultValue = "") final ProxyContainerPojo.Type containerType
	) {

		if (offset > -1) {
			validate(request, "offset");
		}

		final EntityId includingModuleId = populateIncludingModuleId(projectId, includingModule);

		final Parameters parameters = new Parameters.Builder()
				.setProjectId(projectId)
				.addStartField(moduleId, offset, includingModuleId)
				.addStartProxyField(moduleId, containerType, columnName)
				.setDetailLevel(DetailLevel.STATEMENT)
				.setAssembled(assembled)
				.build();

		final DataFlowGraphQueryJob dataFlowGraphQueryJob = new DataFlowGraphQueryJob(parameters);
		final JobMonitor jobMonitor = jobManager.submit(dataFlowGraphQueryJob);
		response.setStatus(HttpStatus.OK.value());

		/* Explicitly returning the job Id as a char array, as a plain String would require a wrapper object for valid JSON. */
		return jobMonitor.getJobId().toCharArray();
	}

	@Nullable
	private EntityId populateIncludingModuleId(final EntityId projectId, final String includingModule) {
		if ( ! StringUtils.isEmpty(includingModule)) {
			return moduleService.findModuleIds(q -> q.ofProject(projectId).withLinkHash(includingModule)).stream().findAny()
					.orElseThrow(() -> new IllegalArgumentException("Including Module with link hash" + includingModule + " not found."));
		} else {
			return null;
		}
	}

	/**
	 * Finds {@code modules} by name.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the modules to find
	 * @param name the name of the modules to find
	 * @return a list of modules
	 */
	@GetMapping(value = MODULE_SEARCH_URL, params = SEARCH_PARAM_NAME)
	@Operation(summary = "Search modules by name", operationId = "findModulesByName")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModulePojo> findByName(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the module name to search for", required = true)
			@RequestParam(SEARCH_PARAM_NAME) final String name) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findModules(q -> q.ofProject(projectId)
												 .withName(name));
	}
	
	/**
	 * Finds {@code modules} by description.
	 * 
	 * @param request access to the request
	 * @param response access the response
	 * @param projectId the project id of the modules to find
	 * @param description the description of the modules to find
	 * @return a list of {@linkplain ModulePojo module}
	 */
	@GetMapping(value = MODULE_SEARCH_URL, params = SEARCH_PARAM_DESCRIPTION)
	@Operation(summary = "Search modules by description", operationId = "findModulesByDescription")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModulePojo> findByDescription(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project to search", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the description of the module to search for", required = true)
			@RequestParam(SEARCH_PARAM_DESCRIPTION) final String description) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		
		return moduleService.findModules(q -> q.ofProject(projectId)
												 .withDescription(description));
	}
	
	/**
	 * Returns the number of modules associated with the given project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return the number of modules associated with the given project
	 */
	@GetMapping(MODULE_COUNT_URL)
	@Operation(summary = "Count all modules for one project", operationId = "getModuleCount")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the provided project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Long getModuleCount(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project to examine", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return moduleService.countModules(q -> q.ofProject(projectId));
	}
	
	/**
	 * Creates a module given a project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param module the module to create
	 * @return the newly created module with its ID
	 */
	@PostMapping(MODULE_COLLECTION_URL)
	@Operation(summary = "Create a new module", operationId = "createModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given module is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.MODULES, reasonPhrase = "Applied Lock on Create Module")
	public ModulePojo create(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The module to create", required = true) 
			@RequestBody final ModulePojoPrototype module) {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());

		if (! module.creator.isDefined()) {
			module.setCreator(Creator.API);
		}

		projectId.matchOrApply(module.project.orElseNonNull(EntityId.VOID), module::setProject);
		final EntityId moduleId = moduleService.create(module);
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(moduleId)));
		return moduleService.findAnyModule(q -> q.byId(moduleId))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId.toString()));
	}

	/**
	 * Updates the given module.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param module the module with the updated values
	 * @return the updated module
	 */
	@PutMapping(MODULE_BY_ID_URL)
	@Operation(summary = "Update an existing module", operationId = "updateModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given module is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.MODULES, reasonPhrase = "Applied Lock on Update Module")
	public ModulePojo update(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module to be updated", required = true, example = "0") 
			@PathVariable final EntityId moduleId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated module data", required = true) 
			@RequestBody final ModulePojoPrototype module) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		moduleId.matchOrApply(module.identityProvisional(), module::withId);
		projectId.matchOrApply(module.project.orElseNonNull(EntityId.VOID), module::setProject);

		final EntityId updatedModule = moduleService.update(module);
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(updatedModule)));
		return moduleService.findAnyModule(q -> q.byId(moduleId))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId.toString()));
	}
	
	/**
	 * Deletes the module with the given ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 */
	@DeleteMapping(MODULE_BY_ID_URL)
	@Operation(summary="Delete a module", operationId  = "deleteModule")
	@ApiResponse(responseCode = "204", description = "if the module was successfully deleted")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.MODULES, reasonPhrase = "Applied Lock on Delete Module")
	public void delete(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the module to delete", required = true, example = "0")
			@PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());

		/* Check that module belongs to project */
		if (moduleService.findAnyModule(q -> q.byId(moduleId)).isEmpty()) {
			throw new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId);
		}

		moduleService.deleteModule(moduleId, true);
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.ofNullable(moduleId)));
	}
	
	/**
	 * Deletes all modules from the given project
	 *
	 * @param request Access to the request
	 * @param response The HTTP response
	 * @param projectId The ID of the project
	 * @param deleteSourceObjects The boolean value should be set to true to delete source objects.
	 */
	@DeleteMapping(value = MODULE_COLLECTION_URL, params = DELETE_PARAM_DELETE_SOURCE_OBJECTS)
	@Operation(summary = "Delete all modules for one project", operationId = "deleteAllModules")
	@ApiResponse(responseCode = "204", description = "if all modules were successfully deleted")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.MODULES, reasonPhrase = "Applied Lock on Delete all modules")
	public void delete(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "should be set to true to delete source objects", required = true)
			@RequestParam(DELETE_PARAM_DELETE_SOURCE_OBJECTS) final boolean deleteSourceObjects) {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		moduleService.deleteModules(projectId, deleteSourceObjects, false);
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.empty()));
	}
	
	/**
	 * Finds all annotations assigned to a module by a given project id and module id.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @return the list of {@link AnnotationPojo}
	 */
	@GetMapping(ANNOTATION_COLLECTION_URL)
	@Operation(summary = "Find all annotations assigned to a module", operationId = "findAnnotationsForModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AnnotationPojo> findAnnotations(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return annotationService.find(q -> q.ofProject(projectId)
											.ofModule(moduleId));
	}
	
	/**
	 * Finds all taxonomies assigned to a module by a given project id and module id.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @return the list of {@link TaxonomyPojo}
	 */
	@GetMapping(TAXONOMY_COLLECTION_URL)
	@Operation(summary = "Find all taxonomies of the module", operationId = "findTaxonomiesForModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<TaxonomyPojo> findTaxonomies(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId){
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return taxonomyService.find(q -> q.ofProject(projectId)
										  .ofModule(moduleId));
	}
	
	/**
	 * Finds all error markers set for the given module and project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @return the list of {@link ErrorMarker}
	 */
	@GetMapping(MODULE_ERROR_MARKER)
	@Operation(summary = "Find error markers in the module", operationId = "findErrorMarkers")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	@Nullable
	public List<ErrorMarker> findErrorMarkers(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project to search", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module to search for", required = true, example = "0") @PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(moduleId)).stream()
								.map(pojo -> new ErrorMarker(pojo.getSeverity(), pojo.getKey(),
															 pojo.getCause(), pojo.getLocation()))
								.collect(Collectors.toList());
	}

	/**
	 * Suggests a module description using generative AI
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @return Module with generated module description using Generative AI
	 */
	@PostMapping(MODULE_DESCRIPTION_GENERATOR)
	@Operation(summary = "Generate module description", operationId = "generateModuleDescription")
	@ApiResponse(responseCode = "404", description = "if the given project or module ID does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@Nullable
	public ModulePojo generateModuleDescription(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId) {
		validate(request);
		final var result = descriptionService.deduceDescription(projectId, moduleId);
		response.setStatus(HttpStatus.OK.value());
		return result;
	}

	/**
	 * Returns all dependent modules and all references specific to a single module.
	 * 
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param maxDepth the depth of the traversal
	 * @param maxGraphNodes maximum graph nodes to return in result
	 * @param query filtering query
	 * @param distinct The distinct option for filtering duplicate dependencies
	 * @return the {@link DependencyGraph}
	 */
	@GetMapping(DEPENDENCIES_URL)
	@Operation(summary = "Find all dependencies of a module", operationId = "traverseModuleDependencies")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public DependencyGraph traverseDependencies(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@Parameter(description = "the depth of the traversal", required = true, example = "0")
			@RequestParam final Long maxDepth,
			@Parameter(description = "the number of graph nodes to be returned", example = "0")
			@RequestParam final Optional<Integer> maxGraphNodes,
			@Parameter(description = "filtering parameters", example = "0")
			@RequestParam final Optional<String> query,
			@Parameter(description = "filter duplicate dependencies", example = "false")
			@RequestParam(required = false, defaultValue = "false") final boolean distinct) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		try {
			final Tuple2<List<NodeType>, List<RelationshipType>> filterParameters = DependencyGraphFilterParser.parseFilterQuery(query);
			final boolean isEnabled = ff4j.exist("dependencyGraphExplore") && ff4j.getFeature("dependencyGraphExplore").isEnable();
			return moduleService.traverseDependencies(projectId, moduleId, maxDepth, maxGraphNodes, filterParameters.a, filterParameters.b, distinct, isEnabled);
		} catch (final RSQLParserException e) {
			LOG.error("An error occured while parsing the filter query", e);
			throw new IllegalArgumentException("An error occured while parsing the filter query", e);
		}
	}
	
	/**
	 * Starts a job that identifies the descriptions for the given modules.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleMatcher the ids and/or paths of the modules to identify the Module descriptions for
	 * @return the Id of the job
	 */
	@Prototype(irisId = "WMIN-597")
	@PostMapping(DESCRIPTION_IDENTIFICATION_URL)
	@Operation(
			summary = "Starts a job that identifies Module descriptions for the given modules and returns the job Id. "
					+ "See the job status at '/v1/jobs/{jobId}/info'",
					operationId  = "identifyModuleDescriptions"
	)
	@ApiResponse(responseCode = "202", description = "if the job has been successfully submitted")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] identifyModuleDescriptions(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final EntityId projectId,
	 		@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "List of id's and/or the file paths of the modules relative to the project. "
	 				+ "Example = {1}, {src/file.cbl}", required = true) 
			@RequestBody final ModuleMatcher moduleMatcher) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		final IdentifyModuleDescriptionsJob job = new IdentifyModuleDescriptionsJob(projectId, moduleMatcher);
		final JobMonitor jobMonitor = jobManager.submit(job);
		/* Explicitly returning the job Id as a char array, as a plain String would require a wrapper object for valid JSON. */
		return jobMonitor.getJobId().toCharArray();
	}
	
	/**
	 * Method to check if the specified module has AST Nodes or not.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @return true if AST Nodes have been calculated, false otherwise
	 */
	@GetMapping(HAS_AST_URL)
	@Operation(summary = "Find if module has AST Nodes", operationId = "hasAstNodes")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public boolean hasAstNodes(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return astService.count(q -> q.ofProject(projectId).ofModule(moduleId)) > 0l;
	}
	
	/**
	 * Endpoint to store the AST Nodes for the specified Module.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param projectId The ID of the Project
	 * @param moduleId The ID of the Module
	 * @return {@code true} if storing AST Nodes was successful or if it was already existing
	 */
	@PostMapping(STORE_AST_URL)
	@Operation(summary = "Store AST Nodes for the specified Module", operationId = "storeAstNodes")
	@ApiResponse(responseCode = "202", description = "if the AST Nodes have been stored successfully or if they're already existing")
	@ApiResponse(responseCode = "404", description = "if the given projectId or moduleId does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public boolean storeAstForModule(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		boolean storeAst = false;

		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(q -> q.ofProject(projectId).byId(moduleId))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId.toString() + " in project: " + projectId));

		if (module.getType() == Type.COPYBOOK) {
			final List<UUID> referencingModules = moduleService.findRelatedModules(moduleId, RelationshipType.INCLUDES, RelationshipDirection.IN);
			if ( ! referencingModules.isEmpty()) {
				storeAst = executorService.executeStoreAst(projectId, EntityId.of(referencingModules.get(0)));
			}
		} else {
			storeAst = executorService.executeStoreAst(projectId, module.identity());
		}
		return storeAst;
	}
	
	/**
	 * Endpoint to retrieve aggregated values for Module fields
	 * 
	 * @param request Access to the request
	 * @param response The HTTP Response
	 * @param projectId The ID of the Project
	 * @param aggregationRequest The aggregation of requested Module fields
	 * @return {List<AggregationResult<ModuleFieldName>>} Returns a list of aggregated value for the requested Module fields and Project Id
	 */
	@PostMapping(AGGREGATIONS_URL)
	@Operation(summary = "Get aggregated values over a number of modules", operationId = "getAggregatedValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AggregationResult<ModuleFieldName>> getAggregatedValues(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true)
			@RequestBody final AggregationRequest<ModuleFieldName> aggregationRequest) {
		validate(request);
		if (openSearchService.isAvailable()) {
			validateAggregationRequest(aggregationRequest);
			try {
				final List<AggregationResult<String>> aggregationResults = openSearchService
						.queryAggregations("moduletable", projectService.getNid(projectId), aggregationRequest);
				final List<AggregationResult<ModuleFieldName>> parsedAggregationResults = new ArrayList<>();
				for (final AggregationResult<String> ar : aggregationResults) {
					final AggregationResult<ModuleFieldName> par = new AggregationResult<>();
					ar.getFields().forEach((k, v) -> {
						par.getFields().put(ModuleFieldName.valueOf(k), v);
					});

					ar.getGroup().forEach((k,v) -> {
						par.getGroup().put(ModuleFieldName.valueOf(k), v);
					});
					parsedAggregationResults.add(par);
				}

				return parsedAggregationResults;
			} catch (final Exception e) {
				Log.debug("OpenSearch not available, defaulting to standard method");
			}
		}

		validateAggregationRequest(aggregationRequest, ModuleFieldName.PROJECT_ID, projectId);
		return moduleService.getAggregations(projectId, aggregationRequest);
	}
	
	/**
	 * End point to retrieve aggregated values for Utility fields
	 * 
	 * @param request Access to the request
	 * @param response The HTTP Response
	 * @param projectId The ID of the Project
	 * @param aggregationRequest The aggregation of requested Utility fields
	 * @return {List<AggregationResult<UtilityFieldName>>} Returns a list of aggregated value for the requested Utility fields and Project Id
	 */
	@PostMapping(UTILITY_AGGREGATIONS_URL)
	@Operation(summary = "Get aggregated values for utilities used by a project", operationId = "getAggregatedUtilityValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING, DISCOVERY})
	@Role({VIEWER})
	public List<AggregationResult<ModuleFieldName>> getAggregatedUtilityValues(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true)
			@RequestBody final AggregationRequest<ModuleFieldName> aggregationRequest) {

		validate(request);
		validateAggregationRequest(aggregationRequest);
		return utilityAggregationService.getAggregatedUtilityValues(projectId, aggregationRequest);
	}

	/**
	 * End point to retrieve aggregated values for Utility fields in CSV format.
	 *
	 * @param request Access to the request
	 * @param response the HTTP response
	 * @param projectId The ID of the Project
	 * @param aggregationRequest The aggregation of requested Utility fields
	 * @return response entity with a resource body
	 */
	@PostMapping(UTILITY_AGGREGATIONS_CSV_URL)
	@Operation(summary = "Get aggregated values for utilities used by a project as CSV", operationId = "getAggregatedUtilityValuesAsCsv")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING, DISCOVERY})
	@Role({VIEWER})
	public ResponseEntity<Resource> getAggregatedUtilityValuesAsCsv(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true)
			@RequestBody final AggregationRequest<ModuleFieldName> aggregationRequest) {

		validate(request);
		validateAggregationRequest(aggregationRequest);
		final List<AggregationResult<ModuleFieldName>> results = utilityAggregationService.getAggregatedUtilityValues(projectId, aggregationRequest);
		final List<ModuleFieldName> groupHeaders = new ArrayList<>(aggregationRequest.getGroupBy());
		final List<ModuleFieldName> fieldHeaders = new ArrayList<>(aggregationRequest.getFields().keySet());
		final List<List<String>> rowsAndColumns = DataPointTableExportHelper.convertAggregationResultToTable(results, groupHeaders, fieldHeaders);
		final Stream<ModuleFieldName> headers = Stream.concat(groupHeaders.stream(), fieldHeaders.stream());
		final long projectNid = projectService.getNid(projectId);

		final ExportValue exportValue = DataPointTableExportHelper
				.buildExportValue("utilities", projectNid, "text/csv", new CSVLineBuilderFactory(), rowsAndColumns,
						(MapUtils.isEmpty(aggregationRequest.getCsvHeaders()) ? headers : headers.map(key -> aggregationRequest.getCsvHeaders().get(key)))
						.map(Object::toString).collect(Collectors.toList()));

		final Resource resource = new InputStreamResource(exportValue.getInputStream());
		response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, String.format("attachment; filename=%s", exportValue.getFileName()))
				.header(HttpHeaders.CONTENT_TYPE, exportValue.getContentType())
				.body(resource);
	}
	
	/**
	 * Returns the number of modules to review on a project
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return number of modules
	 */
	@GetMapping(MODULE_REVIEW_URL)
	@Operation(summary = "Count modules to review on a project", operationId = "countRequiresReview")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Long countRequiresReview(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project for which to count modules to review", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return moduleService.countModules(q -> q.ofProject(projectId)
												  .withRequiresReview(true));
	}
	
	/**
	 * Clear the review status for all modules on a project
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 */
	@DeleteMapping(MODULE_REVIEW_URL)
	@Operation(summary = "Clear the review status for all modules on a project", operationId = "clearRequiresReview")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.MODULES, reasonPhrase = "Applied Lock on Clear review status for all modules on a project")
	public void clearRequiresReview(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project on which to reset the review status for all modules", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		moduleService.updateModules(q -> q.ofProject(projectId), new ModulePojoPrototype().setRequiresReview(false));
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.empty()));
	}
	
	/**
	 * Endpoint for checking if DataLineage is available for module.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param projectId The ID of the Project
	 * @param moduleId The ID of the module
	 * @return boolean Returns true if Data Lineage is available for this module 
	 */
	@GetMapping(CHECK_DATALINEAGE_AVAILABILITY)
	@Operation(summary = "Checks if DataLineage is possible for given module", operationId = "getDataLineageAvailableForModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public boolean getDataLineageAvailableForModule(final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId) {
		validate(request);
		
		return dataLineageCoreService.isSupported(projectId, moduleId);
	}

	/**
	 * Returns the statistics of modules associated with the given project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return the number of modules associated with the given project
	 */
	@GetMapping(MODULE_STATISTICS)
	@Operation(summary = "Statistics of all modules for one project", operationId = "getModuleStatistics")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public ModuleStatisticsResponse getModuleStatistics(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project to examine", required = true, example = "0") @PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return moduleService.calculateStatistics(projectId);
	}

	/**
	 * Returns List of included moduleIds for given moduleId.
	 *
	 * @param request   access to the request
	 * @param response  the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId  the ID of the module
	 * @return a list of included moduleIds for given moduleId.
	 */
	@GetMapping(value = INCLUDED_MODULES_URL)
	@Operation(summary = "List of included moduleIds for a given module", operationId = "findIncludedModuleIds")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<Long> findIncludedModuleIds(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		
		final var includees = EntityId.allNids(moduleService.findModuleIds(q -> q.ofProject(projectId)
																						.withSourceRelationshipsFrom(moduleId, RelationshipType.INCLUDES)));

		if (includees.isEmpty()) {
			return List.of(moduleService.getModuleNid(moduleId));
		}

		final List<Long> result = new ArrayList<>(includees.size() + 1);
		result.add(moduleService.getModuleNid(moduleId));
		result.addAll(includees);
		return result;
	}
}

