/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.cfg;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.controller.BaseController;
import innowake.mining.server.controller.MiningUnsecuredRestController;
import innowake.mining.server.controller.MiningUnsecuredRestEndpoint;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.dependency.graph.NodeType;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for control flow requests.
 */
@MiningUnsecuredRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class ControlFlowController extends BaseController {
	
	/**
	 * URL pattern for retrieving and calculating the control flow graph for a single module.
	 */
	public static final String CONTROL_FLOW_SINGLE_URL = "/v1/projects/{projectId}/control-flow/{moduleId}";
	
	/**
	 * URL pattern for calculation of the control flow graph for multiple modules.
	 */
	public static final String CONTROL_FLOW_MULTIPLE_URL = "/v1/projects/{projectId}/control-flow";
	
	/**
	 * URL pattern for retrieving the list of module types supported by the control flow graph.
	 */
	public static final String CONTROL_FLOW_SUPPORT_URL = "/v1/control-flow-support";

	@Autowired
	private JobManager jobManager;

	@Autowired
	private AstService astService;

	@Autowired
	private DataFlowService dataFlowService;

	/**
	 * Gets the control flow for a module ID.
	 *
	 * @param request access to the request
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param characterLimit the limit for fetching node label
	 * @param checkCalculated do not retrieve the graph, only check if it is present
	 * @return the {@link ControlFlowGraph}
	 */
	@GetMapping(CONTROL_FLOW_SINGLE_URL)
	@Operation(summary = "Gets the control flow for a module ID", operationId  = "getControlFlow")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "204", description = "if checkCalculated was specified and a Control Flow exists")
	@ApiResponse(responseCode = "404", description = "if the given project id or module id does not exist")
	@Role({VIEWER})
	@Nature({MINING})
	public ResponseEntity<ControlFlowGraph> getControlFlow(
			final HttpServletRequest request,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the module ID", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@Parameter(description = "the character Limit", example = "0")
			@RequestParam(required = false, value="characterLimit") final Integer characterLimit,
			@Parameter(description = "only check if a graph is present", example = "0")
			@RequestParam(required = false, value="checkCalculated", defaultValue = "false") final boolean checkCalculated) {
		validate(request);
		if (checkCalculated) {
			return (astService.findModuleRelationships(q -> q.ofModule(moduleId).withType(AstModuleRelationshipType.ENTRY)).isEmpty() 
					? ResponseEntity.notFound() : ResponseEntity.noContent()).build();
		}
		return ResponseEntity.ok(astService.getControlFlow(moduleId, characterLimit));
	}
	
	/**
	 * Starts a {@code CalculateCFGJob} that calculates the Control Flow for a given module. 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param recalculateAst forces re-calculation of the AST before computing CFG
	 * @return the Id of the {@code CalculateCFGJob}
	 */
	@PostMapping(CONTROL_FLOW_SINGLE_URL)
	@Operation(summary = "Starts a job that calculates CFG for the given module Id and returns the job Id."
					+ " See the job status at '/v1/jobs/{jobId}/info'", operationId = "calculateControlFlowForModule")
	@ApiResponse(responseCode = "202", description = "the job has successfully been submitted")
	@ApiResponse(responseCode = "404", description = "if the given projectId or moduleId does not exist")
	@Role({EDITOR})
	@Nature({MINING})
	public char[] calculateControlFlowForModule(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@Parameter(description = "whether to force re-calculation of the AST before computing CFG", required = false, example = "true")
			@RequestParam(required = false, defaultValue = "false") final boolean recalculateAst) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		final EntityId completeModuleId = moduleService.getModuleEntityId(moduleId);
		if (recalculateAst) {
			dataFlowService.deleteForModule(moduleId);
			astService.delete(q -> q.ofModule(completeModuleId));
		}
		final CalculateCFGJob job = new CalculateCFGJob(projectId, completeModuleId);
		final JobMonitor jobMonitor = jobManager.submit(job);
		/* Explicitly returning the job Id as a char array, as a plain String would require a wrapper object for valid JSON. */
		return jobMonitor.getJobId().toCharArray();
	}
	
	/**
	 * Starts a {@code CalculateCFGJob} that calculates the Control Flow for a given list of module paths. 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleMatcher the module matcher of the modules to process
	 * @return the Id of the {@code CalculateCFGJob}
	 */
	@PostMapping(CONTROL_FLOW_MULTIPLE_URL)
	@Operation(summary = "Starts a job that calculates CFG for the given modules and returns the job Id."
					+ " See the job status at '/v1/jobs/{jobId}/info'", operationId = "calculateControlFlowGraphs")
	@ApiResponse(responseCode="202", description = "the job has successfully been submitted")
	@Role({EDITOR})
	@Nature({MINING})
	public char[] calculateControlFlowGraphs(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "list of id's and/or the file paths of the modules relative to the project "
					+ "example = \"{1}, {src/file.cbl}\"", required = true) 
			@RequestBody final ModuleMatcher moduleMatcher) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		final CalculateCFGJob job = new CalculateCFGJob(projectId, moduleMatcher);
		final JobMonitor jobMonitor = jobManager.submit(job);
		/* Explicitly returning the job Id as a char array, as a plain String would require a wrapper object for valid JSON. */
		return jobMonitor.getJobId().toCharArray();
	}
	
	@MiningUnsecuredRestEndpoint
	@GetMapping(CONTROL_FLOW_SUPPORT_URL)
	@Operation(summary = "Gets the module types supported by the control flow graph", operationId  = "getSupportedModuleTypes")
	public Set<NodeType> getSupportedModuleTypes() {
		return ControlFlowSupport.getActuallySupportedTypes();
	}
	
	@GetMapping(CONTROL_FLOW_SUPPORT_URL + "-theoretical")
	@Operation(summary = "Gets the module types theoretically supported by the control flow graph", operationId  = "getTheoreticallySupportedModuleTypes")
	@Role({VIEWER})
	@Nature({MINING})
	public List<NodeType> getTheoreticallySupportedModuleTypes() {
		return ControlFlowSupport.getTheoreticallySupportedTypes();
	}
	
}
