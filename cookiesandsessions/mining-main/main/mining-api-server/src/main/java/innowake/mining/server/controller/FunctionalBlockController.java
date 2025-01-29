/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.server.functionalblocks.job.AutomaticGeneratedFunctionalBlockDeletionJob;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
import innowake.mining.server.functionalblocks.job.ModuleBlockGenerationJob;
import innowake.mining.server.functionalblocks.service.FunctionalBlockMergeService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockOperationsService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockToControlFlowGraphService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockToDependencyGraphService;
import innowake.mining.server.service.GenerativeReachabilityBlockDescriptionService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.functionalblocks.FunctionalBlockMergeRequest;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisRequest;
import innowake.mining.shared.model.functionalblocks.ReachabilityBlockGraphFilterRequest;
import innowake.mining.shared.model.functionalblocks.ReachabilityNetworkGraphFilterRequest;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import static innowake.mining.shared.entities.functionalblocks.FunctionalBlockType.FUNCTIONAL_GROUP;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * Controller for the {@link FunctionalBlockPojo} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class FunctionalBlockController extends BaseController {
	
	/**
	 * URL pattern for the functional-blocks collection by project ID.
	 */
	public static final String FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL = "/v1/projects/{projectId}/functional-blocks";
	
	/**
	 * URL pattern for the functional-blocks by uid.
	 */
	public static final String FUNCTIONAL_BLOCKS_BY_UID_URL = "/v1/projects/{projectId}/functional-blocks/{uid}";
	
	/**
	 * URL pattern for the compute functional-blocks.
	 */
	public static final String COMPUTE_FUNCTIONAL_BLOCKS_URL = "/v1/projects/{projectId}/functional-blocks/compute";
	
	/**
	 * URL pattern for the generating functional-blocks.
	 */
	public static final String FUNCTIONAL_BLOCK_GENERATION_MODULE_BLOCKS = "/v1/projects/{projectId}/functional-blocks/generate/module-blocks";

	/**
	 * URL pattern for the generating functional-block description.
	 */
	public static final String REACHABILITY_BLOCK_DESCRIPTION_GENERATION = "/v1/projects/{projectId}/functional-blocks/generate-reachability-description/{uid}";

	/**
	 * URL pattern for the un-merging functional-blocks.
	 */
	public static final String FUNCTIONAL_BLOCK_UNMERGE = "/v1/projects/{projectId}/functional-blocks/unmerge";
	/**
	 * URL pattern for the merging functional-blocks.
	 */
	public static final String FUNCTIONAL_BLOCK_MERGE = "/v1/projects/{projectId}/functional-blocks/merge";
	/**
	 * URL pattern for changing status of the functional-blocks by uid.
	 */
	public static final String FUNCTIONAL_BLOCK_STATUS_UPDATE = "/v1/projects/{projectId}/functional-blocks/status/{status}";

	public static final String FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE = "/v1/projects/{projectId}/functional-blocks/{uid}/block-graph";
	public static final String FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE = "/v1/projects/{projectId}/functional-blocks/{uid}/network-graph";
	public static final String FUNCTIONAL_BLOCK_CONTROL_FLOW_GRAPH_RETRIEVE = "/v1/projects/{projectId}/functional-blocks/{uid}/control-flow-graph";
	public static final String FUNCTIONAL_BLOCK_FUNCTIONAL_UNIT_CONTROL_FLOW_GRAPH_RETRIEVE =
			"/v1/projects/{projectId}/functional-blocks/{uid}/functional-unit-control-flow-graph";
	/**
	 * URL pattern for the functional-blocks by uid.
	 */
	public static final String AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL = "/v1/projects/{projectId}/functional-blocks/auto-generated-functional-blocks/{uid}";

	/**
	 * URL pattern for the recalculate outdated functional-blocks.
	 */
	public static final String RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL = "/v1/projects/{projectId}/functional-blocks/recalculate-outdated";
	/**
	 * URL pattern for the execute reachability Analysis on Modules.
	 */
	public static final String FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS = "/v1/projects/{projectId}/functional-blocks/generate/reachability-blocks";
	/**
	 * URL pattern for the remove functional-blocks without upper module.
	 */
	public static final String REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL = "/v1/projects/{projectId}/functional-blocks/remove-deleted";
	/**
	 * URL pattern for the generate functional blocks for business variables using data lineage.
	 */
	public static final String FUNCTIONAL_BLOCK_GENERATION_DATA_LINEAGE = "/v1/projects/{projectId}/functional-blocks/generate/data-lineage-functional-blocks";

	/**
	 * URL pattern for ungrouping functional blocks.
	 */
	public static final String UNGROUP_FUNCTIONAL_BLOCKS_URL = "/v1/projects/{projectId}/functional-blocks/{immediateParentUid}/ungroup/{selectedBlockUid}";

	private final FunctionalBlockService functionalBlockService;
	private final FunctionalBlockOperationsService functionalBlockOperationsService;
	private final GenerativeReachabilityBlockDescriptionService generativeReachabilityBlockDescriptionService;
	private final FunctionalBlockMergeService functionalBlockMergeService;
	private final FunctionalBlockToDependencyGraphService functionalBlockToDependencyGraphService;
	private final FunctionalBlockToControlFlowGraphService functionalBlockToControlFlowGraphService;
	
	@Autowired
	private JobManager jobManager;

	public FunctionalBlockController(final FunctionalBlockService functionalBlockService,
									 final FunctionalBlockOperationsService functionalBlockOperationsService,
									 final FunctionalBlockMergeService functionalBlockMergeService,
									 final GenerativeReachabilityBlockDescriptionService generativeReachabilityBlockDescriptionService,
									 final FunctionalBlockToDependencyGraphService functionalBlockToDependencyGraphService,
									 final FunctionalBlockToControlFlowGraphService functionalBlockToControlFlowGraphService) {
		this.functionalBlockService = functionalBlockService;
		this.functionalBlockOperationsService = functionalBlockOperationsService;
		this.functionalBlockMergeService = functionalBlockMergeService;
		this.generativeReachabilityBlockDescriptionService = generativeReachabilityBlockDescriptionService;
		this.functionalBlockToDependencyGraphService = functionalBlockToDependencyGraphService;
		this.functionalBlockToControlFlowGraphService = functionalBlockToControlFlowGraphService;
	}

	/**
	 * Creates a new {@link FunctionalBlockPojo}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to create
	 * @param functionalBlockPojoPrototype the {@link FunctionalBlockPojoPrototype} to create 
	 * @return the new {@link FunctionalBlockPojo}
	 */
	@PostMapping(FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL)
	@Operation(summary = "Create a functional block", operationId = "createFunctionalBlock")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given functionalBlockPojoPrototype is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public FunctionalBlockPojo create(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The functional block to create", required = true) 
			@RequestBody final FunctionalBlockPojoPrototype functionalBlockPojoPrototype) {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());
		projectId.matchOrApply(functionalBlockPojoPrototype.project.isDefined() ? functionalBlockPojoPrototype.project.getNonNull() :
			EntityId.VOID, functionalBlockPojoPrototype::setProject);

		validateChildren(functionalBlockPojoPrototype);

		final UUID uuid = functionalBlockService.create(functionalBlockPojoPrototype);
		return functionalBlockService.find(uuid).orElseThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, uuid.toString()));
	}
	
	/**
	 * Updates an existing {@link FunctionalBlockPojo}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param uid id of the functional block
	 * @param functionalBlockPojoPrototype the {@link FunctionalBlockPojoPrototype} to update 
	 * @return the updated {@link UUID}
	 * @throws IOException if an error occurs if uid does not match
	 */
	@PutMapping(FUNCTIONAL_BLOCKS_BY_UID_URL)
	@Operation(summary = "Update a functional block", operationId = "updateFunctionalBlock")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given uid is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public FunctionalBlockPojo update(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the uid", required = true, example = "0")
			@PathVariable final UUID uid,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The functional block to update", required = true) 
			@RequestBody final FunctionalBlockPojoPrototype functionalBlockPojoPrototype) throws IOException {

		validate(request, "uid");
		projectId.matchOrApply(functionalBlockPojoPrototype.project.isDefined() ? functionalBlockPojoPrototype.project.getNonNull() :
				EntityId.VOID, functionalBlockPojoPrototype::setProject);

		if (functionalBlockPojoPrototype.uid.isDefined() && ! functionalBlockPojoPrototype.uid.getNonNull().equals(uid)) {
			response.sendError(HttpStatus.BAD_REQUEST.value(), "Bad Request: uid provided in the path and request body does not match");
		} else {
			functionalBlockPojoPrototype.setUid(uid);
		}

		if (functionalBlockPojoPrototype.children.isDefined()) {
			validateChildren(functionalBlockPojoPrototype);
			handleFunctionalGroups(uid);

		}

		functionalBlockService.update(functionalBlockPojoPrototype);
		return functionalBlockService.find(uid).orElseThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, uid.toString()));
	}
	
	/**
	 * Deletes selected block and before deleting selected block, it adds children of selected block to immediate parent
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to delete
	 * @param immediateParentUid id of the functional block to be updated with children
	 * @param selectedBlockUid id of the functional block to be deleted
	 */
	@DeleteMapping(UNGROUP_FUNCTIONAL_BLOCKS_URL)
	@Operation(summary = "Before deleting selected block it adds children of selected block to immediate parent", operationId = "deleteFbOnUnGroup")
	@ApiResponse(responseCode = "204", description = "if the selected functional block was successfully deleted")
	@ApiResponse(responseCode = "404", description = "if the project or given functional blocks does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void deleteFbOnUnGroup(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the immediateParent uid", required = true, example = "0")
			@PathVariable final UUID immediateParentUid,
			@Parameter(description = "the selectedBlock uid", required = true, example = "0")
			@PathVariable final UUID selectedBlockUid) {
		validate(request, "immediateParentUid", "selectedBlockUid");
		response.setStatus(HttpStatus.NO_CONTENT.value());
		final List<UUID> updateChildren = new ArrayList<>();
		updateChildren.addAll(functionalBlockService.find(immediateParentUid).orElseThrow(() ->
				new MiningEntityNotFoundException(FunctionalBlockPojo.class, immediateParentUid.toString())).getChildren());
		updateChildren.addAll(functionalBlockService.find(selectedBlockUid).orElseThrow(() ->
				new MiningEntityNotFoundException(FunctionalBlockPojo.class, selectedBlockUid.toString())).getChildren());
		functionalBlockService.update(new FunctionalBlockPojoPrototype().setProject(projectId).setUid(immediateParentUid).setChildren(updateChildren));
		functionalBlockService.delete(selectedBlockUid);
	}

	/**
	 * Delete an existing {@link FunctionalBlockPojo}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to delete
	 * @param uid id of the functional block to delete
	 */
	@DeleteMapping(FUNCTIONAL_BLOCKS_BY_UID_URL)
	@Operation(summary = "Delete a functional block", operationId = "deleteFunctionalBlock")
	@ApiResponse(responseCode = "204", description = "if the functional block was successfully deleted")
	@ApiResponse(responseCode = "404", description = "if the project or functional block does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void delete(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the functional block to be deleted", required = true, example = "0")
			@PathVariable final UUID uid) {
		validate(request, "uid");
		response.setStatus(HttpStatus.NO_CONTENT.value());
		functionalBlockService.delete(uid);
	}

	/**
	 * Deletes a {@link FunctionalBlockPojo} and its children of type "FUNCTIONAL_CONDITION".
	 * and Functional Units of type "FUNCTIONAL" that are not children of any other Functional Block.While deleting the Functional unit,
	 * it also deletes the associated Annotations.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to delete
	 * @param uid id of the functional block to delete
	 * @return the job ID of the delete job
	 */
	@DeleteMapping(AUTOMATED_FUNCTIONAL_BLOCKS_BY_UID_URL)
	@Operation(summary = "Starts a job that deletes the functional block and and its children of type Functional_Condition and Functional Units of type Functional " +
			"that are not children of any other Functional Block and returns the job Id See the job status at '/v1/jobs/{jobId}/info",
			operationId = "deleteAutomatedFunctionalBlock")
	@ApiResponse(responseCode = "200", description = "the job has successfully been submitted")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] deleteAutomatedFunctionalBlock(final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the functional block to be deleted", required = true, example = "0")
			@PathVariable final UUID uid) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		final AutomaticGeneratedFunctionalBlockDeletionJob job = new AutomaticGeneratedFunctionalBlockDeletionJob(projectId, uid);
		return jobManager.submit(job).getJobId().toCharArray();
	}
	
	/**
	 * Computes functional blocks.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param functionalBlockIds the ids of the functional blocks
	 * @return the job id of the {@link FunctionalBlockComputationJob}
	 */
	@PostMapping(COMPUTE_FUNCTIONAL_BLOCKS_URL)
	@Operation(summary = "Computes functional blocks", operationId = "computeFunctionalBlock")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given functionalBlockIds are not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] compute(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The functional block to create", required = true) 
			@RequestBody final Set<UUID> functionalBlockIds) {
		validate(request);
		return functionalBlockOperationsService.executeFunctionalBlockComputation(functionalBlockIds).toCharArray();
	}

	/**
	 * Generates functional blocks.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param moduleMatcher list of id's of the modules to generate functional blocks for
	 * @return The job id of the {@link ModuleBlockGenerationJob}
	 */
	@PostMapping(FUNCTIONAL_BLOCK_GENERATION_MODULE_BLOCKS)
	@Operation(summary = "Starts a job that generates functional block for the given modules and returns the job Id."
			+ " See the job status at '/v1/jobs/{jobId}/info'")
	@ApiResponse(responseCode = "202", description = "the job has successfully been submitted")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] functionalBlockGeneration(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "list of id's of the module to generate functional blocks"
					+ "for example = \"{1}\"", required = true) @RequestBody final ModuleMatcher moduleMatcher) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		return functionalBlockOperationsService.executeModuleBlockCreation(projectId, moduleMatcher).toCharArray();
	}

	/**
	 * Generates description for a reachability block using generative AI.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the reachability block
	 * @param uid the id of the reachability block
	 * @param generateModuleDescriptions whether descriptions of modules in the functional block should be generated automatically
	 * @return The AI generated description
	 */
	@PostMapping(REACHABILITY_BLOCK_DESCRIPTION_GENERATION)
	@Operation(summary = "Generates description a for reachability block using generative AI.")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "400", description = "on failure")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] generateReachabilityBlockDescription(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the UID of the functional block", required = true, example = "0") @PathVariable final UUID uid,
			@Parameter(description = "whether descriptions of modules in the functional block should be generated automatically", required = false,
					example = "true") @RequestParam(required = false) final boolean generateModuleDescriptions) {
		validate(request);
		final var description = generativeReachabilityBlockDescriptionService.generateDescription(projectId, uid, generateModuleDescriptions);
		return description.toCharArray();
	}

	/**
	 * Un-Merges the {@linkplain FunctionalBlockPojo Functional Blocks}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the Merge Block
	 * @param mergeBlock the Merge Blocks
	 */
	@PostMapping(FUNCTIONAL_BLOCK_UNMERGE)
	@Operation(summary = "Unmerge functional block", operationId = "unmergeFunctionalBlock")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void unmergeFunctionalBlock(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "FunctionalBlockMergeRequest Pojo for Unmerging", required = true)
			@RequestBody final FunctionalBlockMergeRequest mergeBlock) {
		validate(request);
		if (mergeBlock.getMergeParent() == null || mergeBlock.getRemoveEmptyBlocks() == null
				|| mergeBlock.getCommonParent() == null || mergeBlock.getMergeChildren() == null) {
			throw new IllegalArgumentException("All properties in FunctionalBlockMergeRequest except for mergeParentProtoType must be non-null");
		}
		functionalBlockMergeService.unmerge(mergeBlock.getCommonParent(),
				mergeBlock.getMergeParent(), mergeBlock.getMergeChildren(), mergeBlock.getRemoveEmptyBlocks());

		/* The merged block would have stale reachability data until computation is done */
		functionalBlockOperationsService.executeFunctionalBlockComputation(Collections.singleton(mergeBlock.getMergeParent()));
	}

	/**
	 * Merges the {@linkplain FunctionalBlockPojo Functional Blocks}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the Merge Block
	 * @param mergeBlock the Merge Block
	 */
	@PostMapping(FUNCTIONAL_BLOCK_MERGE)
	@Operation(summary = "merge functional block", operationId = "mergeFunctionalBlock")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void mergeFunctionalBlock(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "FunctionalBlockMergeRequest Block Pojo for merging", required = true)
			@RequestBody final FunctionalBlockMergeRequest mergeBlock) {
		validate(request);
		if ((mergeBlock.getMergeParentPrototype() == null && mergeBlock.getMergeParent() == null) || mergeBlock.getRemoveEmptyBlocks() == null
				|| mergeBlock.getCommonParent() == null || mergeBlock.getMergeChildren() == null) {
			throw new IllegalArgumentException("Either mergeParent or mergeParentPrototype must be set along with remaining properties in "
					+ "FunctionalBlockMergeRequest");
		}
		final UUID mergeParentUID = functionalBlockMergeService.merge(mergeBlock.getCommonParent(), mergeBlock.getMergeParent(),
				mergeBlock.getMergeParentPrototype(), mergeBlock.getMergeChildren(), mergeBlock.getRemoveEmptyBlocks());

		/* The merged block would have any data until computation is ran hence we run it here to populate table view data for the merged block */
		functionalBlockOperationsService.executeFunctionalBlockComputation(Collections.singleton(mergeParentUID));
	}

	/**
	 * Updates the status of the functional blocks.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param status the status of the functional block
	 * @param functionalBlockUids the ids of the functional blocks
	 */
	@PutMapping(FUNCTIONAL_BLOCK_STATUS_UPDATE)
	@Operation(summary = "change status functional block", operationId = "updateBlockStatus")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project/functional block does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void updateBlockStatus(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the status of the functional block to be updated to", required = true, example = "0")
			@PathVariable final FunctionalBlockStatus status,
			@Parameter(description = "the ID of the functional blocks to be updated", required = true)
			@RequestBody final List<UUID> functionalBlockUids) {
		validate(request,"status");
		functionalBlockService.updateBlocksStatus(functionalBlockUids, status);
	}

	/**
	 * Converts the child blocks and links found on a FunctionalBlockPojo into a DependencyGraph object.
	 * This is useful (only) when each child block represents an entire module or can be represented by a module.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param uid the id of the functional block
	 * @param dependencyGraphFilterRequest the filter request for the dependency graph
	 * @return DependencyGraph object for Links Found on FunctionalBlockPojo.
	 */
	@PostMapping(FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE)
	@Operation(summary = "convert the given functional block (given by uid parameter) into a dependency graph.")
	@ApiResponse(responseCode = "200", description = "the conversion was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public DependencyGraph getFunctionalBlockAsDependencyGraph(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the uid of the block", required = true)
			@PathVariable final UUID uid,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "DependencyGraphFilterRequest to filter the dependency graph")
			@RequestBody(required = false) @Nullable final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest) {

		validate(request);
		final Optional<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(uid);
		return functionalBlockPojo.map(blockPojo -> functionalBlockToDependencyGraphService.toFunctionalBlockGraph(blockPojo, dependencyGraphFilterRequest))
				.orElse(null);
	}

	/**
	 * Converts the child blocks and links found on a reachability network FunctionalBlockPojo into a DependencyGraph object.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param uid the id of the functional block
	 * @param filterRequest the filter request for the dependency graph
	 * @return DependencyGraph object for Links Found on FunctionalBlockPojo.
	 */
	@PostMapping(FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE)
	@Operation(summary = "convert the given reachability network block (given by uid parameter) into a dependency graph.")
	@ApiResponse(responseCode = "200", description = "the conversion was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public DependencyGraph getReachabilityNetworkGraph(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the uid of the block", required = true)
			@PathVariable final UUID uid,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "Filter Object to filter the network graph")
			@RequestBody final ReachabilityNetworkGraphFilterRequest filterRequest) {
		validate(request);
		return functionalBlockToDependencyGraphService.toReachabilityNetworkGraph(uid, filterRequest)
				.orElse(null);
	}

	/**
	 * Converts the child blocks and links found on a FunctionalBlockPojo into a ControlFlowGraph object.
	 * This is useful (only) when each child block represents a code location within the same module.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param uid the id of the functional block
	 * @return ControlFlowGraph object representing child blocks
	 */
	@GetMapping(FUNCTIONAL_BLOCK_CONTROL_FLOW_GRAPH_RETRIEVE)
	@Operation(summary = "convert the given functional block (given by uid parameter) into a control flow graph.")
	@ApiResponse(responseCode = "200", description = "the conversion was successful")
	@ApiResponse(responseCode = "400", description = "if the given functionalBlockId is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public ControlFlowGraph getFunctionalBlockAsControlFlowGraph(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the uid of the block", required = true)
			@PathVariable final UUID uid) {

		validate(request);
		final Optional<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(uid);
		return functionalBlockPojo
				.map(functionalBlockToControlFlowGraphService::toControlFlowGraph)
				.orElse(null);

	}

	/**
	 * Converts a Functional Block into a ControlFlowGraph object by selecting all child blocks of type
	 * {@link FunctionalBlockType#FUNCTIONAL_UNIT}. The resulting graph will only contain the statements represented by those FUNCTIONAL_UNITs
	 * and any conditions that are on a path from the entry point of the module to one of the statements.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param uid the id of the functional block
	 * @return ControlFlowGraph object representing FUNCTIONAL_UNITs and associated conditions
	 */
	@GetMapping(FUNCTIONAL_BLOCK_FUNCTIONAL_UNIT_CONTROL_FLOW_GRAPH_RETRIEVE)
	@Operation(summary = "creates a control flow graph using the functional units contained in the given block")
	@ApiResponse(responseCode = "200", description = "the conversion was successful")
	@ApiResponse(responseCode = "400", description = "if the given uid is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public ControlFlowGraph getControlFlowGraphForFunctionalUnits(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the uid of the block", required = true)
			@PathVariable final UUID uid) {

		validate(request);
		final Optional<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(uid);
		return functionalBlockPojo
				.map(functionalBlockToControlFlowGraphService::toControlFlowGraphOfFunctionalUnits)
				.orElse(null);

	}

	/**
	 * Recalculates outdated functional blocks.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @return the job id of the {@link FunctionalBlockComputationJob}
	 */
	@PutMapping(RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL)
	@Operation(summary = "Recalculates outdated functional blocks", operationId = "recalculateOutDatedFunctionalBlocks")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] recalculateOutDatedFunctionalBlocks(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return functionalBlockOperationsService.executeOutdatedFunctionalBlockComputation(projectId).toCharArray();
	}

	/**
	 * execute reachability Analysis on Blocks.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param reachabilityAnalysisRequest the {@link ReachabilityAnalysisRequest} to calculate reachability blocks
	 * @return the job id of the {@link FunctionalBlockComputationJob}
	 */
	@PostMapping(FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS)
	@Operation(summary = "execute reachability Analysis on Blocks", operationId = "executeReachabilityAnalysis")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] executeReachabilityAnalysis(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "ReachabilityBlockComputationRequest to compute reachability blocks", required = true)
			@RequestBody final ReachabilityAnalysisRequest reachabilityAnalysisRequest) {
		validate(request);
		return functionalBlockOperationsService.executeReachabilityAnalysisOnModules(projectId, reachabilityAnalysisRequest).toCharArray();
	}

	/**
	 * Generate Functional block for business variables using data lineage.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param dataDictionaryUids the ids of the data dictionary
	 * @return the job id of the {@link FunctionalBlockComputationJob}
	 */
	@PostMapping(FUNCTIONAL_BLOCK_GENERATION_DATA_LINEAGE)
	@Operation(summary = "Generates functional blocks for business variables using data lineage", operationId = "generateFunctionalBlocksUsingDataLineage")
	@ApiResponse(responseCode = "202", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given functionalBlockIds are not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] generateFunctionalBlocksUsingDataLineage(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The module id", required = true)
			@RequestBody final List<EntityId> dataDictionaryUids) {
		validate(request);
		return functionalBlockOperationsService.executeDataLineageFunctionalBlockGeneration(projectId, dataDictionaryUids).toCharArray();
	}

	/**
	 * Removes Reachability blocks Having No UpperBound Module.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @return the job id of the {@link FunctionalBlockComputationJob}
	 */
	@DeleteMapping(REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL)
	@Operation(summary = "Removes functional blocks Having No UpperBound Module", operationId = "removeFunctionalBlocksWithoutUpperBoundModule")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public char[] removeFunctionalBlocksWithoutUpperBoundModule(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return functionalBlockOperationsService.executeFunctionalBlockComputation(
				functionalBlockOperationsService.removeFunctionalBlocksWithoutUpperModule(projectId, Collections.emptySet())).toCharArray();
	}

	private void validateChildren(final FunctionalBlockPojoPrototype functionalBlock) {
		if (functionalBlock.children.isDefined()) {
			final List<FunctionalBlockPojo> childBlocks = functionalBlockService.get(functionalBlock.children.getNonNull());
				validateProjectIdEqualityInChildBlocks(functionalBlock.project.getNonNull(), childBlocks);
		}
	}

	private void validateProjectIdEqualityInChildBlocks(final EntityId entityId, final List<FunctionalBlockPojo> childBlocks) {
		for (final FunctionalBlockPojo childBlock : childBlocks) {
			if ( ! childBlock.getProject().equals(entityId)) {
				throw new UserFacingException(HttpStatus.BAD_REQUEST,
						"Children Functional Block must be present in the same project as the parent Functional Block");
			}
		}
	}

	private void handleFunctionalGroups(final UUID uid) {
		final Optional<FunctionalBlockPojo> functionalBlockBeforeUpdate = functionalBlockService.find(uid);
		if (functionalBlockBeforeUpdate.isPresent() && FunctionalBlockUtil.hasType(functionalBlockBeforeUpdate.get(), FUNCTIONAL_GROUP)) {
			functionalBlockService.deleteLinks(uid);
			functionalBlockService.deleteConditionsAndStatementTypeChildren(uid);
		}
	}
}
