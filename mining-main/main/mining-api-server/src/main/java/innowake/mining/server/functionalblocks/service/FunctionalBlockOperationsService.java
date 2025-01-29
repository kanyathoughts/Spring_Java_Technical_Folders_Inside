/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import brave.Tracer;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
import innowake.mining.server.functionalblocks.job.ModuleBlockGenerationJob;
import innowake.mining.server.functionalblocks.job.datalineagefunctionalblock.DataLineageFunctionalBlockJob;
import innowake.mining.server.functionalblocks.job.reachabilityanalysis.ReachabilityBottomUpJob;
import innowake.mining.server.functionalblocks.job.reachabilityanalysis.ReachabilityTopDownJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisRequest;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Service for triggering (bulk) operations on Functional Blocks.
 */
@Service
public class FunctionalBlockOperationsService {

	private static final Logger LOG = LoggerFactory.getLogger(FunctionalBlockOperationsService.class);
	private final JobManager jobManager;
	private final FunctionalBlockService functionalBlockService;
	private final ProjectService projectService;
	private final Tracer tracer;
	private final FunctionalBlockMergeService functionalBlockMergeService;

	public FunctionalBlockOperationsService(final JobManager jobManager, final FunctionalBlockService functionalBlockService,
			final ProjectService projectService, final Tracer tracer,
			final FunctionalBlockMergeService functionalBlockMergeService) {
		this.jobManager = jobManager;
		this.functionalBlockService = functionalBlockService;
		this.projectService = projectService;
		this.tracer = tracer;
		this.functionalBlockMergeService = functionalBlockMergeService;
	}

	/**
	 * Executes {@link innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob} on the given functional blocks
	 * @param functionalBlockIds the functional block ids to run on
	 * @return the id of the computation job
	 */
	@WithSystemUser
	public String executeFunctionalBlockComputation(final Set<UUID> functionalBlockIds) {
		return jobManager.submit(new FunctionalBlockComputationJob(functionalBlockIds)).getJobId();
	}

	@WithSystemUser
	public String executeFunctionalBlockComputation(final Collection<BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder>> filters) {
		final Set<UUID> uids = new HashSet<>();
		for (final BuildingConsumer<FunctionalBlockService.FunctionalBlockInquiryBuilder> filter : filters) {
			uids.addAll(functionalBlockService.findUids(filter));
		}
		return executeFunctionalBlockComputation(uids);
	}

	/**
	 * Executes {@link ModuleBlockGenerationJob} in bulk
	 * @param projectId the Id of the project
	 * @param moduleMatcher the module matcher of all modules to handle
	 * @return the id of the generation job
	 */
	public String executeModuleBlockCreation(final EntityId projectId, final ModuleMatcher moduleMatcher) {
		return jobManager.submit(new ModuleBlockGenerationJob(projectId, moduleMatcher)).getJobId();
	}

	/**
	 * Executes {@link DataLineageFunctionalBlockJob}
	 * @param projectId the Id of the project
	 * @param dataDictionaryUids the uids of the data dictionaries to use
	 * @return the id of the generation job
	 */
	public String executeDataLineageFunctionalBlockGeneration(final EntityId projectId, final List<EntityId> dataDictionaryUids) {
		return jobManager.submit(new DataLineageFunctionalBlockJob(projectId, dataDictionaryUids)).getJobId();
	}

	@WithSystemUser
	public String executeOutdatedFunctionalBlockComputation(final EntityId project) {
		final Set<EntityId> upperBoundFunctionalBlockIds = functionalBlockService.findUids(q -> q.ofProject(project)
						.withFlag(FunctionalBlockFlag.OUTDATED, true)
						.withFlag(FunctionalBlockFlag.DELETED, false)
						.withType(FunctionalBlockType.REACHABILITY)).parallelStream().map(EntityId::of).collect(Collectors.toSet());
		final ReachabilityAnalysisConfig config = projectService.getConfigByName(project, ReachabilityAnalysisConfig.class,
				ReachabilityAnalysisConfig.CONFIG_NAME).orElse(ReachabilityAnalysisConfig.defaultConfig());
		return jobManager.submit(new ReachabilityTopDownJob(project, config, Set.of(), upperBoundFunctionalBlockIds, false, false)).getJobId();
	}

	@WithSystemUser
	public String executeFunctionalBlockComputation(final Set<UUID> functionalBlockIds, final JobExecutionCallback callback) {
		return jobManager.submit(new FunctionalBlockComputationJob(functionalBlockIds), callback).getJobId();
	}

	/**
	 * Executes reachability analysis on the given modules.
	 * @param projectId the project id
	 * @param computationRequest the computation request
	 * @return the id of the computation job
	 */
	public String executeReachabilityAnalysisOnModules(final EntityId projectId, final ReachabilityAnalysisRequest computationRequest) {
		final ReachabilityAnalysisConfig config = projectService.getConfigByName(projectId, ReachabilityAnalysisConfig.class,
				ReachabilityAnalysisConfig.CONFIG_NAME).orElse(ReachabilityAnalysisConfig.defaultConfig());

		if (computationRequest.getAnalysisType() == ReachabilityAnalysisRequest.AnalysisType.BOTTOM_UP) {
			return executeReachabilityBottomUpAnalysis(projectId, computationRequest, config);
		} else if (computationRequest.getAnalysisType() == ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN) {
			return executeReachabilityTopDownAnalysis(projectId, computationRequest, config);
		} else {
			throw new IllegalArgumentException("Invalid ReachabilityAnalysisRequest, Analysis type must be set.");
		}
	}

	/**
	 * Removes the functional blocks without upper module.
	 * @param projectId the project id
	 * @param functionalBlockUids the functional block uids
	 * @return the updated merge blocks
	 */
	public Set<UUID> removeFunctionalBlocksWithoutUpperModule(final EntityId projectId, final Set<UUID> functionalBlockUids) {
		final Set<UUID> deletedStandAloneFunctionalBlocksAndChildren = functionalBlockService.find(q -> {
					q.ofProject(projectId);
					if (functionalBlockUids != null && ! functionalBlockUids.isEmpty()) {
						q.byUids(functionalBlockUids);
					}
					q.withFlag(FunctionalBlockFlag.DELETED, true)
							.withType(FunctionalBlockType.REACHABILITY)
							.notWithType(FunctionalBlockType.MERGE_PARENT);
				})
				.parallelStream()
				.flatMap(fb -> Stream.concat(Stream.of(fb.getUid()), fb.getChildren().stream()))
				.collect(Collectors.toSet());

		final List<FunctionalBlockPojo> mergedBlocksWithDeletedChildren = functionalBlockService.find(q -> {
			q.ofProject(projectId);
			if (functionalBlockUids != null && ! functionalBlockUids.isEmpty()) {
				q.byUids(functionalBlockUids);
			}
			q.withType(FunctionalBlockType.MERGE_PARENT)
					.withChild(c -> c.withFlag(FunctionalBlockFlag.DELETED, true));
		});

		final Set<UUID> updatedMergeBlocks = new HashSet<>();
		if ( ! mergedBlocksWithDeletedChildren.isEmpty()) {
			final UUID commonParent = functionalBlockService.findUids(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY_NETWORK))
					.stream().findFirst().orElseThrow(() -> new IllegalStateException("No REACHABILITY_NETWORK block found in the project " + projectId));
			mergedBlocksWithDeletedChildren.forEach(block -> {
				final List<UUID> mergeBlockChildren = block.getChildren();
				final List<UUID> deletedChildren = functionalBlockService.findUids(
						q -> q.byUids(mergeBlockChildren).withFlag(FunctionalBlockFlag.DELETED, true));

				if ( ! deletedChildren.isEmpty()) {
					final UUID uid = block.getUid();
					if (mergeBlockChildren.size() - deletedChildren.size() <= 1) {
						/* Unmerge and recompute when non-deleted children is 0 or 1. */
						functionalBlockMergeService.unmerge(commonParent, uid, mergeBlockChildren, true);
					} else {
						/* For non-deleted children more than one we need to just re-computed and no need to unmerge as the deleted block would not be fetched
						 * in the merge parent.*/
						updatedMergeBlocks.add(uid);
					}
					deletedStandAloneFunctionalBlocksAndChildren.addAll(deletedChildren);
				}
			});
		}
		functionalBlockService.delete(deletedStandAloneFunctionalBlocksAndChildren);
		return updatedMergeBlocks;
	}

	private String executeReachabilityBottomUpAnalysis(final EntityId projectId, final ReachabilityAnalysisRequest request,
			final ReachabilityAnalysisConfig config) {
		final Set<EntityId> taxonomies = request.getModuleTaxonomies();
		final boolean recalculateAll = request.getRecalculateAll();
		final SecurityContext currentSecurityContext = SecurityContextHolder.getContext();
		return jobManager.submit(new ReachabilityBottomUpJob(projectId, config, taxonomies, recalculateAll), new JobExecutionCallback() {

			@Override
			public void onCompletion() {
				try (final Tracer.SpanInScope parentScope = tracer.withSpanInScope(tracer.newTrace())) {
					SecurityContextHolder.setContext(currentSecurityContext);
					jobManager.submit(new ReachabilityTopDownJob(projectId, config, taxonomies, Collections.emptySet(), recalculateAll, true)).getJobId();
				}
			}

			@Override
			public void onFailure(final Throwable throwable) {
				LOG.error("Failed to execute Reachability Top Down Analysis computation after Bottom Up Computation", throwable);
			}
		}).getJobId();
	}

	private String executeReachabilityTopDownAnalysis(final EntityId projectId, final ReachabilityAnalysisRequest request,
			final ReachabilityAnalysisConfig config) {
		final Set<EntityId> functionalBlockUids = request.getFunctionalBlockUids();
		final boolean recalculateAll = request.getRecalculateAll();

		if (recalculateAll) {
			removeFunctionalBlocksWithoutUpperModule(projectId, functionalBlockUids.parallelStream().map(EntityId::getUid).collect(Collectors.toSet()));
		}

		return jobManager.submit(new ReachabilityTopDownJob(projectId, config, request.getModuleTaxonomies(), functionalBlockUids, recalculateAll, false)).getJobId();
	}
}
