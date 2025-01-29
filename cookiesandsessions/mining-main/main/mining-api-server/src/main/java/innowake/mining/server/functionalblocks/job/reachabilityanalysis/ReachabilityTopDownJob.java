/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job.reachabilityanalysis;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.ReachabilityNetworkGeneration;
import innowake.mining.server.functionalblocks.generation.reachabilityanalysis.ReachabilityTopDownGeneration;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import innowake.mining.shared.model.job.JobSummary;
import innowake.mining.shared.model.job.Message;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * {@link Job} executing {@link ReachabilityTopDownGeneration} on all modules that match a given Taxonomy and are already the
 * {@link FunctionalBlockType#RA_LOWER_BOUND} of an existing {@link FunctionalBlockType#RA_BOTTOM_UP} block.
 */
public class ReachabilityTopDownJob extends MiningJob<Serializable> {

	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Executing top-down reachability generation on %d modules.";

	private static final String JOB_DESCRIPTION_PATTERN = "Reachability top-down generation on project %s";
	public static final String RECALCULATE_REACHABILITY_BLOCKS = "Recalculate Reachability Block(s)";
	public static final String REACHABILITY_TOP_DOWN_GENERATION = "Reachability Top-Down Generation";

	private final ReachabilityAnalysisConfig config;
	private final Set<EntityId> taxonomyIds;
	private final boolean recalculateAllModules;
	private final boolean fromBottomUp;
	private final Set<EntityId> reachabilityBlockIds;
	private final JobSummary jobSummary;

	@Autowired
	private transient FunctionalBlockService functionalBlockService;
	@Autowired
	private transient FunctionalBlockGenerationService generationService;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient ProjectService projectService;

	/**
	 * Constructor for Module types and Taxonomies.
	 * <p>
	 * When {@code fromBottomUp} is {@code true}, the Job expects that {@link ReachabilityBottomUpJob} has already been run on the same Taxonomies,
	 * so there exists {@link FunctionalBlockType#RA_BOTTOM_UP} functional blocks for the Taxonomy. The Top-Down analysis will then be executed on the
	 * upper bounds of the existing bottom-up blocks. The {@code moduleTypes} argument is ignored.
	 * <p>
	 * When {@code fromBottomUp} is {@code false}, the Job simply searches for Modules that match either one of the provided types
	 * or has one of the provided Taxonomies assigned. The Top-Down analysis will then be executed on each such module.
	 *
	 * @param projectId the Id of the project
	 * @param config the configuration for the reachability analysis
	 * @param taxonomyIds for determining start modules by Taxonomy
	 * @param fromBottomUp whether upper bounds should be identified from a previous bottom-up analysis
	 * @param recalculateAllModules whether to recalculate all modules
	 * @param reachabilityBlockIds the module ids to run the job on
	 */
	public ReachabilityTopDownJob(final EntityId projectId,
								  final ReachabilityAnalysisConfig config,
								  final Set<EntityId> taxonomyIds,
								  final Set<EntityId> reachabilityBlockIds,
								  final boolean recalculateAllModules,
								  final boolean fromBottomUp) {
		super(projectId);
		this.config = config;
		this.taxonomyIds = taxonomyIds;
		this.reachabilityBlockIds = reachabilityBlockIds;
		this.recalculateAllModules = recalculateAllModules;
		this.fromBottomUp = fromBottomUp;
		this.jobSummary = new JobSummary();
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final String projectName = projectService.find(projectId).map(ProjectPojo::getName).orElseGet(() -> String.format(" with the ID '%s'", projectId));
		progressMonitor.setJobDescription(String.format(getJobDescriptionPattern(), projectName));

		final TaskProcessor<ReachabilityTaskResult> taskProcessor = new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));

		final Set<EntityId> executableModuleIds = filterExecutableModuleIds();
		final Set<UUID> functionalBlockIds = new HashSet<>();

		progressMonitor.begin(executableModuleIds.size());
		final Iterator<EntityId> iterator = executableModuleIds.iterator();
		final TaskSource<ReachabilityTaskResult> taskSource = new TaskSource<>() {

			@Override
			public boolean hasNextTask() {
				return iterator.hasNext();
			}

			@Override
			public Task<ReachabilityTaskResult> nextTask() {
				final EntityId currentModuleId = iterator.next();
				return new ReachabilityTopDownTask(progressMonitor.subMonitor(1), getJobId(), projectId, currentModuleId, config);
			}
		};

		final ResultConsumer<ReachabilityTaskResult> resultConsumer = new ResultConsumer<>(new ReportMessageExceptionHandler<>(assertNotNull(jobMonitor))) {

			int resultCount = 1;
			@Override
			protected void handleResult(final String taskId, final Result<ReachabilityTaskResult> result) {
				if (result != null && result.value != null) {
					functionalBlockIds.addAll(result.value.getFunctionalBlockIds());
					jobSummary.aggregate(result.value.getTaskSummary());
				}
				progressMonitor.worked(1);
				ProgressMonitorThrottle.throttleStepDescription(
						String.format("Executing Tasks (%d/%d)", resultCount++, executableModuleIds.size()), progressMonitor);
			}
		};

		forkTasks(taskProcessor, taskSource, resultConsumer);
		writeMessage(Message.Severity.INFO, String.format(getIdentifiedMessagePattern(), jobSummary.getSuccessfulModules().size()));
		writeMessage(Message.Severity.INFO, jobSummary.toString());

		/* re-generate the Reachability Network block, adding any new RA_TOP_DOWN_BLOCKS to it */
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(projectId, getJobId(), progressMonitor);
		functionalBlockIds.addAll(assertNotNull(generationService).generate(ReachabilityNetworkGeneration.class, context, null).stream()
				.filter(op -> ! op.getKey().equals(FunctionalBlockGenerationResult.Operation.DELETE)).map(Pair::getValue).toList());
		functionalBlockIds.addAll(functionalBlockService.findUids(q -> q.ofProject(projectId)
						.withTypes(List.of(FunctionalBlockType.MERGE_PARENT, FunctionalBlockType.REACHABILITY))));

		/* run functional block computation after generating the top-Down blocks */
		jobManager.submitFromJobAndWait(new FunctionalBlockComputationJob(functionalBlockIds), Objects.requireNonNull(jobMonitor));
		return new Result<>(new Status(resultConsumer.getHighestSeverity()));
	}

	private Set<EntityId> filterExecutableModuleIds() {
		if (fromBottomUp) {
			return getStartModuleIdsByBoundedContext();
		} else if (reachabilityBlockIds != null && ! reachabilityBlockIds.isEmpty()) {
			return getStartModuleIdsFromReachabilityIdsAndTaxonomies();
		} else {
			return getStartModuleIdsFromConfigurationAndTaxonomies();
		}
	}

	private Set<EntityId> getStartModuleIdsFromReachabilityIdsAndTaxonomies() {
		final Set<UUID> reachabilityBlockUids = reachabilityBlockIds.stream().map(EntityId::getUid).collect(Collectors.toSet());
		final List<UUID> mergedReachabilityBlocks = functionalBlockService.findUids(q -> q
				.ofProject(projectId)
				.withType(FunctionalBlockType.MODULE)
				.withParent(p1 -> p1
						.withType(FunctionalBlockType.RA_UPPER_BOUND)
						.withParent(p2 -> p2
								.withType(FunctionalBlockType.RA_TOP_DOWN)
								.withFlag(FunctionalBlockFlag.DELETED, false)
								.withParent(p3 -> p3
										.byUids(reachabilityBlockUids)
										.withType(FunctionalBlockType.MERGE_PARENT)
								))));

		final List<UUID> upperBoundModuleBlocks = new ArrayList<>(functionalBlockService.findUids(q -> q
				.ofProject(projectId)
				.withType(FunctionalBlockType.MODULE)
				.withParent(p1 -> p1
						.withType(FunctionalBlockType.RA_UPPER_BOUND)
						.withParent(p2 -> p2
								.byUids(reachabilityBlockUids)
								.withType(FunctionalBlockType.RA_TOP_DOWN)
								.withFlag(FunctionalBlockFlag.DELETED, false)
						))));
		upperBoundModuleBlocks.addAll(mergedReachabilityBlocks);

		final Set<String> moduleLinkHashSet = functionalBlockService.getGeneratedFrom(upperBoundModuleBlocks).values().stream()
				.map(GeneratedFrom::getModuleLinkHash)
				.flatMap(Optional::stream)
				.collect(Collectors.toSet());

		return new HashSet<>(moduleService.findModuleIdsByLinkHash(q -> {
			q.ofProject(projectId).withLinkHashes(moduleLinkHashSet);
			if (taxonomyIds != null && ! taxonomyIds.isEmpty()) {
				q.withTaxonomies(taxonomyIds);
			}
		}).values());
	}

	private Set<EntityId> getStartModuleIdsByBoundedContext() {
		/* get all RA_UPPER_BOUND blocks that are inside an RA_BOTTOM_UP and has a RA_LOWER_BOUND referencing one of the selected taxonomies */
		final List<FunctionalBlockPojo> upperBoundBlocks = functionalBlockService.find(q -> q
				.ofProject(projectId)
				.withType(FunctionalBlockType.RA_UPPER_BOUND)
				.withParent(p -> p
						.withType(FunctionalBlockType.RA_BOTTOM_UP)
						.withChild(c -> {
							c.withType(FunctionalBlockType.RA_LOWER_BOUND);
							if (taxonomyIds != null && ! taxonomyIds.isEmpty()) {
								c.withResolvedModulePartHavingTaxonomies(taxonomyIds);
							}
						})));

		final Set<String> upperBoundModuleLinkHashes = functionalBlockService.getGeneratedFrom(upperBoundBlocks.stream()
						.flatMap(b -> b.getChildren().stream())
						.toList())
				.values()
				.stream()
				.map(GeneratedFrom::getModuleLinkHash)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.collect(Collectors.toSet());

		if ( ! recalculateAllModules) {
			/* select existing MODULE blocks that are generated from target module and are within RA_UPPER_BOUND block of an RA_TOP_DOWN block */
			final List<UUID> upperBoundModuleBlocks = functionalBlockService.findUids(q -> q
							.ofProject(projectId)
							.generatedFromModules(upperBoundModuleLinkHashes)
							.withParent(p1 -> p1
									.withType(FunctionalBlockType.RA_UPPER_BOUND)
									.withParent(p2 -> p2
											.withType(FunctionalBlockType.RA_TOP_DOWN))));

			/* remove all modules from target set that have such a MODULE block */
			functionalBlockService.getGeneratedFrom(upperBoundModuleBlocks).values().stream()
					.map(GeneratedFrom::getModuleLinkHash)
					.filter(Optional::isPresent)
					.map(Optional::get)
					.forEach(upperBoundModuleLinkHashes::remove);
		}

		return new HashSet<>(moduleService.findModuleIds(q -> q.ofProject(projectId).withLinkHashes(upperBoundModuleLinkHashes)));
	}

	private Set<EntityId> getStartModuleIdsFromConfigurationAndTaxonomies() {
		final Map<String, EntityId> startModules = new HashMap<>(moduleService.findModuleIdsByLinkHash(q -> {
			q.ofProject(projectId).withTechnologiesAndTypes(
					config.getUpperBoundModuleTypes().stream().map(t -> Tuple2.of(t.getTechnology(), t.getType())).collect(Collectors.toSet()));
			if ( taxonomyIds != null && ! taxonomyIds.isEmpty()) {
				q.withTaxonomies(taxonomyIds);
			}
		}));

		if ( ! recalculateAllModules) {
			/* select existing MODULE blocks that are generated from target module and are within RA_UPPER_BOUND block of an RA_TOP_DOWN block */
			final List<UUID> upperBoundModuleBlocks = functionalBlockService.findUids(q -> q
							.ofProject(projectId)
							.generatedFromModules(startModules.keySet())
							.withParent(p1 -> p1
									.withType(FunctionalBlockType.RA_UPPER_BOUND)
									.withParent(p2 -> p2
											.withType(FunctionalBlockType.RA_TOP_DOWN))));

			/* remove all modules from target set that have such a MODULE block */
			functionalBlockService.getGeneratedFrom(upperBoundModuleBlocks).values().stream()
					.map(GeneratedFrom::getModuleLinkHash)
					.filter(Optional::isPresent)
					.map(Optional::get)
					.forEach(startModules::remove);
		}
		return new HashSet<>(startModules.values());
	}

	private String getJobDescriptionPattern() {
		return JOB_DESCRIPTION_PATTERN;
	}

	private String getIdentifiedMessagePattern() {
		return IDENTIFIED_MODULES_MESSAGE_PATTERN;
	}

	@Override
	public String getJobName() {
		if(this.recalculateAllModules) {
			return RECALCULATE_REACHABILITY_BLOCKS;
		} else {
			return REACHABILITY_TOP_DOWN_GENERATION;
		}
	}
}
