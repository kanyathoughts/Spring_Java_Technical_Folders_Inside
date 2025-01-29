/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job.reachabilityanalysis;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import org.springframework.beans.factory.annotation.Autowired;

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
import innowake.mining.server.functionalblocks.generation.reachabilityanalysis.ReachabilityBottomUpGeneration;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.job.JobSummary;
import innowake.mining.shared.model.job.Message;

/**
 * {@link Job} executing {@link ReachabilityBottomUpGeneration} on all modules matching a given Taxonomy and a list of
 * (currently hard-coded) types.
 */
public class ReachabilityBottomUpJob extends MiningJob<Serializable> {

	private static final String IDENTIFIED_MODULES_MESSAGE_PATTERN =
			"Executing bottom-up reachability generation on %d modules.";

	private static final String JOB_DESCRIPTION_PATTERN = "Reachability bottom-up generation on project %s";

	private final ReachabilityAnalysisConfig config;
	private final Set<EntityId> taxonomyIds;
	private final boolean recalculateAllModules;
	private final JobSummary jobSummary;
	@Autowired
	private transient FunctionalBlockService functionalBlockService;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient ProjectService projectService;

	/**
	 * Constructor.
	 *
	 * @param projectId the Id of the project
	 * @param config the configuration for the reachability analysis
	 * @param taxonomyIds the ids of the taxonomies to create the reachability analysis from
	 * @param recalculateAllModules flag to check if all the modules should be recalculated or only the missing ones.
	 */
	public ReachabilityBottomUpJob(final EntityId projectId,
								   final ReachabilityAnalysisConfig config,
								   final Set<EntityId> taxonomyIds,
								   final boolean recalculateAllModules) {
		super(projectId);
		this.config = config;
		this.taxonomyIds = taxonomyIds;
		this.recalculateAllModules = recalculateAllModules;
		this.jobSummary = new JobSummary();
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final String projectName = projectService.find(projectId).map(ProjectPojo::getName).orElseGet(() -> String.format(" with the ID '%s'", projectId));
		progressMonitor.setJobDescription(String.format(getJobDescriptionPattern(), projectName));

		final TaskProcessor<ReachabilityTaskResult> taskProcessor = new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));
		final Set<EntityId> executableModuleIdsSet = filterExecutableModuleIds();
		final Set<UUID> functionalBlockIds = new HashSet<>();

		progressMonitor.begin(executableModuleIdsSet.size());
		final Iterator<EntityId> iterator = executableModuleIdsSet.iterator();
		final TaskSource<ReachabilityTaskResult> taskSource = new TaskSource<>() {

			@Override
			public boolean hasNextTask() {
				return iterator.hasNext();
			}

			@Override
			public Task<ReachabilityTaskResult> nextTask() {
				final EntityId currentModuleId = iterator.next();
				return new ReachabilityBottomUpTask(progressMonitor.subMonitor(1), getJobId(), projectId, currentModuleId, config);
			}
		};

		final ResultConsumer<ReachabilityTaskResult> resultConsumer = new ResultConsumer<>(new ReportMessageExceptionHandler<>(assertNotNull(jobMonitor))) {
			@Override
			protected void handleResult(final String taskId, final Result<ReachabilityTaskResult> result) {
				if (result != null && result.value != null) {
					functionalBlockIds.addAll(result.value.getFunctionalBlockIds());
					jobSummary.aggregate(result.value.getTaskSummary());
				}
				progressMonitor.worked(1);
			}
		};

		forkTasks(taskProcessor, taskSource, resultConsumer);
		writeMessage(Message.Severity.INFO, String.format(getIdentifiedMessagePattern(), jobSummary.getSuccessfulModules().size()));
		writeMessage(Message.Severity.INFO, jobSummary.toString());

		/* run functional block computation after generating the bottom-up blocks */
		jobManager.submitFromJobAndWait(new FunctionalBlockComputationJob(functionalBlockIds), Objects.requireNonNull(jobMonitor));

		return new Result<>(new Status(resultConsumer.getHighestSeverity()));
	}

	private Set<EntityId> filterExecutableModuleIds() {
		final List<ModulePojo> modules = moduleService.findModules(q -> {
			q.ofProject(projectId).withTechnologiesAndTypes(
					config.getLowerBoundModuleTypes().parallelStream().map(t -> Tuple2.of(t.getTechnology(), t.getType())).collect(Collectors.toSet()));
			if (taxonomyIds != null && ! taxonomyIds.isEmpty()) {
				q.withTaxonomies(taxonomyIds);
			}
		});

		var stream = modules.stream();
		if ( ! recalculateAllModules) {
			stream = stream.filter(mod -> ! alreadyHasReachabilityFB(mod));
		}
		return stream.map(ModulePojo::identity)
				.collect(Collectors.toSet());
	}

	/**
	 * Checks if the module already has a functional block bottom up reachability block or not.
	 *
	 * @param module The module to check.
	 * @return true if reachability bottom up block exists, otherwise false.
	 */
	private boolean alreadyHasReachabilityFB(final ModulePojo module) {
		/* check if exists: MODULE block inside RA_LOWER_BOUND inside RA_BOTTOM_UP */
		return functionalBlockService.find(q -> q
						.ofProject(projectId)
						.generatedFromModule(module.getLinkHash())
						.withParent(p1 -> p1
								.withType(FunctionalBlockType.RA_LOWER_BOUND)
								.withParent(p2 -> p2
										.withType(FunctionalBlockType.RA_BOTTOM_UP))))
				.stream()
				.findAny()
				.isPresent();
	}

	private String getJobDescriptionPattern() {
		return JOB_DESCRIPTION_PATTERN;
	}

	private String getIdentifiedMessagePattern() {
		return IDENTIFIED_MODULES_MESSAGE_PATTERN;
	}

	@Override
	public String getJobName() {
		return "Reachability Bottom-Up Generation";
	}
}
