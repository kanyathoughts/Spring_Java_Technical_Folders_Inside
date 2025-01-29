/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpEntity;

import brave.Span;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.extensions.metadata.MetaDataExportExtension;
import innowake.mining.extensions.metadata.MetaDataExportJob;
import innowake.mining.extensions.metadata.MetaDataImportExtension;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.dawn.metrics.api.service.DiscoveryService;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectAliasManager;
import innowake.mining.server.discovery.monitor.ProgressMonitorText;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.job.deadcode.IdentifyDeadCodeJob;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EffortSummaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.EffortSummaryService.TypeSummaryProperties;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.entities.EffortSummaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.EffortSummaryType;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Job for executing Discover Metrics
 */
public class DiscoverMetricsJob extends MiningJob<Serializable> implements TaskHandler {
	
	public static final String JOB_PREFIX = ProgressMonitorText.DISCOVERY_METRICS_TASK_DISCOVERMETRICS;
	protected static final Logger LOG = LoggerFactory.getLogger(innowake.mining.server.discovery.Logging.METRICS);

	@Autowired
	protected transient DiscoveryJobCache discoveryCache;
	@Autowired
	protected transient ProjectService projectService;
	@Autowired
	protected transient EffortSummaryService effortSummaryService;
	@Autowired
	protected transient BuildProperties buildProperties;
	@Autowired
	protected transient NaturalSourceObjectAliasManager aliasManager;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient JobConfigurationProperties jobConfig;
	@Autowired
	private transient MetaDataExportExtension exportExtension;
	@Autowired
	private transient MetaDataImportExtension importExtension;
	@Autowired
	private transient SourceCachingService sourceService;
	@Autowired
	private transient PersistMetricsModelService persistMetricsModelService;
	@Autowired
	private transient FF4j ff4j;
	@Autowired
	private transient MetricsContributorProvider contributorProvider;

	@Nullable
	private transient ProjectPojo project;
	@Nullable
	private Instant oldMetricsDate;
	@Nullable
	protected transient TaskProcessor<?> taskProcessor;
	@Nullable
	private transient MetricsCollector metricsCollector;

	@Autowired
	private transient DiscoveryService dawnDiscoveryService;

	private transient boolean isIncrementalScanEnabled;

	private final boolean incremental;

	@Autowired
	private transient GenericConfiguration configProperties;
	@Autowired
	private transient GenericConfigProperties genericConfigProperties;
	@Autowired
	private transient ApplicationEventPublisher eventPublisher;

	/**
	 * Public constructor.
	 *
	 * @param projectId the ID of the project to execute Discover Metrics
	 * @param incremental {@code true} for incremental discovery or {@code false} to enforce full discovery. Default is {@code true}
	 */
	public DiscoverMetricsJob(final EntityId projectId, final boolean incremental) {
		super(projectId);
		this.incremental = incremental;
	}

	@Override
	public JobMonitor getJobMonitor() {
		return assertNotNull(jobMonitor);
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		LOG.info(() -> "Starting Discover Metrics for project ID " + projectId);
		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_METRICS_TASK_DISCOVERMETRICS);

		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_METRICS_TASK_INITIAL);
		progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_SUBTASK_INITIAL);

		final Config config;
		try {
			config = loadDiscoveryConfig();
		} catch (final DiscoveryException e) {
			return new Result<>(new Status(e));
		}

		aliasManager.init(jobId, projectId, config);

		progressMonitor.setJobDescription(ProgressMonitorText.DISCOVERY_METRICS_TASK_DISCOVERMETRICS);
		progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_SUBTASK_START);


		final var moduleParameters = new ModuleParameters(applyMetricsDate());
		moduleParameters.setJobId(jobId);
		boolean isSuccess = false;

		try {

			LOG.info(() -> ProgressMonitorText.DISCOVERY_METRICS_BACKUP);
			progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_METRICS_BACKUP);
			final var backUpId = getProject().getMetaDataBackupId();
			final var changedModuleIds = getChangedModuleIds(progressMonitor, config, moduleParameters);
			if (StringUtils.isBlank(backUpId)) {
				final String backupId = backupMetaData(changedModuleIds);
				projectService.update(new ProjectPojoPrototype().setUid(getProject().getUid()).setMetaDataBackupId(backupId));
			}

			executeDiscoverMetrics(progressMonitor, config, moduleParameters);
			moduleService.updateModuleContentHashes(projectId);
			moduleService.updateModuleDependencyHashes(projectId);
			
			progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_METRICS_RESTORE);
			LOG.info(() -> ProgressMonitorText.DISCOVERY_METRICS_RESTORE);
			project = projectService.get(projectId);
			final var backupId = assertNotNull(project).getMetaDataBackupId();
			restoreMetaData(assertNotNull(backupId, "MetaDataBackupId should not be null"));
			/* Delete the backup file entry from project after restoring the metaData */
			projectService.update(new ProjectPojoPrototype().setUid(assertNotNull(project).getUid()).setMetaDataBackupId(null));

			if (config.calculateDeadCodeMetrics()) {
				LOG.info(() -> "Discover Metrics: " + ProgressMonitorText.DISCOVERY_METRICS_DEAD_CODE_METRICS);
				progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_METRICS_DEAD_CODE_METRICS);
				calculateDeadCodeMetrics();
			}
			
			LOG.info(() -> "Discover Metrics: " + ProgressMonitorText.DISCOVERY_METRICS_EFFORT_SUMMARY);
			progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_METRICS_EFFORT_SUMMARY);

			/* Should replace the existing EffortSummary for the project */
			effortSummaryService.delete(builder -> builder.ofProject(projectId));

			/* Create the effort summary report only when the feature is enabled */
			if (ff4j.getFeature(FeatureId.COLLECT_PARSER_ERRORS.getId()).isEnable()) {
				final SummaryCollector collector = new SummaryCollector(moduleService);
				effortSummaryService.create(Stream.concat(collector.collectModuleSummary(projectId).stream(), collector.collectPricingData(projectId).stream())
													.toList());
			} else {
				/* Otherwise store it with a hint so user doesn't file error tickets because of 'missing' effort summary reports */
				final Map<String, Object> properties = new HashMap<>();
				TypeSummaryProperties.TECHNOLOGY.setIn(properties, String.format("Summary is not available when feature '%s' is disabled", FeatureId.COLLECT_PARSER_ERRORS.getId()));
				TypeSummaryProperties.TECHNOLOGY_TYPE.setIn(properties, "MISSING");
				TypeSummaryProperties.COUNT.setIn(properties, Integer.valueOf(1));
				effortSummaryService.create(new EffortSummaryPojoPrototype()
												.setProject(projectId)
												.setType(EffortSummaryType.TYPE)
												.setProperties(properties));
			}

			LOG.info(() -> "Updating Metrics Revision");
			updateProjectMetricsRevision();
			isSuccess = true;
		} catch (final OperationCanceledException e) {
			LOG.info(() -> "[DiscoverMetrics] User canceled the request.");
			return new Result<>(new Status(Severity.CANCELED));
		} catch (final Exception e) {
			LOG.error(() -> "[DiscoverMetrics] Error occurred when creating metrics", e);
			return new Result<>(new Status(e));
		} finally {
			if( ! isSuccess) {
				revertMetricsDate();
			}
			discoveryCache.clearDiscoveryJobCache(jobId);
		}

		LOG.info("Finished Discover Metrics");

		/* Clear the step description as we are done now. */
		progressMonitor.setJobDescription("Discover Metrics");
		progressMonitor.setStepDescription("");
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.ofNullable(null)));
		return new Result<>(new Status(Severity.OK));
	}

	@Override
	public String getJobName() {
		return "Discover Metrics";
	}

	private String backupMetaData(final Optional<Set<Long>> moduleIdsToBackup) {
		final Map<String, List<String>> parameters = new HashMap<>();
		if (moduleIdsToBackup.isPresent()) {
			parameters.put(MetaDataExportJob.PARAM_MODULES_TO_BACKUP, moduleIdsToBackup.get().stream()
					.map(String::valueOf)
					.collect(Collectors.toList()));
		}
		final Job<FileSystemResult> backupJob = exportExtension.createJob(projectId, parameters);
		final Status backupJobStatus = jobManager.submitFromJobAndWait(backupJob, assertNotNull(jobMonitor));
		if (backupJobStatus.getSeverity() != Severity.OK) {
			throw new IllegalStateException("Meta Data Backup Failed");
		}
		/* Backup jobId will be the backup JSON file name. */
		return backupJob.getJobId();
	}

	private void restoreMetaData(final String backupFileName) throws IOException {
		final byte[] backupData = FileUtils.readFileToByteArray(Paths.get(jobConfig.getJobResultFolder(), backupFileName).toFile());
		/* Restore the backup metaData to the modules. */
		final Status jobInfo = jobManager.submitFromJobAndWait(importExtension.createJob(projectId, Collections.emptyMap(),
				new HttpEntity<>(backupData)), assertNotNull(jobMonitor));
		if (jobInfo.getSeverity() != Severity.OK) {
			throw new IllegalStateException("Meta Data Restore Failed");
		}
	}

	/**
	 * Executes the actual Discover Metrics process.
	 *
	 * @param progressMonitor the {@link ProgressMonitor} to track execution
	 * @param config the Discovery {@link Config} to use
	 * @param moduleParameters additional properties to apply to created modules
	 * @throws DiscoveryException when an error occurs during discovery
	 */
	protected void executeDiscoverMetrics(final ProgressMonitor progressMonitor, final Config config, final ModuleParameters moduleParameters)
			throws DiscoveryException {
		progressMonitor.setJobDescription(JOB_PREFIX);
		taskProcessor = new ResultOrderTaskProcessor<>(jobManager, assertNotNull(jobMonitor));

		LOG.info(() -> "Executing Discover Metrics on project  " + projectId);

		progressMonitor.setStepDescription(ProgressMonitorText.DISCOVERY_METRICS_TASK_COLLECTING);

		final ProjectPojo targetProject = getProject();
		if (targetProject.isMarkedDeleted()) {
		    throw new innowake.mining.data.error.ConstraintViolationException(targetProject, "Can not Discover Metrics in project that was marked as deleted");
		}

		/* collect modules, generic and dependent metrics */
		assertNotNull(metricsCollector).collect(targetProject, progressMonitor.subMonitor(0), isIncrementalScanEnabled);
	}

	/**
	 * Returns the ModuleIds of the changed Modules having metaData to backup.
	 *
	 * @param progressMonitor the {@link ProgressMonitor} to track execution
	 * @param config the Discovery {@link Config} to use
	 * @param moduleParameters additional properties to apply to created modules
	 * @return ModuleIds of the deleted and re-scanned Modules having metaData
	 */
	protected Optional<Set<Long>> getChangedModuleIds(final ProgressMonitor progressMonitor, final Config config, final ModuleParameters moduleParameters) {
		isIncrementalScanEnabled = incremental && ff4j.getFeature(FeatureId.INCREMENTAL_SCAN.getId()).isEnable();

		metricsCollector = new MetricsCollector(sourceService, moduleService, contributorProvider,
				persistMetricsModelService, this, buildProperties, moduleParameters, ff4j, jobConfig,
				configProperties.isDiscoveryRestartEnabled(), genericConfigProperties, dawnDiscoveryService);

		return metricsCollector.prepare(getProject(), progressMonitor, isIncrementalScanEnabled);
	}
	
	private void calculateDeadCodeMetrics() {
		final List<EntityId> supportedModuleIds = moduleService.findModuleIds(q -> q
				.ofProject(projectId)
				.withRepresentation(Representation.PHYSICAL)
				.withTechnologiesAndTypes(IdentifyDeadCodeJob.SUPPORTED_TECHNOLOGY_TYPE));
		final var job = new IdentifyDeadCodeJob(projectId, new ModuleMatcher(supportedModuleIds, null));
		final var deadCodeJobStatus = jobManager.submitFromJobAndWait(job, assertNotNull(jobMonitor));
		if (deadCodeJobStatus.getSeverity() != Severity.OK) {
			throw new IllegalStateException("Dead code metrics calculation Failed for project id " + projectId);
		}
	}

	protected ProjectPojo getProject() {
		ProjectPojo maybeProject = this.project;
		if (maybeProject == null) {
			try {
				maybeProject = this.projectService.get(projectId);
			} catch (final MiningEntityNotFoundException e) {
				LOG.error(() -> "Failed to load project for project ID " + projectId);
				throw e;
			}
		}
		return maybeProject;
	}

	/**
	 * Loads the Discovery configuration from the database.
	 *
	 * @return the Discovery {@link Config}
	 * @throws DiscoveryException if the configuration could not be loaded
	 */
	protected Config loadDiscoveryConfig() throws DiscoveryException {
		final Config config;
		try {
			config = Config.loadConfig(projectService, projectId);
		} catch (final DiscoveryException e) {
			LOG.error(() -> "Failed to load configuration for project ID " + projectId, e);
			throw new DiscoveryException("Failed to load configuration for project ID " + projectId ,e);
		}

		if (config.getUtilityExclusionEnabled()) {
			try {
				config.setUtilityList(UtilityList.loadUtilityList(projectService, projectId));
			} catch (final DiscoveryException e) {
				LOG.error(() -> "Failed to load Utility list.", e);
				throw new DiscoveryException("Failed to load Utility list", e);
			}
			LOG.info(() -> "Utility exclusion is enabled. Artifacts matching entries in utilities.xml will be skipped for analysis.");
		} else {
			LOG.info(() -> "Utility exclusion is disabled.");
		}

		return config;
	}

	/**
	 * Updates the metricsDate property of the current Project with the current Date. Returns the current date.
	 *
	 * @return the metrics date that was set on the project
	 */
	protected Instant applyMetricsDate() {
		final Instant metricsDate = Instant.now();
		oldMetricsDate = getProject().getMetricsDate();
		projectService.update(p -> p.withId(projectId).setMetricsDate(metricsDate));
		return metricsDate;
	}

	/**
	 * Reverts the metricsDate property of the current project to the value it had
	 * before {@link #updateProjectMetricsRevision()} was called for this job.
	 */
	protected void revertMetricsDate() {
		projectService.update(p -> p.withId(projectId).setMetricsDate(oldMetricsDate));
	}

	/**
	 * Updates the metricsBaseRevision and metricsVersion properties of the current project.
	 * {@code metricsBaseRevision} will be set to the current value of {@code sourceCodeRevision} and
	 * {@code metricsVersion} is set to the running version of mining-api-server.
	 */
	protected void updateProjectMetricsRevision() {
		final ProjectPojo projectNotNull = getProject();
		projectService.update(p -> p.withId(projectId)
				.setMetricsBaseRevision(projectNotNull.getSourceCodeRevision())
				.setMetricsVersion(buildProperties.getVersion()));
	}

	@Override
	@SuppressWarnings("unchecked") /* we can use the taskProcessor to submit any kind of task */
	public <R extends Serializable> void forkJobTasks(final TaskSource<R> taskSource, final ResultConsumer<R> resultConsumer) {
		super.forkTasks((TaskProcessor<R>) assertNotNull(taskProcessor), taskSource, resultConsumer);
	}

	@Override
	public Span getSpan() {
		return assertNotNull(jobSpan);
}
}
