/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;

import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

/**
 * Context object providing context information to {@link FunctionalBlockGeneration}.
 */
public class FunctionalBlockGenerationContext {

	private final EntityId projectId;
	private final String jobId;
	private final ProgressMonitor progressMonitor;
	private final List<String> additionalData;
	private final ReachabilityAnalysisConfig reachabilityAnalysisConfig;

	/**
	 * Constructor to use when not running inside a job.
	 *
	 * @param projectId the id of the project the functional job generation is executed on
	 */
	public FunctionalBlockGenerationContext(final EntityId projectId) {
		this.projectId = projectId;
		this.jobId = UUID.randomUUID().toString();
		progressMonitor = new NullProgressMonitor();
		this.additionalData = new LinkedList<>();
		this.reachabilityAnalysisConfig = ReachabilityAnalysisConfig.defaultConfig();
	}

	/**
	 * Constructor to use when executing functional block generation inside a job.
	 *
	 * @param projectId the id of the project the functional job generation is executed on
	 * @param jobId the id of the job that executes the functional block generation
	 * @param progressMonitor a progress monitor for the generation which allows to cancel the operation
	 */
	public FunctionalBlockGenerationContext(final EntityId projectId, final String jobId, final ProgressMonitor progressMonitor) {
		this.projectId = projectId;
		this.jobId = jobId;
		this.progressMonitor = progressMonitor;
		this.additionalData = new LinkedList<>();
		this.reachabilityAnalysisConfig = ReachabilityAnalysisConfig.defaultConfig();
	}

	/**
	 * Constructor to use when executing functional block generation inside a job.
	 *
	 * @param projectId the id of the project the functional job generation is executed on
	 * @param jobId the id of the job that executes the functional block generation
	 * @param progressMonitor a progress monitor for the generation which allows to cancel the operation
	 * @param reachabilityAnalysisConfig the configuration for the reachability analysis
	 */
	public FunctionalBlockGenerationContext(final EntityId projectId, final String jobId, final ProgressMonitor progressMonitor,
			final ReachabilityAnalysisConfig reachabilityAnalysisConfig) {
		this.projectId = projectId;
		this.jobId = jobId;
		this.progressMonitor = progressMonitor;
		this.additionalData = new LinkedList<>();
		this.reachabilityAnalysisConfig = reachabilityAnalysisConfig;
	}

	/**
	 * Returns the id of the project the functional job generation is executed on.
	 * @return the id of the project the functional job generation is executed on
	 */
	public EntityId getProjectId() {
		return projectId;
	}

	/**
	 * Returns the id of the job that executes the functional block generation.
	 * @return the id of the job that executes the functional block generation
	 */
	public String getJobId() {
		return jobId;
	}

	/**
	 * Returns a progress monitor for the generation which allows to cancel the operation.
	 * @return a progress monitor for the generation which allows to cancel the operation
	 */
	public ProgressMonitor getProgressMonitor() {
		return progressMonitor;
	}
	
	/**
	 * Returns the additional data for handling data of FunctionalBlock using context.
	 * @return Returns the additional data for handling data of FunctionalBlock using context.
	 */
	public List<String> getAdditionalData() {
		return additionalData;
	}

	/**
	 * Returns the configuration for the reachability analysis.
	 * @return the configuration for the reachability analysis
	 */
	public ReachabilityAnalysisConfig getReachabilityAnalysisConfig() {
		return reachabilityAnalysisConfig;
	}

	/**
	 * Sets the additional data for handling data of FunctionalBlock using context.
	 * @param additionalData the additional data for handling data of FunctionalBlock using context.
	 */
	public void setAdditionalData(final List<String> additionalData) {
		this.additionalData.addAll(additionalData);
	}
}
