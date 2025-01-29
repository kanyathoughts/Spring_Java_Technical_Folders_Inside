/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.util.Map;
import brave.Span;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.preset.AbstractBuilder;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.FeatureId;

/**
 * Parameters for the MetricsContributorProvider.
 */
public class ContributorParameters {
	
	public final String jobId;
	public final ModelArtifact entry;
	public final EntityId projectId;
	public final Config config;
	public final Phase phase;
	@Nullable
	public final SearchOrders searchOrders;
	public final IModuleRepository repo;
	public final ProgressMonitor progressMonitor;
	public final Span taskSpan;
	@Nullable
	public String sourceExportPath;
	public final Map<FeatureId, Boolean> featureMap;
	
	private ContributorParameters(final Builder builder) {
		this.jobId = assertNotNull(builder.jobId);
		this.entry = assertNotNull(builder.entry);
		this.projectId = assertNotNull(builder.projectId);
		this.config = assertNotNull(builder.config);
		this.phase = assertNotNull(builder.phase);
		this.searchOrders = builder.searchOrders;
		this.repo = assertNotNull(builder.repo);
		this.progressMonitor = assertNotNull(builder.progressMonitor);
		this.taskSpan = assertNotNull(builder.taskSpan);
		this.sourceExportPath = builder.sourceExportPath;
		this.featureMap = assertNotNull(builder.featureMap);
	}
	
	/**
	 * Gets the project id.
	 *
	 * @return the project id
	 */
	public EntityId getProjectId() {
		return projectId;
	}		
	
	/**
	 * Gets the {@link IModuleRepository}.
	 *
	 * @return {@link IModuleRepository}
	 */
	public IModuleRepository getRepo() {
		return repo;
	}
	
	/**
	 * Gets the path of the export folder for source content.
	 *
	 * @return path of the export folder for source content
	 */
	@Nullable
	public String getSourceExportPath() {
		return sourceExportPath;
	}
	
	/**
	 * Gets the active discovery {@link Config}.
	 *
	 * @return the active discovery {@link Config}
	 */
	public Config getConfig() {
		return config;
	}
	
	/**
	 * Gets the {@link Phase} for which the contributor is executed.
	 *
	 * @return the current discovery {@link Phase}
	 */
	public Phase getPhase() {
		return phase;
	}
	
	/**
	 * Gets the {@link SearchOrders}.
	 *
	 * @return the {@link SearchOrders}
	 */
	@Nullable
	public SearchOrders getSearchOrders() {
		return searchOrders;
	}

	/**
	 * Builder for the {@link ContributorParameters} of the innowake.mining.server.discovery.metrics.MetricsContributorProvider.
	 */
	public static class Builder extends AbstractBuilder<ContributorParameters, Builder> {
		
		@Nullable
		private String jobId;
		@Nullable
		private ModelArtifact entry;
		@Nullable
		private EntityId projectId;
		@Nullable
		private Config config;
		@Nullable
		private Phase phase;
		@Nullable
		private SearchOrders searchOrders;
		@Nullable
		private IModuleRepository repo;
		@Nullable
		private ProgressMonitor progressMonitor;
		@Nullable
		private Span taskSpan;
		@Nullable
		private String sourceExportPath;
		@Nullable
		private Map<FeatureId, Boolean> featureMap;

		/**
		 * Sets the job Id.
		 * 
		 * @param jobId the job Id
		 * @return this builder instance
		 */
		public Builder setJobId(final String jobId) {
			this.jobId = jobId;
			return getThis();
		}
		
		/**
		 * Sets the {@link ModelArtifact} for which the contributors should be provided.
		 * 
		 * @param artifact the {@link ModelArtifact}
		 * @return this builder instance
		 */
		public Builder setModelArtifact(final ModelArtifact artifact) {
			this.entry = artifact;
			return getThis();
		}
		
		/**
		 * Sets the project Id.
		 * 
		 * @param projectId the project Id
		 * @return this builder instance
		 */
		public Builder setProjectId(final EntityId projectId) {
			this.projectId = projectId;
			return getThis();
		}
		
		/**
		 * Sets the active discovery {@link Config}.
		 * 
		 * @param config the {@link Config}
		 * @return this builder instance
		 */
		public Builder setConfig(final Config config) {
			this.config = config;
			return getThis();
		}
		
		/**
		 * Sets the {@link Phase} in which the contributor is executed.
		 * 
		 * @param phase the active discovery {@link Phase}
		 * @return this builder instance
		 */
		public Builder setPhase(final Phase phase) {
			this.phase = phase;
			return getThis();
		}
		
		/**
		 * Sets the {@link SearchOrders} if required.
		 * 
		 * @param searchOrders the {@link SearchOrders}
		 * @return this builder instance
		 */
		public Builder setSearchOrders(@Nullable final SearchOrders searchOrders) {
			this.searchOrders = searchOrders;
			return getThis();
		}
		
		/**
		 * Sets the {@link IModuleRepository} required for some contributors.
		 * 
		 * @param repo the {@link IModuleRepository}
		 * @return this builder instance
		 */
		public Builder setModuleRepository(final IModuleRepository repo) {
			this.repo = repo;
			return getThis();
		}
		
		/**
		 * Sets the {@link ProgressMonitor} used by some contributor for job cancellation checks.
		 * 
		 * @param progressMonitor the {@link ProgressMonitor}
		 * @return this builder instance
		 */
		public Builder setProgressMonitor(final ProgressMonitor progressMonitor) {
			this.progressMonitor = progressMonitor;
			return getThis();
		}
		
		/**
		 * Sets the active {@link Span}.
		 * 
		 * @param taskSpan the {@link Span}
		 * @return this builder instance
		 */
		public Builder setTaskSpan(final Span taskSpan) {
			this.taskSpan = taskSpan;
			return getThis();
		}

		/**
		 * Sets the path of the export folder for source content.
		 * 
		 * @param sourceExportPath the export folder
		 * @return this builder instance
		 */
		public Builder setSourceExportPath(@Nullable final String sourceExportPath) {
			this.sourceExportPath = sourceExportPath;
			return getThis();
		}

		/**
		 * Sets the featureMap from the FF4j source
		 *
		 * @param featureMap object
		 * @return the builder instance
		 */
		public Builder setFeatureMap(@Nullable final Map<FeatureId, Boolean> featureMap) {
			this.featureMap = featureMap;
			return getThis();
		}

		@Override
		protected Builder reset() {
			jobId = null;
			entry = null;
			projectId = null;
			config = null;
			searchOrders = null;
			repo = null;
			progressMonitor = null;
			taskSpan = null;
			sourceExportPath = null;
			featureMap = null;
			return getThis();
		}

		@Override
		protected ContributorParameters internalBuild() {
			return new ContributorParameters(this);
		}
		
	}
}