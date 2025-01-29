/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import java.time.Duration;
import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.jobs.Job;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.feature.FindFeatureById;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.model.Feature;
import innowake.mining.shared.model.FeatureId;

/**
 * Mining feature maintenance. Holds a cache which automatically refreshes every 10 minutes.
 * <p>
 * Features can be maintained with FF4J feature UI {@code <api-server-url>/feature-console/}.
 */
public final class Features {

	/**
	 * Singleton instance.
	 */
	public static final Features INSTANCE = new Features();
	
	private static final long REFRESH_DURATION_IN_MINUTES = 10;

	private final LoadingCache<FeatureCacheKey, Optional<Feature>> featureCache;
	
	private Features() {
		featureCache = CacheBuilder.newBuilder()
				.refreshAfterWrite(Duration.ofMinutes(REFRESH_DURATION_IN_MINUTES))
				.build(new FeatureCacheLoader());
	}
	
	/**
	 * Returns whether in workbench scope a feature is enabled or not. 
	 *
	 * @param featureId the ID of the feature to ask
	 * @return {@code true} if the feature is enabled
	 */
	public boolean isEnabled(final FeatureId featureId) {
		return isEnabledInternal(null, featureId);
	}
	
	/**
	 * Returns whether in project scope a feature is enabled or not. 
	 *
	 * @param project the project to ask
	 * @param featureId the ID of the feature to ask
	 * @return {@code true} if the feature is enabled
	 */
	public boolean isEnabled(final IProject project, final FeatureId featureId) {
		return isEnabledInternal(project, featureId);
	}
	
	/**
	 * Returns whether for the given {@link ConnectionInfo} a feature is enabled or not. 
	 *
	 * @param connectionInfo the {@link ConnectionInfo} to ask
	 * @param featureId the ID of the feature to ask
	 * @return {@code true} if the feature is enabled
	 */
	public boolean isEnabled(final ConnectionInfo connectionInfo, final FeatureId featureId) {
		final FeatureCacheKey key = new FeatureCacheKey(connectionInfo, featureId);
		final Optional<Feature> feature = featureCache.getUnchecked(key);
		return feature.isPresent() && feature.get().isEnabled();
	}
	
	/**
	 * Refresh the feature cache for all features, projects and workspace.
	 */
	public void refreshCache() {
		final Job job = Job.create("Refreshing feature cache", monitor -> {
			
			featureCache.invalidateAll();
			for (final FeatureId featureId : FeatureId.values()) {
				final boolean workspaceResult = isEnabledInternal(null, featureId);
				Logging.info(String.format("Feature '%s' for workspace is %s", featureId.getId(), toString(workspaceResult)));
			
				for (final IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
					if (project.isOpen() && project.getNature(MiningProjectNature.NATURE_ID) != null) {
						final boolean projectResult = isEnabledInternal(project, featureId);
						Logging.info(String.format("Feature '%s' for project '%s' is %s", featureId.getId(), project.getName(), toString(projectResult)));
					}
				}
			}
		});
		job.schedule();
	}
	
	/**
	 * Refresh the feature cache for all features for the given {@link ConnectionInfo}.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo} to refresh the cache for 
	 */
	public void refreshCache(final ConnectionInfo connectionInfo) {
		final Job job = Job.create("Refreshing feature cache", monitor -> {
			
			for (final FeatureId featureId : FeatureId.values()) {
				featureCache.invalidate(new FeatureCacheKey(connectionInfo, featureId));
				final boolean result = isEnabled(connectionInfo, featureId);
				Logging.info(String.format("Feature '%s' for URL '%s' is %s", featureId.getId(), connectionInfo.getUrl(), toString(result)));
			}
		});
		job.schedule();
	}

	private boolean isEnabledInternal(@Nullable final IProject project, final FeatureId featureId) {
		final Optional<ConnectionInfo> connectionInfo = project != null ? 
				MiningPreferences.getConnectionInfo(project) : MiningPreferences.getConnectionInfo();
		if (connectionInfo.isPresent()) {
			return isEnabled(connectionInfo.get(), featureId);
		}
		return false;
	}
	
	private static Object toString(final boolean result) {
		return result ? "enabled" : "disabled";
	}

	private static final class FeatureCacheLoader extends CacheLoader<FeatureCacheKey, Optional<Feature>> {
		
		@Override
		public Optional<Feature> load(@Nullable final FeatureCacheKey key) {
			if (key == null) {
				return Optional.empty();
			}
			final FindFeatureById findFeatureById = ApiClient.featureService(key.connectionInfo).findFeatureById().setFeatureId(key.featureId);
			return MiningServiceExecutor.create(() -> findFeatureById).executeWithDefaultErrorHandling();
		}
	}
	
	private static final class FeatureCacheKey {
		
		private final ConnectionInfo connectionInfo;
		private final FeatureId featureId;
		
		private FeatureCacheKey(final ConnectionInfo connectionInfo, final FeatureId featureId) {
			this.connectionInfo = connectionInfo;
			this.featureId = featureId;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + featureId.hashCode();
			result = prime * result + connectionInfo.hashCode();
			return result;
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			final FeatureCacheKey other = (FeatureCacheKey) obj;
			if (featureId != other.featureId) {
				return false;
			}
			return connectionInfo.equals(other.connectionInfo);
		}

		@Override
		public String toString() {
			return new StringBuilder()
					.append("FeatureCacheKey [url=").append(connectionInfo.getUrl())
					.append(", featureId=").append(featureId).append("]")
					.toString();
		}
	}
}
