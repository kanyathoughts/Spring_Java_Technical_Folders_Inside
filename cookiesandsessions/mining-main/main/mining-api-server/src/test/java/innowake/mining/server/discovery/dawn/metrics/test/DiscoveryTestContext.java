/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import org.mockito.Mockito;

import brave.Span;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Discovery Context for unit tests.
 */
public class DiscoveryTestContext implements DiscoveryContext {

	private static final Instant DEFAULT_METRICS_DATE = ZonedDateTime.of(LocalDateTime.of(2022, 1, 1, 0, 0), ZoneId.of("Europe/Paris")).toInstant();

	private final List<SourcePojo> sourceObjects;
	private final Config config;
	private final ModuleParameters moduleParameters;
	private final EntityId projectId;

	/**
	 * Prepare a Discovery Context with a list of test source files and the default Discovery configuration.
	 *
	 * @param sourceObjects the list of test sources
	 */
	public DiscoveryTestContext(final List<SourcePojo> sourceObjects) {
		this.sourceObjects = sourceObjects;
		this.config = Config.getDefaultConfig();
		this.moduleParameters = new ModuleParameters(DEFAULT_METRICS_DATE);
		this.projectId = EntityId.of(0l);
	}

	/**
	 * Prepare a Discovery Context with a list of test source files and a custom Discovery configuration.
	 *
	 * @param sourceObjects the list of test sources
	 * @param config the custom Discovery configuration
	 */
	public DiscoveryTestContext(final List<SourcePojo> sourceObjects, final Config config) {
		this.sourceObjects = sourceObjects;
		this.config = config;
		this.moduleParameters = new ModuleParameters(DEFAULT_METRICS_DATE);
		this.projectId = EntityId.of(0l);
	}
	
	/**
	 * Prepare a Discovery Context with a list of test source files and a custom Discovery configuration.
	 *
	 * @param sourceObjects the list of test sources
	 * @param projectId the id of the project
	 */
	public DiscoveryTestContext(final List<SourcePojo> sourceObjects, final EntityId projectId) {
		this.sourceObjects = sourceObjects;
		this.config = Config.getDefaultConfig();
		this.moduleParameters = new ModuleParameters(DEFAULT_METRICS_DATE);
		this.projectId = projectId;
	}

	@Override
	public EntityId getProjectId() {
		return projectId;
	}

	@Override
	public String getJobId() {
		return "test-job";
	}

	@Override
	public int getIteration() {
		return 0;
	}

	@Override
	public Config getConfig() {
		return config;
	}

	@Override
	public SearchOrders getSearchOrders() {
		return new SearchOrders(Collections.singletonList(new SearchOrder()));
	}

	@Override
	public SourceObjectResolver getSourceObjectResolver() {
		return new SourceObjectResolver() {
			@Override
			public SourcePojo resolveObject(SourcePojo context, String targetName) {
				return sourceObjects.stream()
						.filter(so -> so.getName().equals(targetName))
						.findFirst()
						.orElse(null);
			}

			@Override
			public SourcePojo resolveObject(SourcePojo context, String targetName, SourceObjectMatcher targetMatcher) {
				return sourceObjects.stream()
						.filter(so -> so.getName().equals(targetName))
						.filter(so -> so.getTechnology() == targetMatcher.getTechnology() && targetMatcher.getTypes().contains(so.getType()))
						.findFirst()
						.orElse(null);
			}
		};
	}

	@Override
	public ModuleParameters getModuleParameters() {
		return moduleParameters;
	}

	@Override
	public Span getCurrentSpan() {
		return Mockito.mock(Span.class);
	}

	@Override
	public TimedWorker getTimedWorker() {
		return new TestTimedWorker();
	}

	/**
	 * @return map with all features disabled.
	 */
	@Override
	public Map<FeatureId, Boolean> getFeatureMap() {
		final Map<FeatureId, Boolean> featureMap = new EnumMap<>(FeatureId.class);
		for (final FeatureId feature : FeatureId.values()) {
			featureMap.put(feature, Boolean.FALSE);
		}

		return featureMap;
	}

	@Override
	public void close() {
		/* does nothing because the TestTimedWorker does not need closing */
	}
}
