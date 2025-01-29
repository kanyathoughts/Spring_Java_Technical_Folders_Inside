/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.preset.AbstractBuilder;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.natural.NaturalModuleCollector;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectManager;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Collects all resources of a {@link SourcePojo} as a list of {@link ModelArtifact} instances.
 */
@Component
public class SourceObjectResourceCollector {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);

	@Autowired
	private SourceCachingService sourceService;
	@Autowired
	private Tracer tracer;
	@Autowired
	private GenericConfigProperties configProperties;
	@Autowired
	private ParserProviderService parserProviderService;

	@Value("${configuration.discovery-enable-legacy-contributors}")
	private boolean enableLegacyContributors;

	/**
	 * Collects all resources of the given {@link SourcePojo}.
	 *
	 * @param parameters the {@link Parameters} used for collecting the resources
	 * @return a list of {@link ModelArtifact} instances
	 */
	public List<ModelArtifact> collect(final Parameters parameters) {
		if ( ! enableLegacyContributors) {
			return Collections.emptyList();
		}

		final SourcePojo sourceObject = parameters.sourceObject;
		final Technology technology = sourceObject.getTechnology();
		final TimedWorker timedWorker = getTimedWorker(technology, parameters.progressMonitor, tracer, parameters.taskSpan);
		final List<ModelArtifact> result = new ArrayList<>();
		final Config config = parameters.config;
		final String jobId = parameters.jobId;
		final SearchOrders searchOrders = assertNotNull(parameters.searchOrders);

		try {
			switch (technology) {
				case JCL:
					/* Handled by DAWN */
					break;
				case COBOL:
				case CICS:
					/* Handled by DAWN */
					break;
				case PL1:
				    /* Handled by DAWN */
					break;
				case ASSEMBLER:
					/* Handled by DAWN */
					break;
				case NATURAL:
					result.addAll(collectNatural(assertNotNull(timedWorker), sourceObject, searchOrders, config, jobId));
					break;
				case BINARY:
					/* handled by Dawn */
					break;
				case CSD:
					break;
				case EASYTRIEVE:
					/* handled by Dawn */
					break;
				case C:
					/* handled by Dawn */
					break;
				case IMS:
					/* handled by Dawn */
					break;
				case VMS:
					result.addAll(collectVms(parameters));
					break;
				case ORACLE:
					/* handled by Dawn */
					break;
				case BASIC:
					/* handled by Dawn */
					break;
				case ECL:
					/* handled by Dawn */
					break;
				case XML:
					/* handled by Dawn */
					break;
				case JAVA:
				case SQL:
					/* handled by Dawn */
					break;
				case VB:
					/* handled by Dawn */
					break;
				case CPP:
					/* handled by Dawn */
					break;
				default:
					throw new IllegalArgumentException("Unsupported technology: " + technology.name());
			}
		} catch (final OperationCanceledException e) {
			throw e;
		} catch (final Exception e) {
			final String sourceObjectInfo = String.format("[%s] Could not analyze %s", sourceObject.getName(), sourceObject.getPath());
			final String message = String.format("[%s] %s", sourceObject.getName(), e.getMessage());
			LOG.error(() -> sourceObjectInfo + "\n" + message, e);
			LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
			ResolveTarget target;
			try {
				target = ResolveTargetHelper.fromTechnologyAndType(sourceObject.getTechnology(), sourceObject.getType());
			} catch (final Exception ex) {
				target = ResolveTarget.UNKNOWN;
				LOG.error(() -> "Error resolving target type:" + ex);
			}

			final ModelArtifact errorArtifact = new ModelArtifact()
												.setName(sourceObject.getName())
												.setPath(sourceObject.getPath())
												.setType(target)
												.validate();

			errorArtifact.addError(new ErrorMarker()
					.setErrorSeverity()
					.setKey(ErrorKey.MODULE_ABORT)
					.setCause(e)
					.validate());

			return Collections.singletonList(errorArtifact);
		} finally {
			if (timedWorker != null) {
				timedWorker.shutdown();
			}
		}

		return result;
	}

	@Nullable
	private TimedWorker getTimedWorker(final Technology technology, final ProgressMonitor progressMonitor, @Nullable final Tracer tracer, @Nullable final Span taskSpan) {
		switch (technology) {
			case JCL:
			case COBOL:
			case NATURAL:
			case PL1:
			case CSD:
			case C:
			case BASIC:
			case ECL:
			case VMS:
			case IMS:
			case EASYTRIEVE:
			case CICS:
				return new TimedWorkerImpl(progressMonitor, tracer, taskSpan);
			default:
				return null;
		}
	}

	private Set<ModelArtifact> collectNatural(final TimedWorker timedWorker, final SourcePojo sourceObject, final SearchOrders searchOrders,
			final Config config, final String jobId) throws DiscoveryException {
		final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		final NaturalParseResultProvider natParserResultProvider = parserProviderService.createNaturalParser(config, timedWorker, searchOrders, jobId);
		final NaturalSourceObjectManager naturalObjectManager = new NaturalSourceObjectManager(sourceObjectResolver, configProperties, natParserResultProvider);
		return new NaturalModuleCollector(sourceObjectResolver, sourceService, naturalObjectManager, natParserResultProvider)
				.collect(sourceObject);
	}

	private Set<ModelArtifact> collectVms(final Parameters parameters) {
		switch (parameters.resolveTarget) {
			case DCL:
				return Collections.emptySet();
			case FMS_FORM:
				/* Supported in DAWN now */
				return Collections.emptySet();
			case IFDL_FORM:
				/* Supported in DAWN now */
				return Collections.emptySet();
			case VAX_MACRO:
				return Collections.emptySet();
			default:
				throw new IllegalArgumentException("Unsupported resolve target for VMS: " + parameters.resolveTarget.name());
		}
	}

	/**
	 * The parameters for {@link SourceObjectResourceCollector#collect(Parameters)}.
	 */
	public static class Parameters {

		private final ProgressMonitor progressMonitor;
		private final Config config;
		private final SourcePojo sourceObject;
		@Nullable
		private final SearchOrders searchOrders;
		private final ResolveTarget resolveTarget;
		private final Span taskSpan;
		public final String jobId;
		private final Map<FeatureId, Boolean> featureMap;

		private Parameters(final Builder builder) {
			this.jobId = assertNotNull(builder.jobId);
			this.progressMonitor = assertNotNull(builder.progressMonitor);
			this.config = assertNotNull(builder.config);
			this.sourceObject = assertNotNull(builder.sourceObject);
			this.searchOrders = builder.searchOrders;
			this.resolveTarget = assertNotNull(builder.resolveTarget);
			this.taskSpan = assertNotNull(builder.taskSpan);
			this.featureMap = assertNotNull(builder.featureMap);
}

		/**
		 * Builder for the {@link Parameters} of the {@link SourceObjectResourceCollector}.
		 */
		public static class Builder extends AbstractBuilder<Parameters, Builder> {

			@Nullable
			private SourcePojo sourceObject;
			@Nullable
			private Config config;
			@Nullable
			private SearchOrders searchOrders;
			@Nullable
			private ProgressMonitor progressMonitor;
			@Nullable
			private ResolveTarget resolveTarget;
			@Nullable
			private Span taskSpan;
			@Nullable
			private String jobId;
			@Nullable
			private Map<FeatureId, Boolean> featureMap;

			/**
			 * Sets the {@link SourcePojo} for which the resources should be collected.
			 *
			 * @param sourceObject the {@link SourcePojo}
			 * @return this builder instance
			 */
			public Builder setSourceObject(final SourcePojo sourceObject) {
				this.sourceObject = sourceObject;
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
			 * Sets the actual {@link ResolveTarget} of the resources to resolve.
			 *
			 * @param resolveTarget the {@link ResolveTarget}
			 * @return this builder instance
			 */
			public Builder setResolveTarget(final ResolveTarget resolveTarget) {
				this.resolveTarget = resolveTarget;
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
			 * Sets the map containing feature settings.
			 *
			 * @param featureMap the feature map
			 * @return this builder instance
			 */
			public Builder setFeatureMap(final Map<FeatureId, Boolean> featureMap) {
				this.featureMap = featureMap;
				return getThis();
			}

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

			@Override
			protected Builder reset() {
				sourceObject = null;
				config = null;
				searchOrders = null;
				progressMonitor = null;
				resolveTarget = null;
				taskSpan = null;
				featureMap = null;
				jobId = null;
				return getThis();
			}

			@Override
			protected Parameters internalBuild() {
				return new Parameters(this);
			}

		}
	}
}
