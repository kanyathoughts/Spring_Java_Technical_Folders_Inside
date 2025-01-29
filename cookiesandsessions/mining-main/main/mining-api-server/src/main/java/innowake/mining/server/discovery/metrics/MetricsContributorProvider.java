/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.aop.aspectj.annotation.AspectJProxyFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.discovery.metrics.ContributorParameters;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.extensions.MetricsContributorExtension;
import innowake.mining.server.aspect.profiling.MetricsContributorProfilingAspect;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.natural.NaturalContributor;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectAliasManager;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectAliasProvider;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectManager;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.FeatureId;

/**
 * This provider will provide the actual {@link MetricsContributor} instances for a specific source object.
 */
@Component
public class MetricsContributorProvider {

	@Autowired
	private NaturalSourceObjectAliasManager aliasManager;
	@Autowired
	private SourceCachingService sourceService;
	@Autowired
	protected Tracer tracer;
	@Autowired(required = false)
	private List<MetricsContributorExtension> customContributors = Collections.emptyList();
	@Autowired
	private GenericConfigProperties configProperties;
	@Autowired
	private ParserProviderService parserProviderService;
	@Autowired
	private ModuleService moduleService;

	@Value("${configuration.discovery-enable-legacy-contributors}")
	private boolean enableLegacyContributors;

	@Autowired(required = false)
	@Nullable
	private MetricsContributorProfilingAspect metricsContributorProfilingAspect;

	private enum Contributor {
		NONE(NoneContributor::accept),
		RESOURCE(ResourceContributor::accept),
		NATURAL(NaturalContributor::accept),
		UNKNOWN(UnknownContributor::accept);

		private final ContributorFunction function;

		private Contributor(final ContributorFunction function) {
			this.function = function;
		}

		private boolean accept(final ModelArtifact modelArtifact, final Phase phase) {
			return this.function.accept(modelArtifact, phase);
		}

		private static interface ContributorFunction {

			/**
			 * Calls the accept method of a {@link MetricsContributor} with the given arguments.
			 *
			 * @param modelArtifact the {@link ModelArtifact}
			 * @param phase the {@link Phase}
			 * @return the result from the {@link MetricsContributor MetricsContributors} accept method
			 */
			boolean accept(ModelArtifact modelArtifact, Phase phase);
		}
	}

	/**
	 * Provides a list of {@link MetricsContributor} instances that are applicable for a single source object based on the given {@code parameters}.
	 *
	 * @param parameters the {@link ContributorParameters} providing the necessary information on which contributor should be provided
	 * @return a list of {@link MetricsContributor} instances
	 */
	public List<MetricsContributor> provideContributors(final ContributorParameters parameters) {
		if ( ! enableLegacyContributors) {
			return Collections.emptyList();
		}

		final List<Contributor> applicableDefaultContributors = getApplicableDefaultContributors(parameters.entry, parameters.phase);
		final TimedWorker timedWorker = getTimedWorker(applicableDefaultContributors, parameters.progressMonitor, tracer, parameters.taskSpan);
		final IModuleRepository repo = parameters.repo;
		final EntityId projectId = parameters.projectId;
		final Config config = parameters.config;
		final SearchOrders searchOrders = assertNotNull(parameters.searchOrders);
		final String jobId = parameters.jobId;

		final List<MetricsContributor> enabledContributors = applicableDefaultContributors.stream().map(contributor -> {
			final MetricsContributor metricsContributor;
			switch (contributor) {
				case NONE:
					metricsContributor = new NoneContributor();
					break;
				case RESOURCE:
					metricsContributor = new ResourceContributor();
					break;
				case NATURAL:
					metricsContributor = createNaturalContributor(repo, assertNotNull(timedWorker), searchOrders, projectId, config, jobId, parameters.featureMap);
					break;
				case UNKNOWN:
					metricsContributor = new UnknownContributor();
					break;
				default:
					throw new IllegalStateException("Unsupported contributor of type: " + contributor.name());
			}

			return new CloseableMetricsContributor(metricsContributor, timedWorker);
		}).collect(Collectors.toList());

		/* Handle custom contributors */
		customContributors.stream()
				.filter(extension -> extension.accept(parameters.entry, parameters.phase))
				.map(extension -> extension.init(parameters, sourceService,
						extension.requiresTimedWorker() ? createTimedWorker(parameters.progressMonitor, tracer, parameters.taskSpan) : null))
				.forEach(enabledContributors::add);

		if (metricsContributorProfilingAspect != null) {
			/* Manually weave the profiling aspect into the returned contributor classes - this is necessary because
			 * we instantiate the contributors manually using "new", so Spring can not do it for us automatically. */
			return enabledContributors.stream().map(contributor -> {
				final AspectJProxyFactory proxyFactory = new AspectJProxyFactory(contributor);
				proxyFactory.setProxyTargetClass(true);
				proxyFactory.addAspect(assertNotNull(metricsContributorProfilingAspect));
				return proxyFactory.<MetricsContributor>getProxy();
			}).collect(Collectors.toList());
		} else {
			return enabledContributors;
		}
	}

	private static List<Contributor> getApplicableDefaultContributors(final ModelArtifact entry, final Phase phase) {
		final List<Contributor> contributors = new ArrayList<>();
		for (final Contributor contributor : Contributor.values()) {
			if (contributor.accept(entry, phase)) {
				contributors.add(contributor);
			}
		}

		return contributors;
	}

	/**
	 * @param entry the {@link ModelArtifact} to test
	 * @param phase the {@link Phase}
	 * @return {@code true} if a custom or build-in {@code Contributor} is applicable for the given {@code entry} and {@code phase}. Otherwise {@code false}
	 */
	public boolean hasApplicableContributors(final ModelArtifact entry, final Phase phase) {
		for (final Contributor contributor : Contributor.values()) {
			if (contributor.accept(entry, phase)) {
				return true;
			}
		}

		return customContributors.stream().anyMatch(extension -> extension.accept(entry, phase));
	}

	public boolean hasCustomContributors() {
		return ! customContributors.isEmpty();
	}

	@Nullable
	private TimedWorker getTimedWorker(final List<Contributor> contributors, final ProgressMonitor progressMonitor, @Nullable final Tracer tracer,
			@Nullable final Span taskSpan) {
		final boolean requiresWorker = contributors.stream().anyMatch(contributor -> {
			switch (contributor) {
				case NATURAL:
					return true;
				default:
					return false;
			}
		});
		return requiresWorker ? createTimedWorker(progressMonitor, tracer, taskSpan) : null;
	}

	private TimedWorker createTimedWorker(final ProgressMonitor progressMonitor, @Nullable final Tracer tracer, @Nullable final Span taskSpan) {
		return new TimedWorkerImpl(progressMonitor, tracer, taskSpan);
	}

	private MetricsContributor createNaturalContributor(final IModuleRepository repo, final TimedWorker timedWorker, final SearchOrders searchOrders,
			final EntityId projectId, final Config config, final String jobId, final Map<FeatureId, Boolean> featureMap) {
		final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrders);
		final NaturalSourceObjectAliasProvider aliasProvider = new NaturalSourceObjectAliasProvider(jobId, aliasManager);
		final JclParseResultProvider jclParseResultProvider = parserProviderService.createJclParser(config, timedWorker, jobId, projectId);
		final NaturalParseResultProvider natParserResultProvider = parserProviderService.createNaturalParser(config, timedWorker, searchOrders, jobId);
		final NaturalSourceObjectManager naturalObjectManager = new NaturalSourceObjectManager(sourceObjectResolver, aliasProvider, configProperties,
				natParserResultProvider);
		return new NaturalContributor(repo, jclParseResultProvider, natParserResultProvider, projectId, sourceService, sourceObjectResolver,
				naturalObjectManager, featureMap, config);
	}

	/*
	 * WMIN-7525: Made class and constructor public to avoid:
	 * org.springframework.aop.framework.AopConfigException: Could not generate CGLIB subclass of class innowake.mining.server.discovery.metrics.MetricsContributorProvider$a: Common causes of this problem include using a final class or a non-visible class; nested exception is java.lang.IllegalArgumentException: No visible constructors in class innowake.mining.server.discovery.metrics.MetricsContributorProvider$a
	 * 		at org.springframework.aop.framework.CglibAopProxy.getProxy(CglibAopProxy.java:209) ~[spring-aop-5.3.18.jar!/:5.3.18]
	 * 		at org.springframework.aop.framework.CglibAopProxy.getProxy(CglibAopProxy.java:156) ~[spring-aop-5.3.18.jar!/:5.3.18]
	 * 		at org.springframework.aop.aspectj.annotation.AspectJProxyFactory.getProxy(AspectJProxyFactory.java:182) ~[spring-aop-5.3.18.jar!/:5.3.18]
	 * 		at innowake.mining.server.discovery.metrics.MetricsContributorProvider.a(Unknown Source) ~[classes!/:99.9.0-alpha-202301130221-1]
	 * 		at java.util.stream.ReferencePipeline$3$1.accept(ReferencePipeline.java:195) ~[?:?]
	 * 		at java.util.ArrayList$ArrayListSpliterator.forEachRemaining(ArrayList.java:1654) ~[?:?]
	 * 		at java.util.stream.AbstractPipeline.copyInto(AbstractPipeline.java:484) ~[?:?]
	 * 		at java.util.stream.AbstractPipeline.wrapAndCopyInto(AbstractPipeline.java:474) ~[?:?]
	 * 		at java.util.stream.ReduceOps$ReduceOp.evaluateSequential(ReduceOps.java:913) ~[?:?]
	 * 		at java.util.stream.AbstractPipeline.evaluate(AbstractPipeline.java:234) ~[?:?]
	 * 		at java.util.stream.ReferencePipeline.collect(ReferencePipeline.java:578) ~[?:?]
	 * 		at innowake.mining.server.discovery.metrics.MetricsContributorProvider.provideContributors(Unknown Source) ~[classes!/:99.9.0-alpha-202301130221-1]
	 */
	public static class CloseableMetricsContributor implements MetricsContributor, AutoCloseable {

		private final MetricsContributor contributor;
		@Nullable
		private final TimedWorker timedWorker;

		public CloseableMetricsContributor(final MetricsContributor contributor, @Nullable final TimedWorker timedWorker) {
			this.contributor = contributor;
			this.timedWorker = timedWorker;
		}

		@Override
		public void calculateGenericMetrics(final ModelArtifact artifact) throws DiscoveryException {
			contributor.calculateGenericMetrics(artifact);
		}

		@Override
		public void calculateDependentMetrics(final ModelArtifact artifact) throws DiscoveryException {
			contributor.calculateDependentMetrics(artifact);
		}

		@Override
		public void calculateTransitiveMetrics(final ModelArtifact artifact) throws DiscoveryException {
			contributor.calculateTransitiveMetrics(artifact);
		}

		@Override
		public void close() {
			contributor.close();
			if (timedWorker != null) {
				timedWorker.shutdown();
			}
		}

		@Override
		public String getName() {
			return contributor.getName();
		}
	}
}
