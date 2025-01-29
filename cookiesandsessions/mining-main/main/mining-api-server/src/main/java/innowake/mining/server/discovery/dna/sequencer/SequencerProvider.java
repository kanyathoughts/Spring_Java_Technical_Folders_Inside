/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;


import java.util.Map;
import java.util.function.UnaryOperator;

import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.server.discovery.dna.sequencer.jcl.JclSequencer;
import innowake.mining.server.discovery.parser.c.CAntlrAstModelParseResultProvider;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.dna.sequencer.c.CSequencer;
import innowake.mining.server.discovery.dna.sequencer.cobol.CobolSequencer;
import innowake.mining.server.discovery.dna.sequencer.natural.NaturalSequencer;
import innowake.mining.server.discovery.dna.sequencer.pl1.Pl1Sequencer;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Provides {@linkplain Sequencer DNA Sequencers} for different languages.
 */
public class SequencerProvider {

	private final String jobId;
	private final UnaryOperator<String> configProvider;
	private final TimedWorker timedWorker;
	private final SourceObjectResolver sourceObjectResolver;
	private final ParseResultCacheService parseResultCacheService;
	private final Map<FeatureId, Boolean> featureMap;
	private final SourceCachingService sourceService;
	private final SearchOrders searchOrders;
	private final DiscoveryCache discoveryCache;
	private final Config config;
	private final JclSequencer jclSequencer;

	/**
	 * Constructs new sequencer provider.
	 *
	 * @param jobId the id of the find communities job
	 * @param configProvider Function providing configuration data by configuration name.
	 * @param timedWorker a timed worker used for canceling long-running parse jobs
	 * @param sourceObjectResolver source object resolver used to assemble source files during parsing
	 * @param parseResultCacheService cache service where parse results are stored
	 * @param sourceService Provider of source objects.
	 * @param searchOrders The searchOrders
	 * @param discoveryCache {@link DiscoveryCache}
	 * @param featureMap the feature map
	 * @param config the Discovery config
	 * @param jclSequencer the JCL sequencer's spring bean
	 */
	public SequencerProvider(final String jobId, final UnaryOperator<String> configProvider, final TimedWorker timedWorker, final SourceObjectResolver sourceObjectResolver,
			final ParseResultCacheService parseResultCacheService, final SourceCachingService sourceService, final SearchOrders searchOrders,
			final DiscoveryCache discoveryCache, final Map<FeatureId, Boolean> featureMap, final Config config, final JclSequencer jclSequencer) {
		this.jobId = jobId;
		this.configProvider = configProvider;
		this.timedWorker = timedWorker;
		this.sourceObjectResolver = sourceObjectResolver;
		this.parseResultCacheService = parseResultCacheService;
		this.featureMap = featureMap;
		this.sourceService = sourceService;
		this.searchOrders = searchOrders;
		this.discoveryCache = discoveryCache;
		this.config = config;
		this.jclSequencer = jclSequencer;
	}

	/**
	 * Provides a sequencer for the given technology and type. The returned sequencer should be able to parse source objects of the given
	 * technology and type and extract DNA strings from it.
	 *
	 * @param technology the target technology
	 * @param type the target type
	 * @return a {@link Sequencer} instance
	 * @throws IllegalArgumentException if the given technology/type combination is not supported
	 */
	public Sequencer provideSequencer(final Technology technology, final Type type) {
		if (technology == Technology.COBOL && type == Type.PROGRAM) {
			final CobolParseResultProvider parseResultProvider = new CobolParseResultProvider(config, timedWorker, sourceObjectResolver, jobId,
					parseResultCacheService, sourceService, searchOrders, discoveryCache);
			return new CobolSequencer(configProvider, parseResultProvider);
		} else if (technology == Technology.PL1 && (type == Type.PROGRAM || type == Type.MAINPROGRAM)) {
			final Pl1ParseResultProvider parseResultProvider =
					new Pl1ParseResultProvider(config, timedWorker, jobId, parseResultCacheService, sourceObjectResolver, featureMap);
			return new Pl1Sequencer(configProvider, parseResultProvider);
		} else if (technology == Technology.NATURAL && (type == Type.PROGRAM || type == Type.SUBPROGRAM || type == Type.SUBROUTINE ||
				type == Type.FUNCTION)) {
			final NaturalParseResultProvider parseResultProvider =
					new NaturalParseResultProvider(sourceObjectResolver, config, timedWorker, jobId, parseResultCacheService);
			return new NaturalSequencer(configProvider, parseResultProvider);
		} else if (technology == Technology.C && (type == Type.PROGRAM || type == Type.HEADER)) {
			final CAntlrAstModelParseResultProvider parseResultProvider = new CAntlrAstModelParseResultProvider(config, timedWorker, jobId,
					parseResultCacheService);
			return new CSequencer(configProvider, parseResultProvider);
		} else if (technology == Technology.JCL && type == Type.JOB) {
			return jclSequencer;
		} else {
			throw new IllegalArgumentException("Technology " + technology + " and Type " + type + " not supported for DNA.");
		}
	}
}
