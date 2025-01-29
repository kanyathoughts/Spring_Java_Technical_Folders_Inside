/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.ecl;

import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.parsing.parser.ecl.EclParser;
import innowake.ndt.parsing.parser.ecl.EclParserConfiguration;
import innowake.ndt.parsing.parser.ecl.model.EclModel;

/**
 * Provide Ecl Parser results and caches the {@link EclModel}.
 */
public class EclParseResultProvider extends AbstractCachingParseResultProvider<EclModel> {

	/**
	 * Creates an instance of EclParserResultProvider.
	 * 
	 * @param config The active discovery {@link Config}; not {@code null}
	 * @param worker The {@link TimedWorker} for the execution of the parsing; not {@code null}
	 * @param jobId The job ID
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public EclParseResultProvider(final Config config, final TimedWorker worker, 
			final String jobId, final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.ECL), jobId, parseResultCacheService);
	}

	/**
	 * Parses the content of the given {@code SourcePojo} and returns the {@link EclModel} for it.
	 * <br><br>
	 *
	 * @param sourceObject The {@link SourcePojo}; not {@code null}
	 * @return The {@link EclModel}; not {@code null}
	 * @throws DiscoveryException if parsing encounters problems
	 */
	@Override
	public EclModel getParseResult(SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject,
							MessageProvider.from(sourceObject, ResolveTarget.ECL),
							new CancellableParserTask<>() {

								@Override
								public EclModel call() throws DiscoveryException {
									final EclParserConfiguration<SourcePojo> config = new EclParserConfiguration.EclParserConfigurationBuilder<SourcePojo>()
																							.setAssemblingDataProvider(new EclDataProvider())
																							.build();
									config.setParserProgressMonitor(this);

									return new EclParser<>(config)
													.parse(sourceObject)
													.orElseThrow(() -> new DiscoveryException("Unable to parse the content in the path " + sourceObject.getPath()));
								}
							});
		} catch (final WorkerException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}
}
