/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.basic;

import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicParser;

/**
 * Provide BASIC Parser results.
 * It also caches the BasicModel.
 */
public class BasicParseResultProvider extends AbstractCachingParseResultProvider<BasicModel> {

	/**
	 * Creates an instance of BasicParserResultProvider.
	 * @param config the configuration
	 * @param worker executor for the parsing
	 * @param jobId the job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public BasicParseResultProvider(final Config config, final TimedWorker worker,
										final String jobId, final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.BASIC), jobId, parseResultCacheService);
	}

	@Override
	public BasicModel getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject,
					MessageProvider.from(sourceObject, ResolveTarget.BASIC), new CancellableParserTask<>() {

						@Override
						public BasicModel call() throws DiscoveryException {
							final BaseParserConfiguration<SourcePojo> configuration = new BaseParserConfiguration.Builder<SourcePojo>()
									.setAssemblingDataProvider(new BasicDataProvider())
									.build();
							configuration.setParserProgressMonitor(this);

							final BasicParser<SourcePojo> parser = new BasicParser<>(configuration);
							return parser.parse(sourceObject)
										 .orElseThrow(() -> new DiscoveryException("Unable to parse the content in the path " + sourceObject.getPath()));
						}
					});
		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}
}
