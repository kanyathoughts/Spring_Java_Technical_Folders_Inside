/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.plsql;

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
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.parsing.parser.plsql.PlSqlParser;
import innowake.ndt.parsing.parser.plsql.model.PlSqlModel;

/**
 * Provide PlSql Parser results
 */
public class PlSqlParseResultProvider extends AbstractCachingParseResultProvider<PlSqlModel> {

	private static final IAssemblingDataProvider<SourcePojo> DATA_PROVIDER = new PlSqlDataProvider();

	/**
	 * Creates an instance of PlSqlParserResultProvider
	 * 
	 * @param config the configuration
	 * @param worker executor for the parsing
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public PlSqlParseResultProvider(final Config config, final TimedWorker worker, final String jobId, final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.SQL), jobId, parseResultCacheService);
	}

	@Override
	public PlSqlModel getParseResult(final SourcePojo file) throws DiscoveryException {
		try {
			return getParseResult(file, MessageProvider.from(file, ResolveTarget.SQL), new CancellableParserTask<>() {

				@Override
				public PlSqlModel call() throws DiscoveryException {
					final var parserConfiguration = new BaseParserConfiguration.Builder<SourcePojo>()
															.setAssemblingDataProvider(DATA_PROVIDER)
															.build();
					parserConfiguration.setParserProgressMonitor(this);

					final PlSqlParser<SourcePojo> parser = new PlSqlParser<>(parserConfiguration);
					return parser.parse(file)
								 .orElseThrow(() -> new DiscoveryException("Unable to parse the content in the path " + file.getPath()));
				}
			});
		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}
}
