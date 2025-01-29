/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.parser.sqllightweight;

import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.discovery.parser.DefaultDataProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.parsing.parser.sql.lightweight.SqlLightweightParser;

import java.util.Optional;

/**
 * Parse Result Provider using the {@link SqlLightweightParser}.
 */
public class SqlLightWeightParseResultProvider extends AbstractCachingParseResultProvider<AstModel> {

	/**
	 * Creates an instance of SqlLightWeightParseResultProvider.
	 *
	 * @param config				  The Discovery Config
	 * @param worker                  The {@link TimedWorker} that executes the parser tasks
	 * @param jobId                   The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService} To cache the parser results
	 */
	public SqlLightWeightParseResultProvider(final Config config, final TimedWorker worker, final String jobId,
											 final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.SQL), jobId, parseResultCacheService);
	}

	@Override
	public AstModel getParseResult(final SourcePojo sourceObject) throws DiscoveryException, TimedWorker.WorkerCancellationException {
		return getParseResult(sourceObject, TimedWorker.MessageProvider.from(sourceObject, ResolveTarget.SQL_STORED_PROCEDURE), new CancellableParserTask<>() {

			@Override
			public AstModel call() throws DiscoveryException {
				final BaseParserConfiguration<SourcePojo> configuration = new BaseParserConfiguration.Builder<SourcePojo>()
						.setAssemblingDataProvider(new DefaultDataProvider())
						.build();

				configuration.setParserProgressMonitor(this);
				configuration.setModuleName(sourceObject.getName());

				final Optional<AstModel> parseResult = new SqlLightweightParser<>(configuration).parse(sourceObject);
				if (parseResult.isEmpty()) {
					throw new DiscoveryException("parsing of " + sourceObject.getPath() + " has failed");
				}
				return parseResult.get();
			}
		});
	}
}
