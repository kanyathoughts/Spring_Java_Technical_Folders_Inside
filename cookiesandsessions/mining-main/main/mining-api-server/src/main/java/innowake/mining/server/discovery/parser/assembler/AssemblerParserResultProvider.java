/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.parser.assembler;

import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.assembler.model.ast.AssemblerModel;
import innowake.ndt.assembler.parser.AssemblerParser;
import innowake.ndt.assembler.parser.ParserConfiguration;

/**
 * Provides Assembler parser results.
 */
public class AssemblerParserResultProvider extends AbstractCachingParseResultProvider<AssemblerModel<SourcePojo>> {

	private final SourceObjectResolver sourceObjectResolver;

	/**
	 * Creates an instance of AssemblerParserResultProvider.
	 * 
	 * @param worker The {@link TimedWorker}
	 * @param sourceObjectResolver The source object resolver
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public AssemblerParserResultProvider(final TimedWorker worker, final SourceObjectResolver sourceObjectResolver, final String jobId,
			final ParseResultCacheService parseResultCacheService) {
		super(worker, -1, jobId, parseResultCacheService);
		this.sourceObjectResolver = sourceObjectResolver;
	}

	/**
	 * Parse the source object if not present in cache and return parser result otherwise get it from cache.
	 * 
	 * @param sourceObject to parse
	 * @return the parse result
	 * @throws DiscoveryException if the parsing fails
	 */
	@Override
	public AssemblerModel<SourcePojo> getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.ASSEMBLER), new CancellableParserTask<>() {

				@Override
				public AssemblerModel<SourcePojo> call() {
					final ParserConfiguration<SourcePojo> configuration =
							new ParserConfiguration.Builder<SourcePojo>().setAssemblingDataProvider(new AssemblerDataProvider(sourceObjectResolver)).build();

					configuration.setParserProgressMonitor(this);

					final AssemblerParser<SourcePojo> parser = new AssemblerParser<>(configuration);
					return parser.parse(sourceObject);
				}
			});

		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException("Exception while getting assembler parser results ", exception);
		}
	}
}
