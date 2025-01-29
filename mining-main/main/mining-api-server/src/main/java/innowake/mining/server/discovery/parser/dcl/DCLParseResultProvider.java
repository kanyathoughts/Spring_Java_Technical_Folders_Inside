/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.parser.dcl;

import java.util.Optional;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.assembler.AssemblerDataProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.parsing.parser.dcl.DclParser;
import innowake.ndt.parsing.parser.dcl.DclParserConfiguration;
import innowake.ndt.parsing.parser.dcl.model.DclModel;

/**
 * Provide Ecl Parser results and caches the {@link DclModel}.
 */
public class DCLParseResultProvider extends AbstractCachingParseResultProvider<Optional<DclModel>> {
	
	private final SourceObjectResolver sourceObjectResolver;

	/**
	 * Creates an instance of DCLParseResultProvider.
	 * 
	 * @param config The active discovery {@link Config}
	 * @param worker The {@link TimedWorker} for the execution of the parsing
	 * @param jobId The job ID
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 * @param sourceObjectResolver The source object resolver
	 */
	public DCLParseResultProvider(final Config config, final TimedWorker worker, final String jobId, final ParseResultCacheService parseResultCacheService,
			final SourceObjectResolver sourceObjectResolver) {
		super(worker, -1, jobId, parseResultCacheService);
		this.sourceObjectResolver = sourceObjectResolver;
	}

	/**
	 * Parses the content of the given {@code SourcePojo} and returns the {@link DclModel} for it.
	 * <br><br>
	 *
	 * @param sourceObject The {@link SourcePojo}
	 * @return The {@link Optional} with the {@link DclModel}
	 */
	@Override
	public Optional<DclModel> getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.DCL), () -> {
				final DclParserConfiguration<SourcePojo> config = new DclParserConfiguration.Builder<SourcePojo>()
																			.setAssemblingDataProvider(new AssemblerDataProvider(sourceObjectResolver))
																			.build();
				return new DclParser<SourcePojo, DclModel>(config).parse(sourceObject);
			});
		} catch (final WorkerException exception) {
			throw new DiscoveryException(exception.getCause().getMessage(), exception.getCause());
		}
	}
}
