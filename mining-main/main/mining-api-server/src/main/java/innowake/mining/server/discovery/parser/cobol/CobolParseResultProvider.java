/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.cobol;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.cobol.parser.ast.CobolParseConfiguration;
import innowake.ndt.cobol.parser.ast.CobolParserAst;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;
import innowake.ndt.core.parsing.spi.StringContent;

/**
 * A caching parser result provider for {@code Cobol} modules.
 */
public class CobolParseResultProvider extends AbstractCachingParseResultProvider<Tuple2<CobolModel, IAssembling<SourcePojo>>> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.COBOL_PARSER);

	private final SourceObjectResolver sourceObjectResolver;
	private SourceCachingService sourceService;
	private SearchOrders searchOrders;
	private DiscoveryCache discoveryCache;

	/**
	 * Constructor
	 * 
	 * @param config The {@link Config}
	 * @param worker The {@link TimedWorker}
	 * @param sourceObjectResolver The {@link SourceObjectResolver}
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 * @param sourceService Service for accessing source objects.
	 * @param searchOrders The searchOrders
	 * @param discoveryCache {@link DiscoveryCache}
	 */
	public CobolParseResultProvider(final Config config, final TimedWorker worker, final SourceObjectResolver sourceObjectResolver,
			final String jobId, final ParseResultCacheService parseResultCacheService, final SourceCachingService sourceService, final SearchOrders searchOrders,
			final DiscoveryCache discoveryCache) {
		super(worker, config.getParserTimeout(ResolveTarget.COBOL), jobId, parseResultCacheService);
		this.sourceObjectResolver = sourceObjectResolver;
		this.sourceService = sourceService;
		this.searchOrders = searchOrders;
		this.discoveryCache = discoveryCache;
	}

	/**
	 * Returns a {@link Tuple2} of parse results for the given {@code cobolObject}.
	 * 
	 * @param cobolObject The Cobol source object
	 * @return {@link Tuple2} of parse results
	 * @throws DiscoveryException if parsing failed or if the worker was cancelled
	 */
	@Override
	public Tuple2<CobolModel, IAssembling<SourcePojo>> getParseResult(final SourcePojo cobolObject) throws DiscoveryException {
		try {
			return getParseResult(cobolObject, MessageProvider.from(cobolObject, ResolveTarget.COBOL), new CancellableParserTask<>() {

						@Override
						public Tuple2<CobolModel, IAssembling<SourcePojo>> call() throws AssemblingException {
							CobolParseConfiguration<SourcePojo> configuration = new CobolParseConfiguration.Builder<>(
																								cobolObject,
																								cobolObject.getContent().toString(),
																								new CobolDataProvider(cobolObject.getProject(), jobId, 
																										sourceService, searchOrders, sourceObjectResolver, 
																										discoveryCache))
																								.build();
							configuration.setModuleName(cobolObject.getName());
							configuration.setParserProgressMonitor(this);

							final CobolParserAst parser = new CobolParserAst(LOG);
							LOG.trace(String.format("Parsing and assembling Cobol source object %s with ID %s", cobolObject.getPath(), cobolObject.getId()));

							final IAssembling<SourcePojo> assembling = parser.assemble(configuration);

							/* For the parsing we can reuse the assembling  */
							configuration = new CobolParseConfiguration.Builder<>(
										cobolObject,
										cobolObject.getContent().toString(),
										new CobolDataProvider(cobolObject.getProject(), jobId, 
												sourceService, searchOrders, sourceObjectResolver, 
												discoveryCache))
									/* additionally set assembling */
									.setAssembling(assembling)
									.build();
							configuration.setModuleName(cobolObject.getName());
							return new Tuple2<>(parser.parse(configuration), assembling);
						}
					});
		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}

	/**
	 * Returns the {@link CobolModel} for the given {@code cobolObject}.
	 * 
	 * @param cobolObject The Cobol source object
	 * @return the {@link CobolModel}
	 * @throws DiscoveryException if parsing failed or if the worker was cancelled
	 */
	public CobolModel getModel(final SourcePojo cobolObject) throws DiscoveryException {
		if (LOG.isDebugEnabled()) LOG.debug("Get Cobol Model for {} with ID {}", cobolObject.getPath(), cobolObject.getId());
		return getParseResult(cobolObject).e1;
	}

	/**
	 * Returns the {@link IAssembling} for the given {@code cobolObject}.
	 * 
	 * @param cobolObject The Cobol source object
	 * @return the {@link IAssembling}
	 * @throws DiscoveryException if parsing failed or if the worker was cancelled
	 */
	public IAssembling<SourcePojo> getAssembling(final SourcePojo cobolObject) throws DiscoveryException {
		if (LOG.isDebugEnabled()) LOG.debug("Get Cobol Assembling for {} with ID {}", cobolObject.getPath(), cobolObject.getId());
		return getParseResult(cobolObject).e2;
	}

	/**
	 * Returns the {@link ITokenPartitioning} for the given {@code content}.
	 * 
	 * @param content The source content
	 * @return the {@link ITokenPartitioning}
	 */
	public static ITokenPartitioning getPartitioning(final String content) {
		return TokenPartitioner2.create(CobolLexerFactory.get()).doPartitioning(new StringContent(content));
	}
}
