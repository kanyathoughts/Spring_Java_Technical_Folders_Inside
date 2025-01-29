/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.parser.c;

import C.CLexer;
import C.CParser;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.antlr.ast.AntlrParseResult;
import innowake.ndt.antlr.base.BaseAntlrErrorListener;
import innowake.ndt.antlr.base.BaseAntlrParseException;
import innowake.ndt.antlr.c.CAntlr2AstListener;
import innowake.ndt.antlr.c.CAntlrParser;
import innowake.ndt.antlr.c.CProgramPreProcessor;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Provides the parse results for the Language: {@link Technology#C}. The parse result is an {@link AstModel}.
 */
public class CAntlrAstModelParseResultProvider extends AbstractCachingParseResultProvider<AstModel> {

	/**
	 * Creates an instance of CAntlrAstModelParseResultProvider.
	 *
	 * @param config the configuration
	 * @param worker executor for the parsing
	 * @param jobId the job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public CAntlrAstModelParseResultProvider(final Config config, final TimedWorker worker, final String jobId,
			final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.C), jobId, parseResultCacheService);
	}

	@Override
	public AstModel getParseResult(final SourcePojo sourceObject) throws DiscoveryException, WorkerCancellationException {
		try {
			return getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.C), new CancellableParserTask<>() {

				@Override
				public AstModel call() throws DiscoveryException {
					final CAntlrParser antlrParser = new CAntlrParser().setPreProcessor(new CProgramPreProcessor()).setErrorListener(new BaseAntlrErrorListener());
					antlrParser.setParserProgressMonitor(this);

					try {
						final CLexer lexer = antlrParser.tokenizeText(sourceObject.getContent().toString());
						final CParser parser = antlrParser.setupParser(lexer);
						final CAntlr2AstListener cListener = new CAntlr2AstListener();
						final AntlrParseResult<AstModel> result = antlrParser.parse(parser, cListener);
						return result.getAstModel();
					} catch (final BaseAntlrParseException e) {
						throw new DiscoveryException("Error while parsing the file", e);
					}
				}
			});
		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}
}