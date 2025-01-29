/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.c;

import C.CLexer;
import C.CParser;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.metrics.c.CDiscoveryMetricsListener;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.antlr.ast.AntlrParseResult;
import innowake.ndt.antlr.base.BaseAntlrErrorListener;
import innowake.ndt.antlr.base.BaseAntlrParseException;
import innowake.ndt.antlr.c.CAntlr2AstListener;
import innowake.ndt.antlr.c.CAntlrParser;
import innowake.ndt.antlr.c.CProgramPreProcessor;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Provide C Parser results.
 * It also caches the {@link CAntlrParseResult} parse result.
 */
public class CAntlrParseResultProvider extends AbstractCachingParseResultProvider<CAntlrParseResult> {

	/**
	 * Creates an instance of CAntlrParserResultProvider
	 * 
	 * @param config the configuration
	 * @param worker executor for the parsing
	 * @param jobId the job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public CAntlrParseResultProvider(final Config config, final TimedWorker worker,
			final String jobId, final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.C), jobId, parseResultCacheService);
	}

	/**
	 * Parse a file and return parser result. Will timeout if timeout time is configured.
	 * 
	 * @param file the file to parse
	 * @return a parse result
	 * @throws DiscoveryException thrown on IO or parser errors.
	 */
	@Override
	public CAntlrParseResult getParseResult(final SourcePojo file) throws DiscoveryException {
		try {
			return getParseResult(file, MessageProvider.from(file, ResolveTarget.C), new CancellableParserTask<>() {

				@Override
				public CAntlrParseResult call() throws BaseAntlrParseException {
					final CAntlrParser antlrParser = new CAntlrParser()
															.setPreProcessor(new CProgramPreProcessor())
															.setErrorListener(new BaseAntlrErrorListener());
					antlrParser.setParserProgressMonitor(this);

					final CLexer lexer = antlrParser.tokenizeText(file.getContent().toString());
					final CParser parser = antlrParser.setupParser(lexer);
					final CDiscoveryMetricsListener metricsListener = new CDiscoveryMetricsListener();
					final AntlrParseResult<AstModel> result = antlrParser.parse(parser, new CAntlr2AstListener(), metricsListener);

					return new CAntlrParseResult(result)
							.setAssignmentValues(metricsListener.getAssignmentValues())
							.setUnvalidatedDependencies(metricsListener.getUnvalidatedDependencies())
							.setFunctionDependencies(metricsListener.getFunctionDependencies())
							.setHeaderDependencies(metricsListener.getHeaderDependencies());
				}
			});
		} catch (final WorkerException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}
}
