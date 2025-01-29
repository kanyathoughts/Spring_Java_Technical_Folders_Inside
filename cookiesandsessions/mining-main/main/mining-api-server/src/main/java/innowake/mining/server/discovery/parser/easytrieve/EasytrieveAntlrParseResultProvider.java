/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.easytrieve;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.UnaryOperator;

import com.google.common.io.Files;

import easytrieve.EasytrieveLexer;
import easytrieve.EasytrieveParser;
import groovy.lang.Tuple2;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.metrics.easytrieve.EasytrieveDiscoveryFileDetectionVisitor;
import innowake.mining.server.discovery.metrics.easytrieve.EasytrieveDiscoveryMacroFileDetectionVisitor;
import innowake.mining.server.discovery.metrics.easytrieve.EasytrieveDiscoveryMetricsListener;
import innowake.mining.server.discovery.parser.CancellableParserProgressMonitor;
import innowake.mining.server.discovery.parser.ParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.antlr.ast.AntlrParseResult;
import innowake.ndt.antlr.base.BaseAntlrErrorListener;
import innowake.ndt.antlr.base.BaseAntlrParseException;
import innowake.ndt.antlr.base.BaseAntlrParserError;
import innowake.ndt.antlr.easytrieve.EasytrieveAntlr2AstListener;
import innowake.ndt.antlr.easytrieve.EasytrieveAntlrParser;
import innowake.ndt.antlr.easytrieve.EasytrieveAntlrParser.AssembledEasytrieveProgram;
import innowake.ndt.antlr.easytrieve.EasytrieveProgramPreProcessor;
import innowake.ndt.antlr.easytrieve.macro.EasytrieveAntlrMacroParser;
import innowake.ndt.antlr.easytrieve.parsers.MacroCall;
import innowake.ndt.core.parsing.ast.AstModel;

public class EasytrieveAntlrParseResultProvider implements ParseResultProvider<EasytrieveAntlrParseResult> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.EZT_PARSER);
	private static final int FILE_DETECTION_ERROR_LIMIT = 2;

	private final int timeout;
	private final TimeUnit timeoutUnit;
	private final Config config;
	private final TimedWorker worker;

	public EasytrieveAntlrParseResultProvider(final Config config, final TimedWorker worker) {
		this.timeout = config.getParserTimeout(ResolveTarget.EASYTRIEVE);
		this.timeoutUnit = TimeUnit.SECONDS;
		this.worker = worker;
		this.config = config;
	}

	private EasytrieveAntlrParseResult gatherAndReturnParseResultFromParser(final EasytrieveDiscoveryMetricsListener eztMetricsListener,
			final AntlrParseResult<AstModel> parseResult) {
		/* try to resolve field references */
		eztMetricsListener.resolveAllFieldToDependencyReferences();
		
		/* set metrics */
		return new EasytrieveAntlrParseResult(parseResult).setLinesOfCode(eztMetricsListener.getNewlineCountWithComments())
												.setLinesOfComments(eztMetricsListener.getCommentLineCount())
												.setNumberOfStatements(eztMetricsListener.getNumberOfStatements())
												.setComplexity(eztMetricsListener.getComplexity())
												.addModelErrors(eztMetricsListener.getErrors())
												.setUnvalidatedDependencies(eztMetricsListener.getUnvalidatedDependencies())
												.setInlineMacroNames(eztMetricsListener.getInlineMacros())
												.setAssignmentValues(eztMetricsListener.getAssignmentValues());
								
	}
	
	/**
	 * Assemble this easytrieve file by replacing all macro calls with actual code in the macro (if it can be found)
	 *
	 * @param file ezt file to assemble
	 * @param macroFiles all the potential macro files
	 * @return the assembled program text
	 * @throws DiscoveryException when there is an error parsing or reading the file(s).
	 */
	public AssembledEasytrieveProgram assembleFile(final SourcePojo file, final Set<SourcePojo> macroFiles) throws DiscoveryException {
		final EasytrieveAntlrParser antlrParser = new EasytrieveAntlrParser()
															.setPreProcessor(new EasytrieveProgramPreProcessor() )
															.setErrorListener(new BaseAntlrErrorListener());

		final CancellableParserProgressMonitor progressMonitor = new CancellableParserProgressMonitor();
		antlrParser.setParserProgressMonitor(progressMonitor);

		try {
			final UnaryOperator<String> getMacroContent = macroName -> {
				final Optional<SourcePojo> macro = macroFiles.parallelStream()
					.filter(m -> Files.getNameWithoutExtension(m.getName()).trim().equals(macroName))
					.findFirst();
				if (macro.isPresent()) {
					return macro.get().getContent().toString();
				}
				return null;
			};
			final AssembledEasytrieveProgram result = worker.execute(
					() -> antlrParser.getPreAssembleEasytrieveProgram(file.getContent().toString(), getMacroContent), 
					timeout, timeoutUnit, MessageProvider.from(file, ResolveTarget.EASYTRIEVE)
			);

			for (final BaseAntlrParserError error : result.getErrorsDuringAssembly()) {
				LOG.error(() -> String.format("ERROR During Assembling Program [%s]: %s", file.getPath(), error.toString()));
			}

			return result;
		} catch (final WorkerException exception) {
			progressMonitor.cancel();
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}
	
	/**
	 *
	 * Parse a file and return parser result. Will timeout if timeout time is configured.
	 *
	 * @param file the file to parse
	 * @return the parse result
	 * @throws DiscoveryException exception thrown for IO or parser errors
	 */
	@Override
	public EasytrieveAntlrParseResult getParseResult(final SourcePojo file) throws DiscoveryException {
		return parseText(file.getContent().toString());
	}
	
	/**
	 *
	 * Parse a string containing all the easytrieve code to parse. Will timeout if this is set in config
	 *
	 * @param text the text to parse.
	 * @return a parse result
	 * @throws DiscoveryException thrown on IO or parser errors.
	 */
	public EasytrieveAntlrParseResult parseText(final String text) throws DiscoveryException {
		final EasytrieveAntlrParser antlrParser = new EasytrieveAntlrParser()
															.setPreProcessor(new EasytrieveProgramPreProcessor())
															.setErrorListener(new BaseAntlrErrorListener());

		final CancellableParserProgressMonitor progressMonitor = new CancellableParserProgressMonitor();
		antlrParser.setParserProgressMonitor(progressMonitor);

		try {
			final EasytrieveLexer lexer = antlrParser.tokenizeText(text);
			final EasytrieveParser parser = antlrParser.setupParser(lexer);
			final EasytrieveAntlr2AstListener antlr2AstListener = new EasytrieveAntlr2AstListener();
			
			final Tuple2<EasytrieveDiscoveryMetricsListener, AntlrParseResult<AstModel>> result = worker.execute(() -> {
				final EasytrieveDiscoveryMetricsListener metricsListener = new EasytrieveDiscoveryMetricsListener(antlrParser.getTokenStream(), config);
				final AntlrParseResult<AstModel> parseResult = antlrParser.parse(parser, antlr2AstListener, metricsListener);
				return new Tuple2<EasytrieveDiscoveryMetricsListener, AntlrParseResult<AstModel>>(metricsListener, parseResult);
			}, timeout, timeoutUnit, MessageProvider.from(ResolveTarget.EASYTRIEVE));
			
			return gatherAndReturnParseResultFromParser(result.getFirst(), result.getSecond());
		} catch (final WorkerException | BaseAntlrParseException exception) {
			progressMonitor.cancel();
			final var errorMessage = exception.getCause() != null ? exception.getCause().getMessage() : exception.getMessage();
			throw new DiscoveryException(errorMessage, exception);
		}
	}
	
	/**
	 * Special parse method used just to identify a file as easytrieve macro or not. Will timeout according to config settings.
	 *
	 * @param file file to parse to see if it's an easytrieve macro
	 * @return the {@link Identification} or {@code null}
	 *
	 */
	@Nullable
	public Identification checkIfMacroFile(final SourcePojo file) {
		final EasytrieveDiscoveryMacroFileDetectionVisitor macroDetectionVisitor = new EasytrieveDiscoveryMacroFileDetectionVisitor();
		macroDetectionVisitor.setErrorLimit(FILE_DETECTION_ERROR_LIMIT + 1);
		final EasytrieveAntlrMacroParser antlrMacroParser = new EasytrieveAntlrMacroParser()
																		.setPreProcessor(new EasytrieveProgramPreProcessor())
																		.setErrorListener(new BaseAntlrErrorListener());

		final CancellableParserProgressMonitor progressMonitor = new CancellableParserProgressMonitor();
		antlrMacroParser.setParserProgressMonitor(progressMonitor);

		final String fileContents = file.getContent().toString();
		
		try {
			worker.execute(() -> {
				antlrMacroParser.parseTextWithVisitor(fileContents, macroDetectionVisitor);
				return null;
			}, timeout, timeoutUnit, MessageProvider.from(file, ResolveTarget.EASYTRIEVE));
		} catch (final WorkerCancellationException e) {
			progressMonitor.cancel();
			/* explicitly ignore the exception */
			LOG.debug(() -> String.format("[%s] Easytrieve Macro: parsing cancelled due to timeout", file.getName()));
			return null;
		} catch (final WorkerException exception) {
			progressMonitor.cancel();
			LOG.error(() -> String.format("[%s] Easytrieve Macro: unable to parse content because of %s", file.getName(), exception.getMessage()), exception);
			return null;
		}
		
		if (macroDetectionVisitor.isMacroFile() && antlrMacroParser.getSyntaxErrors().size() <= FILE_DETECTION_ERROR_LIMIT) {
			/* return identity as macro (maybe because it won't be yes unless it's used for sure) */
			return new Identification(ID.MAYBE, file.getId(), ResolveTarget.EASYTRIEVE_MACRO_FILE, ResolveTarget.EASYTRIEVE);
		}

		return null;
	}
	
	/**
	 * Special parse method used just to identify a file as an easytrieve program or not. Will timeout according to config settings.
	 *
	 * @param file file to parse to see if it's an easytrieve program
	 * @param getMacroContent function to provide the content of a macro
	 * @param returnedMacroCalls the positive identified macros used by this program
	 * @return the {@link Identification} or {@code null}
	 */
	@Nullable
	public Identification checkIfEztProgram(final SourcePojo file, final UnaryOperator<String> getMacroContent, Set<MacroCall> returnedMacroCalls) {
		final EasytrieveDiscoveryFileDetectionVisitor fileDetectionVisitor = new EasytrieveDiscoveryFileDetectionVisitor();
		fileDetectionVisitor.setErrorLimit(FILE_DETECTION_ERROR_LIMIT + 1);
		final EasytrieveAntlrParser antlrParser = new EasytrieveAntlrParser()
															.setPreProcessor(new EasytrieveProgramPreProcessor())
															.setErrorListener(new BaseAntlrErrorListener());

		final CancellableParserProgressMonitor progressMonitor = new CancellableParserProgressMonitor();
		antlrParser.setParserProgressMonitor(progressMonitor);
		
		try {
			final AssembledEasytrieveProgram assembledProgram = worker.execute(
					() -> antlrParser.getPreAssembleEasytrieveProgram(file.getContent().toString(), getMacroContent),
					timeout, timeoutUnit, MessageProvider.from(file, ResolveTarget.EASYTRIEVE)
			);
			returnedMacroCalls.addAll(assembledProgram.getMacroCalls());
			final String assembledText = assembledProgram.getAssembledText();

			worker.execute(() -> {
				/* parse assembled text to determine if ezt program */
				antlrParser.parseTextWithVisitor(assembledText, fileDetectionVisitor);
				return null;
			}, timeout, timeoutUnit, MessageProvider.from(file, ResolveTarget.EASYTRIEVE));
			
			if (fileDetectionVisitor.isValidProgram() && antlrParser.getSyntaxErrors().isEmpty()) {
				return new Identification(ID.YES, file.getId(), ResolveTarget.EASYTRIEVE_PROGRAM, ResolveTarget.EASYTRIEVE);
			} else if (fileDetectionVisitor.isValidProgram() && antlrParser.getSyntaxErrors().size() <= FILE_DETECTION_ERROR_LIMIT) {
				return new Identification(ID.MAYBE, file.getId(), ResolveTarget.EASYTRIEVE_PROGRAM, ResolveTarget.EASYTRIEVE);
			} else {
				return null;
			}
		} catch (final WorkerCancellationException e) {
			progressMonitor.cancel();
			/* explicitly ignore the exception */
			LOG.debug(() -> String.format("[%s] Easytrieve: parsing cancelled due to timeout", file.getName()));
			return null;
		} catch (final WorkerException exception) {
			progressMonitor.cancel();
			LOG.error(() -> String.format("[%s] Easytrieve: unable to parse content because of %s", file.getName(), exception.getMessage()), exception);
			return null;
		}
	}

}
