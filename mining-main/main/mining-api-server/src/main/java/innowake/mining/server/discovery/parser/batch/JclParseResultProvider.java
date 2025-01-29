/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.batch;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.AstType;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.jcl.JclLexerFactory;
import innowake.ndt.core.parsing.jcl.ast.JclLightweightParser;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.jcl.parser.api.JCLConfigParam;
import innowake.ndt.jcl.parser.api.JclLine;
import innowake.ndt.jcl.parser.assembling.IContentProvider;
import innowake.ndt.jcl.parser.assembling.JCLAssembler;
import innowake.ndt.jcl.parser.assembling.JCLBlockParser;
import innowake.ndt.jcl.parser.assembling.mvs.JCLMvsAssembler;
import innowake.ndt.jcl.parser.env.JclError;
import innowake.ndt.jcl.parser.env.OSType;
import innowake.ndt.jcl.parser.model.JCL;
import innowake.ndt.jcl.parser.model.JclContent;
import innowake.ndt.jcl.parser.model.StepExec;
import innowake.ndt.jcl.parser.model.StepIf;
import innowake.ndt.jcl.parser.parser.mvs.JCLMvsParser;

/**
 * A caching parser result provider for {@code Batch} modules.
 */
public class JclParseResultProvider extends AbstractCachingParseResultProvider<JclParseResultProvider.JclParseResult> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.JCL_PARSER);

	private final JclLightweightParser<SourcePojo> parserLightweight;
	private final JCLConfigParam configParam = new JCLConfigParam();
	private final IContentProvider provider;
	
	/**
	 * Constructor.
	 * 
	 * @param config The {@link Config}
	 * @param worker The {@link TimedWorker}
	 * @param provider The discovery {@link IContentProvider} for JCL
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public JclParseResultProvider(final Config config, final TimedWorker worker, final IContentProvider provider, final String jobId, 
			final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.JCL), jobId, parseResultCacheService);
		this.configParam.setStrictMode(false);
		parserLightweight = JclLightweightParser.createParser(AstType.DEPENDENCY, new JclParserProvider());
		this.provider = provider;
	}

	/**
	 * Returns the {@link ITokenPartitioning} for the given {@code content}
	 * 
	 * @param content The source content.
	 * @return The {@link ITokenPartitioning}
	 */
	public static ITokenPartitioning getPartitioning(final String content) {
		return TokenPartitioner2.create(JclLexerFactory.get()).doPartitioning(new StringContent(content));
	}

	/**
	 * Returns the {@link IAst} for the given {@code file}
	 * 
	 * @param file The {@link SourcePojo} file.
	 * @return The {@link IAst}
	 */
	public IAst<SourcePojo> getLightweight(final SourcePojo file) {
		return parserLightweight.parse(file);
	}

	/**
	 * Returns the {@link JclParseResult} for the given {@code sourceObject}
	 * 
	 * @param sourceObject The {@link SourcePojo}
	 * @return The {@link JclParseResult}
	 * @throws WorkerCancellationException when the worker thread was cancelled
	 * @throws DiscoveryException when the worker thread was interrupted or threw an error
	 */
	@Override
	public final JclParseResult getParseResult(final SourcePojo sourceObject) throws DiscoveryException, WorkerCancellationException {
		return getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.JCL),
					() -> {
						final JCLMvsAssembler jclAssembler = new JCLMvsAssembler(new JCLMvsParser(), true, configParam);
						LOG.trace(() -> String.format("Parsing and assembling JCL source object %s with ID %s", sourceObject.getPath(), sourceObject.getId()));
						final List<JclLine> jclLines = JCLAssembler.split(sourceObject.getContent().toString(), OSType.MVS);
					return new JclParseResult(sourceObject, jclAssembler.execute(jclLines, provider,
							JCLBlockParser.Type.fromName(sourceObject.getType().name()), FilenameUtils.getBaseName(sourceObject.getPath())),
							jclAssembler.getJclErrors());
					});
	}

	/**
	 * Returns the key for the given {@code sourceObject}.
	 * <p>The implementation additionally adds the IContentProvider.hashcode() to the path and job id.</p>
	 * <pre>jobId + "@" + sourceObject.getPath() + "$" + hashCode</pre>
	 *
	 * @param sourceObject The {@link SourcePojo} for calculating the key for caching
	 * @return the cache key
	 */
	@Override
	protected String getKey(final SourcePojo sourceObject) {
		return super.getKey(sourceObject) + "$" + provider.hashCode();
	}

	/**
	 * The parse result of a {@code Batch} {@link SourcePojo}
	 */
	public final class JclParseResult {

		private final SourcePojo sourceObject;
		private final JclContent jclContents;
		private final List<JclError> assemblerErrors;

		@Nullable
		private List<JCL> fullModel;

		private JclParseResult(final SourcePojo sourceObject, final JclContent jclContent, final List<JclError> assemblerErrors) {
			this.sourceObject = sourceObject;
			this.jclContents = jclContent;
			this.assemblerErrors = assemblerErrors;
		}

		/**
		 * @return the assembled {@link JclError}s of the parsed {@code Batch} {@link SourcePojo}.
		 */
		public List<JclError> getAssembledJCLErrors() {
			return assemblerErrors;
		}

		/**
		 * @return the assembled content of the parsed {@code Batch} {@link SourcePojo}.
		 */
		public String getJclContents() {
			return jclContents.getContent();
		}

		/**
		 * @return the list {@link JCL}s
		 * @throws WorkerCancellationException when parsing was necessary and the parser worker thread was cancelled
		 * @throws DiscoveryException when parsing was necessary and the worker thread was interrupted or threw an error
		 */
		public List<JCL> getFull() throws DiscoveryException, WorkerCancellationException {
			if (fullModel == null) {
				fullModel = getFull(sourceObject, jclContents);
			}

			return Assert.assertNotNull(fullModel);
		}

		/**
		 * Returns the list {@link JCL}s for the given {@code sourceObject}
		 *
		 * @param sourceObject The {@link SourcePojo}
		 * @param jclContents the {@link JclContent}
		 * @return The list {@link JCL}s
		 * @throws WorkerCancellationException when the worker thread was cancelled
		 * @throws DiscoveryException when the worker thread was interrupted or threw an error
		 */
		private List<JCL> getFull(final SourcePojo sourceObject, final JclContent jclContents) throws DiscoveryException, WorkerCancellationException {
			try {
				return worker.execute(() -> {
					final List<JCL> parseResult = new JCLMvsParser(assemblerErrors).parse(configParam, jclContents);
					if (parseResult.size() > 1) {
						/* Find the first valid JCL to append the steps to it, this is to avoid appending the steps to the
						 * invalid JCL (JCL with type null/Error JCL) if it occurs first */
						final Optional<JCL> firstValidJCLOp = parseResult.stream().filter(jcl -> jcl.getType() != null).findFirst();
						final List<JCL> result = new ArrayList<>();
						final List<JCL> errorJCLList = parseResult.stream().filter(jcl -> ! jcl.getJclErrors().isEmpty()).collect(Collectors.toList());
						/* Adds the erroneous JCL if any to the result to collect it as errors in the final result */
						result.addAll(errorJCLList);

						if (firstValidJCLOp.isPresent()) {
							/* If the parse result contains more than 1 entry, then it means that this file contains more than 1 Job.
							 * Currently, Discovery can not handle this. Therefore, we collect all steps that are _not_ on the first job,
							 * attach them to the first job, and return only the first job. This makes it appear as if there is only one job
							 * in the source file, which contains all the steps from all jobs. */
							final JCL firstValidJCL = firstValidJCLOp.get();
							parseResult.stream().flatMap(JobControl::getJCLStepsRecursively)
									.filter(step -> step instanceof StepExec || step instanceof StepIf)
									.filter(step -> step.getParent() instanceof JCL && step.getParent() != firstValidJCL)
									.forEach(step -> {
										firstValidJCL.addStep(step);
										firstValidJCL.getAllSteps().add(step);
									});
							result.add(firstValidJCL);
							return result;
						}
						return result;
					}
					return parseResult;
				}, timeout, UNIT, MessageProvider.from(sourceObject, ResolveTarget.JCL));
			} catch (final WorkerCancellationException e) {
				throw e;
			} catch (final WorkerException exception) {
				throw new DiscoveryException(exception.getMessage(), exception);
			}
		}
	}
}
