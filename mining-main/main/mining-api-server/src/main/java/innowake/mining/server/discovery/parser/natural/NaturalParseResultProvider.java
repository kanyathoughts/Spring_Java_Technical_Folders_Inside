/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.natural;

import innowake.lib.core.lang.Assert;
import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserProgressMonitor;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.natural.NaturalAssembler;
import innowake.ndt.core.assembling.natural.NaturalAssemblerConfiguration;
import innowake.ndt.core.parsing.natural.INaturalParseResult;
import innowake.ndt.core.parsing.natural.NaturalLightweightParsing;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.INaturalObjectResolver;
import innowake.ndt.naturalparser.NaturalModelParser;
import innowake.ndt.naturalparser.NaturalObjectType;
import innowake.ndt.naturalparser.NaturalSystemContext;
import innowake.ndt.naturalparser.NaturalVersion;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;

import static innowake.ndt.core.assembling.natural.INaturalAssemblingTypes.ASSEMBLE_COPYCODES;
import static innowake.ndt.core.assembling.natural.INaturalAssemblingTypes.ASSEMBLE_DATA_AREAS_AGGRESSIVELY;
import static innowake.ndt.core.parsing.replacement.ReplacementMode.FAST_LOCATIONS;

/**
 * A caching parser result provider for {@code Natural} modules.
 */
public class NaturalParseResultProvider extends AbstractCachingParseResultProvider<NaturalParseResultProvider.NaturalParseResult> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.NATURAL_PARSER);

	private static final NaturalAssemblerConfiguration ASSEMBLER_CONFIG = new NaturalAssemblerConfiguration(FAST_LOCATIONS, false, ASSEMBLE_COPYCODES);
	private static final NaturalAssemblerConfiguration ASSEMBLER_CONFIG_DA = new NaturalAssemblerConfiguration(FAST_LOCATIONS, true, ASSEMBLE_DATA_AREAS_AGGRESSIVELY);
	private static final NaturalVersion VERSION = new NaturalVersion(NaturalVersion.OS_HOST, 4, 2);
	private static final int MAX_LINE_SIZE = 249;
	private static final char DEFAULT_DECIMAL_CHAR = NaturalSystemContext.ALT1_DC;
	private static final char INPUT_DELIMITER = NaturalSystemContext.ALT1_ID;

	private final SourceObjectResolver sourceObjectResolver;
	private final Config config;

	/**
	 * Constructor.
	 * 
	 * @param sourceObjectResolver The {@link SourceObjectResolver} for the {@link NaturalAssembler}
	 * @param config The {@link Config}
	 * @param worker The {@link TimedWorker}
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 */
	public NaturalParseResultProvider(final SourceObjectResolver sourceObjectResolver,
										final Config config, final TimedWorker worker, final String jobId,
										final ParseResultCacheService parseResultCacheService) {
		super(worker, config.getParserTimeout(ResolveTarget.NATURAL), jobId, parseResultCacheService);
		this.sourceObjectResolver = sourceObjectResolver;
		this.config = config;
	}

	@Override
	public NaturalParseResult getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			final NaturalParseResult parseResult = getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.NATURAL), () -> new NaturalParseResult(sourceObject, worker));
			parseResult.setTimedWorker(worker);
			return parseResult;
		} catch (final WorkerCancellationException exception) {
			throw new IllegalStateException("Exception while getting Natural parser results", exception);
		}
	}

	static NaturalObjectType mapType(final Type type) {
		switch (type) {
			case SUBPROGRAM:
				return NaturalObjectType.TYPE_SUBPROGRAM;
			case PROGRAM:
				return NaturalObjectType.TYPE_PROGRAM;
			case COPYCODE:
				return NaturalObjectType.TYPE_COPYCODE;
			case HELP:
				return NaturalObjectType.TYPE_HELP;
			case DDM:
				return NaturalObjectType.TYPE_DDM;
			case SUBROUTINE:
				return NaturalObjectType.TYPE_SUBROUTINE;
			case PDA:
				return NaturalObjectType.TYPE_PDA;
			case GDA:
				return NaturalObjectType.TYPE_GDA;
			case LDA:
				return NaturalObjectType.TYPE_LDA;
			case ADAPTER:
				return NaturalObjectType.TYPE_UNKNOWN;
			case ADAPTVIEW:
				return NaturalObjectType.TYPE_ADAPTVIEW;
			case CLASS:
				return NaturalObjectType.TYPE_CLASS;
			case CPM:
				return NaturalObjectType.TYPE_CPM;
			case DIALOG:
				return NaturalObjectType.TYPE_DIALOG;
			case DIALOG_PRIV_RES:
				return NaturalObjectType.TYPE_UNKNOWN;
			case ERROR_MESSAGE:
				return NaturalObjectType.TYPE_UNKNOWN;
			case FUNCTION:
				return NaturalObjectType.TYPE_FUNCTION;
			case MAP:
				return NaturalObjectType.TYPE_MAP;
			case TEXT:
				return NaturalObjectType.TYPE_TEXT;
			default:
				return NaturalObjectType.TYPE_PROGRAM;
		}
	}

	private Tuple2<INaturalModel, INaturalParseResult> parse(final Callable<Tuple2<INaturalModel, INaturalParseResult>> parsing, final CancellableParserProgressMonitor monitor, final TimedWorker worker) throws DiscoveryException {
		try {
			return worker.execute(
					parsing,
					timeout, UNIT, MessageProvider.from(ResolveTarget.NATURAL));
		} catch (final WorkerException e) {
			monitor.cancel();
			if (LOG.isDebugEnabled()) {
				LOG.debug("Parser error occured.", e);
				LOG.debug(ExceptionUtils.getFullStackTrace(e));
			}
			throw new DiscoveryException(e.getMessage(), e);
		}
	}

	private NaturalSystemContext createSystemContext() {
		final char decimalChar;
		if (StringUtils.isEmpty(config.getNaturalDecimalChar()) || config.getNaturalDecimalChar().length() > 1) {
			decimalChar = DEFAULT_DECIMAL_CHAR;
		} else {
			decimalChar = config.getNaturalDecimalChar().charAt(0);
		}
		final char inputDelimiter;
		if (StringUtils.isEmpty(config.getNaturalInputDelimiter()) || config.getNaturalInputDelimiter().length() > 1) {
			inputDelimiter = INPUT_DELIMITER;
		} else {
			inputDelimiter = config.getNaturalInputDelimiter().charAt(0);
		}

		return new NaturalSystemContext(VERSION, decimalChar, inputDelimiter, MAX_LINE_SIZE);
	}

	/**
	 * An Natural parser result which creates {@link INaturalModel INaturalModels} when required.
	 */
	public class NaturalParseResult {

		private final SourcePojo sourceObject;
		@Nullable
		private Tuple2<INaturalModel, INaturalParseResult> parseResult;
		private Optional<DataArea> dataArea = Optional.empty();
		private TimedWorker worker;

		private NaturalParseResult(final SourcePojo sourceObject, final TimedWorker worker) {
			this.sourceObject = sourceObject;
			this.worker = worker;
		}

		private void setTimedWorker(final TimedWorker worker) {
			this.worker = worker;
		}

		private Tuple2<INaturalModel, INaturalParseResult> parse(final SourcePojo sourceObject, @Nullable final INaturalObjectResolver resolver, final NaturalAssemblerConfiguration assemblerConfig) throws DiscoveryException {
			final CancellableParserProgressMonitor monitor = new CancellableParserProgressMonitor();
			return NaturalParseResultProvider.this.parse(() -> {
				final NaturalAssembler<SourcePojo> assembler = new NaturalAssembler<>(new AssemblingDataProvider(sourceObjectResolver));

				final NaturalSystemContext context = createSystemContext();
				final NaturalModelParser parser = new NaturalModelParser(true, context, resolver, assembler, sourceObject);
				parser.forceSlashPreprocessing(true);
				parser.setParserProgressMonitor(monitor);

				final IAssembling<SourcePojo> assembling = assembler.assemble(sourceObject, sourceObject.getContent().toString(), assemblerConfig);
				final String content = assembling.getAssembledContent();

				final INaturalModel heavyWeightModel = parser.parse(sourceObject.getPath(), mapType(sourceObject.getType()), content,true, true, assembling);
				final INaturalParseResult lightWeightModel = NaturalLightweightParsing.computeNaturalTokens(content);

				return Tuple2.of(heavyWeightModel, lightWeightModel);
			},
			monitor,
			worker);
		}

		/**
		 * @return the {@link INaturalModel} of the parsed {@link SourcePojo}
		 * @throws DiscoveryException if a parser error occurred
		 */
		public INaturalModel getHeavyweightModel() throws DiscoveryException {
			if (parseResult == null) {
				parseResult = parse(sourceObject, createObjectResolver(sourceObject), ASSEMBLER_CONFIG);
			}
			return Assert.assertNotNull(parseResult).a;
		}
		
		/**
		 * @return the {@link INaturalParseResult} of the parsed {@link SourcePojo}
		 * @throws DiscoveryException if a parser error occurred
		 */
		public INaturalParseResult getLightweightModel() throws DiscoveryException {
			if (parseResult == null) {
				parseResult = parse(sourceObject, createObjectResolver(sourceObject), ASSEMBLER_CONFIG);
			}
			return Assert.assertNotNull(parseResult).b;
		}

		/**
		 * Optimized method to parse the define data block. Returns the {@link DataArea}, if the define data block present.
		 * <p>This method is not thread safe.</p>
		 *
		 * @return the parsed Natural object model of the define data block
		 * @throws DiscoveryException if a parser error occurred
		 */
		public Optional<DataArea> getDataArea() throws DiscoveryException {
			if ( ! dataArea.isPresent()) {
				if (parseResult == null) {
					parseResult = parse(sourceObject, null, ASSEMBLER_CONFIG_DA);
				}

				final DefineDataParser ddParser = new DefineDataParser(createSystemContext(), Objects.requireNonNull(parseResult).a);
				try {
					dataArea = ddParser.parse();
					if ( ! dataArea.isPresent() && LOG.isDebugEnabled()) {
						LOG.debug(() -> String.format("%s: No data definition found for %s", sourceObject.getPath(), FilenameUtils.getBaseName(sourceObject.getPath())));
					}
				} catch (final Exception exception) {
					dataArea = Optional.empty();
					LOG.error(() -> String.format("%s: Error while parsing data definition of %s", sourceObject.getPath(), FilenameUtils.getBaseName(sourceObject.getPath())), exception);
				}
			}

			return dataArea;
		}

		private INaturalObjectResolver createObjectResolver(final SourcePojo natObject) {
			return (final String name, @Nullable final NaturalObjectType naturalObjectType) -> {
				final Type type;
				if (naturalObjectType == null) {
					type = null;
				} else {
					final String suffix = naturalObjectType.getFileSuffix();
					final Set<ResolveTarget> targets = ResolveTargetHelper.getResolveTargets(suffix);
					if (targets.isEmpty()) {
						throw new IllegalStateException("Unknown Natural file extension " + suffix + " for object " + name);
					} else if (targets.size() > 1) {
						throw new IllegalStateException("Multiple types found for file extension " + suffix + " object " + name);
					}

					final ResolveTarget target = targets.iterator().next();
					type = ResolveTargetHelper.toType(target);
				}

				final SourcePojo resolveObject = sourceObjectResolver.resolveObject(natObject, name, new SourceObjectMatcher(Technology.NATURAL, type));
				return resolveObject == null ? null : resolveObject.getContent().toString();
			};
		}
	}
}
