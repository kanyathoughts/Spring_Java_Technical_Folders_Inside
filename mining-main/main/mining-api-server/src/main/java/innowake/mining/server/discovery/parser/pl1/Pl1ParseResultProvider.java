/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.pl1;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import innowake.mining.shared.model.ModuleType;
import org.apache.commons.lang.exception.ExceptionUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserProgressMonitor;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.ParserProgressMonitor;
import innowake.ndt.core.assembling.AssemblerConfiguration;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.core.parsing.pl1.Pl1LexerConfiguration;
import innowake.ndt.core.parsing.pl1.Pl1LexerConfiguration.Pl1LexerConfigurationBuilder;
import innowake.ndt.core.parsing.replacement.ReplacementMode;
import innowake.ndt.parsing.assembling.pl1.Pl1Assembler;
import innowake.ndt.parsing.base.producer.NodeProducer;
import innowake.ndt.parsing.base.token.ast.TokenBasedAstNode;
import innowake.ndt.parsing.parser.base.SimpleNodeRegistry;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.NamedVariableDeclaration;
import innowake.ndt.parsing.parser.pl1.PL1ParserConfiguration.PL1ParserConfigurationBuilder;
import innowake.ndt.parsing.parser.pl1.Pl1IncludeParser;
import innowake.ndt.parsing.parser.pl1.Pl1LightWeightParser;
import innowake.ndt.pl1parser.ast.model.Pl1Model;
import innowake.ndt.pl1parser.parser.ExecSqlParboiledNodeProducer;
import innowake.ndt.pl1parser.parser.PL1ParseConfiguration;
import innowake.ndt.pl1parser.parser.PL1ParseConfiguration.PL1ParseConfigurationBuilder;
import innowake.ndt.pl1parser.parser.Pl1NodeFactory;
import innowake.ndt.pl1parser.parser.Pl1Parser;
import innowake.ndt.pl1parser.parser.Pl1Rules;

/**
 * Parser for PL1. Supports configurable time boxing for parsing.
 */
public class Pl1ParseResultProvider extends AbstractCachingParseResultProvider<Pl1ParseResultProvider.Pl1ParseResult> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.PARSER_PARBOILED);
	private static final IAssemblingDataProvider<String> stringDataProvider = new Pl1StringBasedDataProvider();

	private final IAssemblingDataProvider<SourcePojo> dataProvider;
	private final Pl1LexerConfiguration pl1LexerConfiguration;
	private final Config config;
	
	
	/**
	 * Returns if a {@link NamedVariableDeclaration} is of a given type and not variable.
	 *
	 * @param dcl the {@link NamedVariableDeclaration}
	 * @param type the type
	 * @return {@code true} if the given @link NamedVariableDeclaration} is of the given type and not variable; {{@code  false} otherwise}
	 */
	public static boolean isNamedDeclarationOfType(final NamedVariableDeclaration dcl, final String type) {
		return ! dcl.isVariableAttribute() && dcl.getDataFormatTypes().contains(type);
	}
	
	/**
	 * Constructor.
	 * 
	 * @param config The {@link Config}
	 * @param worker The {@link TimedWorker}
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 * @param sourceObjectResolver the {@link SourceObjectResolver}
	 * @param featureMap the feature map
	 */
	public Pl1ParseResultProvider(final Config config, final TimedWorker worker,
			final String jobId, final ParseResultCacheService parseResultCacheService, final SourceObjectResolver sourceObjectResolver, final Map<FeatureId, Boolean> featureMap) {
		super(worker, config.getParserTimeout(ResolveTarget.PL1), jobId, parseResultCacheService);
		this.config = config;
		dataProvider = new Pl1DataProvider(sourceObjectResolver);
		pl1LexerConfiguration = new Pl1LexerConfigurationBuilder().setMargin(config.getPL1ParserMarginStart(), config.getPL1ParserMarginEnd(), config.getPL1ParserANS()).build();
	}

	/**
	 * Parse the source object if not present in cache and return parser result otherwise get it from cache. 
	 * 
	 * @param sourceObject to parse
	 * @return the parse result
	 * @throws DiscoveryException if the parsing fails
	 */
	@Override
	public Pl1ParseResult getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
			try {
				final Pl1ParseResult parseResult = getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.PL1), () -> new Pl1ParseResult(sourceObject, worker));
				parseResult.setTimedWorker(worker);
				return parseResult;
			} catch (WorkerCancellationException | DiscoveryException e) {
				throw new IllegalStateException("Exception while getting Pl1 parser results " + e);
			}
	}

	public class Pl1ParseResult {
		@Nullable
		private Pl1Model programModel;
		@Nullable
		private AstModel pl1LightweightModel;
		@Nullable
		private AstModel pl1IncludeModel;
		@Nullable
		private AstModel pl1ExternalCallsModel;
		@Nullable
		private Map<String, ModuleType> externalEntries;
		@Nullable
		private AstModel pl1internalCallsModel;
		
		private final SourcePojo sourceObject;
		@Nullable
		private IAssembling<SourcePojo> assembling;
		
		private TimedWorker worker;

		private Pl1ParseResult(final SourcePojo sourceObject, final TimedWorker worker) {
			this.sourceObject = sourceObject;
			this.worker = worker;
		}

		private void setTimedWorker(final TimedWorker worker) {
			this.worker = worker;
		}

		public final SourcePojo getSourceObject() {
			return sourceObject;
		}
		
		public final AstModel getPl1LightweightModel() throws DiscoveryException {
			if (pl1LightweightModel != null) {
				return pl1LightweightModel;
			}

			final CancellableParserProgressMonitor monitor = new CancellableParserProgressMonitor();
			final Pl1LightWeightParser<SourcePojo> lightweightParser = createLightweightParser(pl1LexerConfiguration, monitor);

			pl1LightweightModel = parse(() -> lightweightParser.parse(getAssembling()).orElseThrow(() -> new IllegalStateException("Could not create lightweight model for " + sourceObject.getName())),
					monitor,
					worker);

			return pl1LightweightModel;
		}
		
		public final AstModel getPl1IncludeModel() throws DiscoveryException {
			if (pl1IncludeModel != null) {
				return pl1IncludeModel;
			}

			final CancellableParserProgressMonitor monitor = new CancellableParserProgressMonitor();
			final BaseParserConfiguration<SourcePojo>lightWeightAssemblingDataProvider = new PL1ParserConfigurationBuilder<SourcePojo>().setPl1LexerConfiguration(pl1LexerConfiguration).setAssemblingDataProvider(dataProvider).build();
			lightWeightAssemblingDataProvider.setParserProgressMonitor(monitor);

			final Pl1IncludeParser<SourcePojo> pl1LightweightIncludeParser = new Pl1IncludeParser<>(lightWeightAssemblingDataProvider);
			pl1IncludeModel =  parse(() -> pl1LightweightIncludeParser.parse(sourceObject).orElseThrow(() -> new IllegalStateException("Could not create include model for " + sourceObject.getName())),
					monitor,
					worker);

			return pl1IncludeModel;
		}
		
		public final AstModel getPl1ExternalCallsModel() throws DiscoveryException {
			if (pl1ExternalCallsModel != null) {
				return pl1ExternalCallsModel;
			}

			final CancellableParserProgressMonitor monitor = new CancellableParserProgressMonitor();
			final BaseParserConfiguration<SourcePojo>lightWeightAssemblingDataProvider = new PL1ParserConfigurationBuilder<SourcePojo>().setPl1LexerConfiguration(pl1LexerConfiguration).setAssemblingDataProvider(dataProvider).build();
			lightWeightAssemblingDataProvider.setParserProgressMonitor(monitor);

			pl1ExternalCallsModel = parse(() -> new Pl1ExternalFunctionCallExtractor(lightWeightAssemblingDataProvider, getExternalEntries()).parse(getAssembling()).orElseThrow(() -> new IllegalStateException("Could not create external calls model for " + sourceObject.getName())),
					monitor,
					worker);

			return pl1ExternalCallsModel;
		}
		
		public final AstModel getPl1InternalCallsModel() throws DiscoveryException {
			if (pl1internalCallsModel != null) {
				return pl1internalCallsModel;
			}

			final CancellableParserProgressMonitor monitor = new CancellableParserProgressMonitor();
			final BaseParserConfiguration<SourcePojo>lightWeightAssemblingDataProvider = new PL1ParserConfigurationBuilder<SourcePojo>().setPl1LexerConfiguration(pl1LexerConfiguration).setAssemblingDataProvider(dataProvider).build();
			lightWeightAssemblingDataProvider.setParserProgressMonitor(monitor);

			pl1internalCallsModel = parse(() -> new Pl1InternalFunctionCallExtractor(lightWeightAssemblingDataProvider, getProcedureLabels()).parse(getAssembling()).orElseThrow(() -> new IllegalStateException("Could not create internal calls model for " + sourceObject.getName())),
					monitor,
					worker);

			return pl1internalCallsModel;
		}

		public IAssembling<SourcePojo> getAssembling() {
			if (assembling != null) {
				return assembling;
			}
			try {
				final Pl1Assembler<SourcePojo> assembler = new Pl1Assembler<>(dataProvider, config.getPL1ParserMarginStart(), config.getPL1ParserMarginEnd(), config.getPL1ParserANS());
				assembling = assembler.assemble(sourceObject, dataProvider.getSource(sourceObject), new AssemblerConfiguration(ReplacementMode.DETAILED_LOCATIONS, true));
				return assembling;
			} catch (final AssemblingException e) {
				throw new IllegalStateException("Could not assemble source code for " + sourceObject.getName(), e);
			}
		}
		
		public final Map<String, ModuleType> getExternalEntries() throws DiscoveryException {
			if (externalEntries != null) {
				return externalEntries;
			}
			final Optional<AstNode> root = getPl1LightweightModel().getRoot();
			if ( ! root.isPresent()) {
				externalEntries = Collections.emptyMap();
			} else {
				final List<NamedVariableDeclaration> childrenDeep = root.get()
						.getChildrenDeep(NamedVariableDeclaration.class, dcl -> isNamedDeclarationOfType((NamedVariableDeclaration) dcl, "ENTRY"));
				externalEntries = childrenDeep
						.stream()
						.collect(Collectors.toMap(NamedVariableDeclaration::getVariableName, entryName -> entryName.getDataFormatTypes().contains("RETURNS")
								? ModuleType.PL1_FUNCTION : ModuleType.PL1_SUBROUTINE));
			}

			return Objects.requireNonNull(externalEntries);
		}

		public Pl1Model getProgramModel() throws DiscoveryException {
			if (programModel == null) {
				final CancellableParserProgressMonitor monitor = new CancellableParserProgressMonitor();
				final Pl1Parser<String> pl1Parser = new Pl1Parser<>(createParserConfiguration(config, stringDataProvider, monitor));
				programModel = parse(
						() -> pl1Parser.parse(getAssembling().getAssembledContent()).orElseThrow(() -> 
						new IllegalStateException("Could not create pl1 model for " + sourceObject.getName())),
						monitor,
						worker);
			}

			return Assert.assertNotNull(programModel);
		}

		private Set<String> getProcedureLabels() throws DiscoveryException {
			final Optional<AstNode> root = getPl1LightweightModel().getRoot();
			if ( ! root.isPresent()) {
				return Collections.emptySet();
			}
			return root.get()
				.getChildrenDeep(AbstractPl1ProcedureNode.class).stream().flatMap(proc -> proc.getLabels().stream()).collect(Collectors.toSet());
		}

		private <T> PL1ParseConfiguration<T, Pl1Rules> createParserConfiguration(@Nullable final Config config, final IAssemblingDataProvider<T> dataProvider, final ParserProgressMonitor monitor) {
			final PL1ParseConfigurationBuilder<T, Pl1Rules> builder =
					new PL1ParseConfigurationBuilder<T, Pl1Rules>()
							.setAssemblingDataProvider(dataProvider)
							.setNodeFactory(new Pl1NodeFactory()).setRulesClass(Pl1Rules.class);

			if (config != null) {
				builder.setMargin(config.getPL1ParserMarginStart(), config.getPL1ParserMarginEnd())
						.setNotSymbols(config.getPL1ParserNotSymbol())
						.setOrSymbols(config.getPL1ParserOrSymbol());
			}

			final var parserConfig = builder.build();
			parserConfig.setParserProgressMonitor(monitor);

			return parserConfig;
		}

		private Pl1LightWeightParser<SourcePojo> createLightweightParser(
				final Pl1LexerConfiguration pl1LexerConfiguration,
				final ParserProgressMonitor monitor) {
			final PL1ParseConfigurationBuilder<String, Pl1Rules> builder = new PL1ParseConfigurationBuilder<String, Pl1Rules>()
							.setAssemblingDataProvider(stringDataProvider)
							.setNodeFactory(new Pl1NodeFactory())
							.setMargin(0, Integer.MAX_VALUE) /* Config for the EXEC SQL Parboiled parser, this only obtains a preprocessed String */
							.setRulesClass(Pl1Rules.class);

			if (config != null) {
				builder.setNotSymbols(config.getPL1ParserNotSymbol())
				.setOrSymbols(config.getPL1ParserOrSymbol());
			}
			
			final BaseParserConfiguration<SourcePojo> pl1Config = new PL1ParserConfigurationBuilder<SourcePojo>()
					.setPl1LexerConfiguration(pl1LexerConfiguration)
					.setAssemblingDataProvider(dataProvider)
					.setNodeRegistry(new SimpleNodeRegistry())
					.enableNodeLookup(true)
					.build();

			pl1Config.setParserProgressMonitor(monitor);

			return new Pl1LightWeightParser<SourcePojo>(pl1Config) {
				@Override
				protected List<NodeProducer<TokenBasedAstNode>> getProducers() {
					final List<NodeProducer<TokenBasedAstNode>> result = new ArrayList<>(PL1_DEFAULT_PRODUCERS);
					result.add(innowake.ndt.parsing.parser.pl1.producer.ExecSqlIncludeProducer.INSTANCE);
					result.add(new ExecSqlParboiledNodeProducer(builder.build()));
					return result;
				}
			};
		}

		private <T extends AstModel> T parse(final Callable<T> parsing, final CancellableParserProgressMonitor monitor, final TimedWorker worker) throws DiscoveryException {
			try {
				return worker.execute(parsing, timeout, UNIT, MessageProvider.from(ResolveTarget.PL1));
			} catch (final WorkerException e) {
				monitor.cancel();
				if (LOG.isDebugEnabled()) {
					LOG.debug("Parser error occured.", e);
					LOG.debug(ExceptionUtils.getFullStackTrace(e));
				}
				throw new DiscoveryException(e.getMessage(), e);
			}
		}
	}

}
