/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.ndt.core.assembling.natural.INaturalAssemblingTypes.ASSEMBLE_COPYCODES;
import static innowake.ndt.core.assembling.natural.INaturalAssemblingTypes.ASSEMBLE_DATA_AREAS_AGGRESSIVELY;

import java.util.List;
import java.util.Optional;

import org.apache.commons.lang.exception.ExceptionUtils;

import C.CLexer;
import C.CParser;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert.AssertionException;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.api.DataProvider;
import innowake.mining.data.core.api.DefaultModel;
import innowake.mining.data.core.api.Model;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Type;
import innowake.ndt.antlr.base.BaseAntlrErrorListener;
import innowake.ndt.antlr.base.BaseAntlrParseException;
import innowake.ndt.antlr.c.CAntlr2AstListener;
import innowake.ndt.antlr.c.CAntlrParser;
import innowake.ndt.antlr.c.CProgramPreProcessor;
import innowake.ndt.antlr.c.ast.CBindingResolver;
import innowake.ndt.assembler.model.ast.AssemblerModel;
import innowake.ndt.assembler.parser.AssemblerParser;
import innowake.ndt.assembler.parser.ParserConfiguration;
import innowake.ndt.bms.model.ast.BmsAstModel;
import innowake.ndt.bms.model.util.BmsAstUtils;
import innowake.ndt.cobol.parser.ast.CobolNodeRegistry;
import innowake.ndt.cobol.parser.ast.CobolParseConfiguration;
import innowake.ndt.cobol.parser.ast.CobolParserAst;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.core.assembling.AssemblerConfiguration;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.natural.NaturalAssembler;
import innowake.ndt.core.assembling.natural.NaturalAssemblerConfiguration;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.core.parsing.base.BindingConfiguration;
import innowake.ndt.core.parsing.base.GenericNodeRegistry;
import innowake.ndt.core.parsing.replacement.ReplacementMode;
import innowake.ndt.jcl.parser.api.JCLConfigParam;
import innowake.ndt.jcl.parser.api.JclLine;
import innowake.ndt.jcl.parser.assembling.JCLAssembler;
import innowake.ndt.jcl.parser.assembling.mvs.JCLMvsAssembler;
import innowake.ndt.jcl.parser.ast.JclAstConverter;
import innowake.ndt.jcl.parser.env.OSType;
import innowake.ndt.jcl.parser.model.JclContent;
import innowake.ndt.jcl.parser.model.JclModel;
import innowake.ndt.jcl.parser.parser.mvs.JCLMvsParser;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.NaturalModelParser;
import innowake.ndt.naturalparser.NaturalObjectType;
import innowake.ndt.naturalparser.model.NaturalAstModel;
import innowake.ndt.parsing.assembling.pl1.Pl1Assembler;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicParser;
import innowake.ndt.parsing.parser.java.JavaParserAst;
import innowake.ndt.parsing.parser.java.JavaParserConfiguration;
import innowake.ndt.parsing.parser.java.JavaParserConfiguration.JavaParserConfigurationBuilder;
import innowake.ndt.parsing.parser.java.model.JavaModel;
import innowake.ndt.pl1parser.ast.model.Pl1Model;
import innowake.ndt.pl1parser.parser.PL1ParseConfiguration;
import innowake.ndt.pl1parser.parser.Pl1NodeFactory;
import innowake.ndt.pl1parser.parser.Pl1Parser;
import innowake.ndt.pl1parser.parser.Pl1Rules;

/**
 * Provides the technology specific parse model in a language independent way.
 */
public class ModelProvider {
	
	/**
	 * SEE_THE_LOG_FOR_MORE_DETAILS.
	 */
	private static final String SEE_LOG_MSG = ". See the log for more details.";
	private static final Logger LOG = LoggerFactory.getLogger(ModelProvider.class);

	/**
	 * Returns a model given the module.
	 *
	 * @param module the module from which the model should be generated from
	 * @param dataProvider the data provider for the parser
	 * @return the parse model of the given module
	 */
	@Nullable
	public Model get(final ModulePojo module, final DataProvider dataProvider) {
		try {
			switch (module.getTechnology()) {
				case COBOL:
					return parseCobol(module, dataProvider);
				case CICS:
					return parseBms(module, dataProvider);
				case NATURAL:
					return parseNatural(module, dataProvider);
				case JCL:
					return parseJcl(module, dataProvider);
				case BASIC:
					return parseBasic(module, dataProvider);
				case PL1:
					return parsePl1(module, dataProvider);
				case C:
					return parseC(module);
				case JAVA:
					return parseJava(module,dataProvider);
				default:
					LOG.warn(() -> String.format("The module with ID %d has the unsupported technology '%s'", module.getId(), module.getTechnology()));
					return null;
			}
		} catch (final AssertionException e) {
			/* This is thrown by the COBOL parser when the parent/child hierarchy is not correct.
			 * As there are still a lot of those issues, we handle this separately. */
			LOG.debug(() -> String.format("%s Occured in module %d", e.getLocalizedMessage(), module.getId()), e);
			return null;
		} catch (final Exception e) {
			/* The parsers could throw any exception, so everything has to be caught. */
			final Throwable rootCause = ExceptionUtils.getRootCause(e);
			final String message = rootCause != null ? rootCause.getLocalizedMessage() : e.getLocalizedMessage();
			LOG.debug(() -> String.format("%s Occured in module %d", message, module.getId()), e);
			return null;
		}
	}
	
	private Model parseBms(final ModulePojo module, final DataProvider dataProvider) {
		final AssemblerParser<ModulePojo> asmParser = new AssemblerParser<>(
				new ParserConfiguration.Builder<ModulePojo>()
				.setAssemblingDataProvider(dataProvider)
				.build()
		);
		LOG.debug(() -> String.format("Parsing BMS Map: %s with the module ID %d", module.getName(), module.getId()));
		final AssemblerModel<ModulePojo> model = asmParser.parse(module);
		final BmsAstModel bmsModel = BmsAstUtils.convertToBMSModel(model);
		return new DefaultModel(bmsModel, new DummyIAssembling(module));
	}

	private Model parseCobol(final ModulePojo module, final DataProvider dataProvider) {
		final CobolParseConfiguration<ModulePojo> configuration =
				new CobolParseConfiguration.Builder<>(module, module.getContent().orElseThrow(), dataProvider).shouldValidateNodeHierarchy(true).build();
		configuration.setModuleName(module.getName());
		CobolNodeRegistry.getInstance().setEnabled(true);
		final CobolParserAst parser = new CobolParserAst(
				new MessageDecoratingLogger(message -> String.format("[%s (%d)]: %s",
						module.getName(), module.getId(), message), LOG));
		final IAssembling<ModulePojo> assembling;
		try {
			assembling = parser.assemble(configuration);
		} catch (final AssemblingException e) {
			throw new IllegalStateException(e);
		}
		final CobolParseConfiguration<ModulePojo> cobolParseConfiguration =
				new CobolParseConfiguration.Builder<>(module, module.getContent().orElseThrow(), dataProvider)
						.shouldValidateNodeHierarchy(true)
						.setAssembling(assembling)
						.build();
		cobolParseConfiguration.setModuleName(module.getName());
		final CobolModel parseModel = parser.parse(assertNotNull(cobolParseConfiguration));
		return new DefaultModel(parseModel, assembling);
	}
	
	private Model parseNatural(final ModulePojo module, final DataProvider dataProvider) throws AssemblingException {
		GenericNodeRegistry.getInstance().enableNodeLookup(true);

		final String mainModuleContent = module.getContent().orElse(null);
		final NaturalAssembler<ModulePojo> assembler = new NaturalAssembler<>(new NaturalAssemblingDataProvider(dataProvider));

		/* process main content. */
		final NaturalAssemblerConfiguration assemblerConfig =
				new NaturalAssemblerConfiguration(ReplacementMode.FAST_LOCATIONS, false, ASSEMBLE_COPYCODES | ASSEMBLE_DATA_AREAS_AGGRESSIVELY);
		final IAssembling<ModulePojo> assembling = assembler.assemble(module, mainModuleContent, assemblerConfig);
		final INaturalModel model = new NaturalModelParser(true, null, null, assembler, module)
				.parse(module.getName(), mapType(module.getType()), assembling.getAssembledContent(), false, false, assembling);

		return new DefaultModel(NaturalAstModel.from(model, BindingConfiguration.RESOLVE_ALL), assembling);
	}

	private Model parseJcl(final ModulePojo module, final DataProvider dataProvider) {
		final JCLConfigParam properties = new JCLConfigParam();
		final JCLMvsAssembler assembler = new JCLMvsAssembler(new JCLMvsParser(), true, properties);
		LOG.debug(() -> String.format("Assembling  module %s with the ID %d", module.getName(), module.getId()));
		final JclContent assembledContent = assembler.execute(JCLAssembler.split(module.getContent().orElse(null), OSType.MVS), dataProvider, module.getName());
		LOG.trace(() -> String.format("Assembled content for module %s with the ID %d:%n%s", module.getName(), module.getId(), assembledContent.getContent()));
		final List<JclLine> jclLines = JCLAssembler.split(module.getContent().orElse(null), assembler.getOsType());
		final JCLMvsParser parser = new JCLMvsParser(assembler.getJclErrors());
		final JclModel model = JclAstConverter.createAstModel(parser.parse(properties, assembledContent),
				new JclContent(jclLines, module.getName()));
		return new DefaultModel(model, new DummyIAssembling(module));
	}
	
	private Model parseJava(final ModulePojo module, final DataProvider dataProvider) {
		final JavaParserConfiguration<ModulePojo> javaConfig =
				new JavaParserConfigurationBuilder<ModulePojo>().setAssemblingDataProvider(dataProvider).build();
		javaConfig.setModuleName(module.getName());
		final JavaModel parseModel = new JavaParserAst<>(javaConfig).parse(module)
				.orElseThrow(() -> new IllegalStateException("Could not parse Java module " + module.getName() + SEE_LOG_MSG));
		return new DefaultModel(parseModel, new DummyIAssembling(module));
	}
	
	private Model parseBasic(final ModulePojo module, final DataProvider dataProvider) {
		final BaseParserConfiguration<ModulePojo> basicParserConfiguration = new BaseParserConfiguration.Builder<ModulePojo>()
		.setAssemblingDataProvider(dataProvider).build();
		final BasicParser<ModulePojo> parser = new BasicParser<>(basicParserConfiguration);
		final Optional<BasicModel> parseModel = parser.parse(module);
		return new DefaultModel(parseModel
				.orElseThrow(() -> new IllegalStateException("Could not parse Basic module " + module.getName() + SEE_LOG_MSG)),
				new DummyIAssembling(module));
	}
	
	private Model parsePl1(final ModulePojo module, final DataProvider dataProvider) throws AssemblingException {
		final Pl1Assembler<ModulePojo> assembler = new Pl1Assembler<>(dataProvider);
		final IAssembling<ModulePojo> assembling = assembler.assemble(module, new AssemblerConfiguration(ReplacementMode.FAST_LOCATIONS, false));
		final PL1ParseConfiguration<ModulePojo, Pl1Rules> configuration = new PL1ParseConfiguration.PL1ParseConfigurationBuilder<ModulePojo, Pl1Rules>()
				.setAssemblingDataProvider(dataProvider)
				.setNodeFactory(new Pl1NodeFactory())
				.setRulesClass(Pl1Rules.class)
				.setAssemble(true)
				.setNodeBindingConfiguration(BindingConfiguration.RESOLVE_ALL)
				.build();
		final Pl1Parser<ModulePojo> parser = new Pl1Parser<>(configuration);
		final Pl1Model parseModel = parser.parse(module, assembling)
				.orElseThrow(() -> new IllegalStateException("Could not parse PL/1 module " + module.getName() + SEE_LOG_MSG));
		return new DefaultModel(parseModel, assembling);
	}
	
	private Model parseC(final ModulePojo module) throws BaseAntlrParseException {
		final CAntlrParser antlrParser = new CAntlrParser().setPreProcessor(new CProgramPreProcessor()).setErrorListener(new BaseAntlrErrorListener());
		final CLexer lexer = antlrParser.tokenizeText(module.getContent().orElse(null));
		final CParser cParser = antlrParser.setupParser(lexer);
		final AstModel parseModel = antlrParser.parse(cParser, new CAntlr2AstListener()).getAstModel();
		final CBindingResolver bindingResolver = new CBindingResolver();
		bindingResolver.resolveBindingsForAstModel(parseModel, BindingConfiguration.RESOLVE_ALL);
		return new DefaultModel(parseModel, new DummyIAssembling(module));
	}
	
	private static NaturalObjectType mapType(final Type type) {
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
}

