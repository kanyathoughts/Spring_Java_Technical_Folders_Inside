/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */

package innowake.mining.server.discovery.dawn.metrics.contributors.assembler;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstInputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.input.MetricInputProvider;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.assembler.AssemblerParserResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.assembler.model.ast.instruction.MacroInstruction;
import innowake.ndt.assembler.model.ast.instruction.UnknownInstruction;
import innowake.ndt.assembler.model.ast.instruction.hlasm.HlasmInstructionCopy;
import innowake.ndt.assembler.model.ast.operand.KeywordOperand;
import innowake.ndt.assembler.model.ast.operand.MacroOperand;
import innowake.ndt.assembler.model.ast.operand.MacroOptionOperand;
import innowake.ndt.assembler.model.ast.operand.SubList;
import innowake.ndt.assembler.model.ast.operand.expression.OrdinarySymbol;
import innowake.ndt.core.parsing.assembler.AssemblerLexerConfiguration;
import innowake.ndt.core.parsing.assembler.AssemblerLexerFactory;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;

@Component
public class AssemblerContributor implements DiscoveryContributorFromSource {

	private static final Set<String> MACRO_COMMANDS_TO_IGNORE = Set.of("WTO", "ABEND", "GETMAIN", "FREEMAIN");

	@Autowired
	private ParserProviderService parserProvider;

	private static final Logger LOG = LoggerFactory.getLogger(AssemblerContributor.class);

	@Override public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.ASSEMBLER;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.fromTechnologyAndType(Technology.ASSEMBLER, 
				sourceObject.getType())).deferAction("addDependencyForUnknownInstruction");
		final Optional<AstModel> optionalAstModel = parse(context, sourceObject, rootModule);
		calculateSourceMetrics(sourceObject, rootModule, optionalAstModel);
		if (optionalAstModel.isEmpty()) {
			return;
		}
		
		final var astModel = optionalAstModel.get();
		final Optional<AstNode> optionalRootAstNode = astModel.getRoot();
		if (optionalRootAstNode.isEmpty()) {
			LOG.error("Error while parsing " + sourceObject.getPath() + ": No root found.");
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Unable to parse Assembler File. No root found.");
			/* root is missing (parse error) so nothing more to discover */
			return;
		}
		final var rootAstNode = optionalRootAstNode.get();
		addDependencyForHlasmInstructionCopy(rootModule, rootAstNode);
		addDependencyForMacroOptionOperand(rootModule, rootAstNode);
	}

	private void calculateSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder rootModule, final Optional<AstModel> optionalAstModel) {
		final var iLexerFactory = AssemblerLexerFactory.get(AssemblerLexerConfiguration.DEFAULT);
		final MetricInputProvider inputProvider = optionalAstModel.isPresent() ? new AstInputProvider(sourceObject, iLexerFactory, optionalAstModel.get())
				: new InputProvider(sourceObject, iLexerFactory);
		final var metricsContributor = new GenericMetricsContributor(inputProvider);
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY_FLAT));

		try {
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}

	private void addDependencyForHlasmInstructionCopy(final ModuleBuilder rootModule, final AstNode astNode) {
		/* Calculating the dependency metrics for all HlasmInstructionCopy which are instances of OrdinarySymbol */
		astNode.getChildrenDeep(HlasmInstructionCopy.class).stream()
				.map(HlasmInstructionCopy::getMember)
				.filter(OrdinarySymbol.class::isInstance)
				.map(OrdinarySymbol.class::cast)
				.forEach(ordinarySymbol -> addDependency(rootModule, ordinarySymbol.getName(), ordinarySymbol.getStartOffset(), ordinarySymbol.getLength(), Optional.empty()));
	}
	
	private Optional<AstModel> parse(final DiscoveryContext context, final SourcePojo sourceObject, final ModuleBuilder rootModule) {
		final AssemblerParserResultProvider parserResultProvider = parserProvider.createAssemblerParser(context);
		try {
			return Optional.of(parserResultProvider.getParseResult(sourceObject));
		} catch (final DiscoveryException e) {
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
			return Optional.empty();
		} catch (final Exception e) {
			LOG.error("Exception occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
			return Optional.empty();
		} catch (final Throwable e) {
			LOG.error("Unxpected error occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
			return Optional.empty();
		}
	}

	private void addDependencyForMacroOptionOperand(final ModuleBuilder rootModule, final AstNode astNode) {
		final List<MacroOptionOperand> macroOptionOperandList = new ArrayList<>();

		/* Calculating the dependency metrics for all MacroInstruction which are instances of MacroOptionOperand and KeywordOperand */
		astNode.getChildrenDeep(MacroInstruction.class).stream()
				.filter(macroInstruction -> ! macroInstruction.getOperands().isEmpty())
				/* Filtering the macro commands that need to be ignored which will be present in the macroInstruction.getStartToken() */
				.filter(macroInstruction -> ! MACRO_COMMANDS_TO_IGNORE.contains(macroInstruction.getStartToken().toString()))
				.map(macroInstruction -> macroInstruction.getOperands().get(0))
				.filter(Optional::isPresent)
				.map(Optional::get)
				.forEach(macroOperand -> {
					if (macroOperand instanceof MacroOptionOperand) {
						macroOptionOperandList.add((MacroOptionOperand) macroOperand);
					} else if (macroOperand instanceof KeywordOperand) {
						final Optional<MacroOperand> keywordMacroOperand = ((KeywordOperand) macroOperand).getValue();
						if (keywordMacroOperand.isPresent()) {
							if (keywordMacroOperand.get() instanceof MacroOptionOperand) {
								macroOptionOperandList.add((MacroOptionOperand) keywordMacroOperand.get());
							} else if (keywordMacroOperand.get() instanceof SubList) {
								((SubList) keywordMacroOperand.get()).getOperands().stream()
										.filter(Optional::isPresent)
										.map(Optional::get)
										.filter(MacroOptionOperand.class::isInstance)
										.map(MacroOptionOperand.class::cast)
										.forEach(macroOptionOperandList::add);
							}
						}
					}
				});

		macroOptionOperandList.forEach(macroOptionOperand -> addDependency(rootModule, macroOptionOperand.getEntry().getContent(),
				macroOptionOperand.getStartOffset(), macroOptionOperand.getLength(), Optional.empty()));
	}

	@DeferredAction
	public void addDependencyForUnknownInstruction(final ModuleBuilder rootModule, final DiscoveryContext context, final SourcePojo sourceObject) {
		final Optional<AstModel> optionalAstModel = parse(context, sourceObject, rootModule);
		if (optionalAstModel.isEmpty()) {
			return;
		}
		final var astModel = optionalAstModel.get();
		final Optional<AstNode> optionalRootAstModel = astModel.getRoot();
		if (optionalRootAstModel.isEmpty()) {
			return;
		}
		/* Calculating the dependency metrics for all UnknownInstruction which are instances of OrdinarySymbol */
		optionalRootAstModel.get().getChildren(UnknownInstruction.class)
				.forEach(unknownInstruction -> unknownInstruction.getChildren(OrdinarySymbol.class)
						.forEach(ordinarySymbol -> {
							if ( ! ordinarySymbol.getName().equalsIgnoreCase(sourceObject.getName())) {
								addDependency(rootModule, ordinarySymbol.getName(), ordinarySymbol.getStartOffset(), ordinarySymbol.getLength(), Optional
										.of(new ModuleFilter().setNames(ordinarySymbol.getName())
												.setTypes(ModuleType.ASSEMBLER_PROGRAM, ModuleType.ASSEMBLER_MACRO)));
							}
						}));
	}

	private void addDependency(final ModuleBuilder rootModule, final String name, final int offset, final int length, final Optional<ModuleFilter> moduleFilter) {
	final var dependencyFilter = moduleFilter.orElse(new ModuleFilter()
			.setNames(name));
		rootModule.declareDependency(RelationshipType.REFERENCES, dependencyFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.setBinding(Binding.LATE)
				.setLocation(new ModuleLocation(offset, length));
	}
}
