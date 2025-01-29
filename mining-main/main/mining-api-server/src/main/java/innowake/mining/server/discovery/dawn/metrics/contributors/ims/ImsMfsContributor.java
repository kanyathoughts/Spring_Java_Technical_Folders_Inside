/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.ims;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.AnchorToBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.DiscoveryPersistence;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;
import innowake.ndt.mfsparser.ast.model.MfsModel;
import innowake.ndt.mfsparser.ast.model.statement.MfsStatementLabel;
import innowake.ndt.mfsparser.ast.model.statement.common.operand.MfsTypeOperand.MfsMessageType;
import innowake.ndt.mfsparser.ast.model.statement.message.MfsDeviceFieldOperand;
import innowake.ndt.mfsparser.ast.model.statement.message.MfsMfldStatement;
import innowake.ndt.mfsparser.ast.model.statement.message.MfsMsgStatement;
import innowake.ndt.mfsparser.parser.MfsParserAst;

/**
 * Contributor for IMS MFS files.
 */
@Component
public class ImsMfsContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(ImsMfsContributor.class);
	
	private static final List<ModuleType> IMS_PROGRAM_TYPES = Arrays.asList(ImsMetricsUtil.IMS_PROGRAM_TYPES);
	private static final List<RelationshipType> RELATIONSHIPS = Arrays.asList(RelationshipType.CALLS, RelationshipType.INCLUDES,
			RelationshipType.ACCESSES, RelationshipType.REFERENCES);
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private DiscoveryPersistence orientDiscoveryPersistence;
	
	@Autowired
	private ParserProviderService parserProviderService;
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return  sourceObject.getTechnology() == Technology.IMS && sourceObject.getType() == Type.MFS;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.IMS_MFS);
		calculateSourceMetrics(sourceObject, rootModule);
		collectSubModules(sourceObject, builder, rootModule);
		rootModule.deferAction("collectTransitiveMetrics");
	}

	private void calculateSourceMetrics(final SourcePojo sourceObject, final ModuleBuilder rootModule) {
		final var metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, CobolLexerFactory.get()));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		
		try {
			final SourceMetrics sourceMetrics = GenericMetricsUtil.executeAndGetResults(metricsContributor);
			/*
			 * We are adding a positive default value 0 for dead code lines and complexity. If we remove this we need to adjust the feature matrix as this
			 * technology won't support the dead code calculation and complexity, which is not correct.
			 */
			if (sourceMetrics.getDeadCodeLines() == -1) {
				sourceMetrics.setDeadCodeLines(0);
			}
			if (sourceMetrics.getComplexityMcCabe() == null || sourceMetrics.getComplexityMcCabe() == -1) {
				sourceMetrics.setComplexityMcCabe(0);
			}
			rootModule.addAdditionalInfo(sourceMetrics);
		} catch (final MetricException e) {
			LOG.error(String.format("Error while calculating metrics of %s", sourceObject.getPath()), e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}
	
	private void collectSubModules(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		final MfsParserAst<SourcePojo> imsMfsParser = parserProviderService.createImsMfsParser();
		final Optional<MfsModel> mfsModel = imsMfsParser.parse(sourceObject);
		mfsModel.flatMap(AstModel::getRoot).ifPresent(rootNode -> rootNode.getChildrenDeep(MfsMsgStatement.class).forEach(mfsMsgStatement -> {
			final MfsStatementLabel statementLabel = mfsMsgStatement.getStatementLabel();
			final var mfsTypeOperand = mfsMsgStatement.getType();
			if (statementLabel != null && mfsTypeOperand != null) {
				final var mfsMessageType = mfsTypeOperand.getType();
				final String name = statementLabel.getLabel();
				if (mfsMessageType == MfsMessageType.INPUT) {
					final ModuleBuilder midModule = builder.declareSubModule(name, ModuleType.IMS_MFS_MID);
					final Optional<MfsMfldStatement> mfsMfldStatement = rootNode.getChildrenDeep(MfsMfldStatement.class).stream()
							.filter(field -> field.getStartOffset() > mfsMsgStatement.getStartOffset()).findFirst();
					determineTransactionName(rootModule, mfsMfldStatement, statementLabel, midModule);
				} else if (mfsMessageType == MfsMessageType.OUTPUT) {
					builder.declareSubModule(name, ModuleType.IMS_MFS_MOD);
				}
			}
		}));
	}
	
	/**
	 * Calculates the transitive metrics between IMS_MFS and {@code IMS_PROGRAM_TYPES}.
	 *
	 * @param context the DiscoveryContext
	 * @param sourceObject the {@link SourcePojo}
	 * @param builder the {@link DiscoveryBuilder}
	 * @param rootModuleBuilder the ModuleBuilder
	 * @param module the source module
	 */
	@DeferredAction
	public void collectTransitiveMetrics(final DiscoveryContext context, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModuleBuilder,
			final ModuleLightweightPojo module, final SourcePojo sourceObject) {
		final var childModuleFilter = new ModuleFilter().setTypes(ModuleType.IMS_MFS_MID).setContainedIn(new ModuleFilter().setModuleIds(module.identity()));
		final List<EntityId> midModules = orientDiscoveryPersistence.findModules(context, childModuleFilter);
		midModules.forEach(midModule -> {
			final List<ModuleBasePojo> midModuleDependencies = getTargetModules(context.getProjectId(), midModule);
			final List<EntityId> transactionModules = midModuleDependencies.stream()
					.filter(midModuleDep -> midModuleDep.getModuleType() == ModuleType.IMS_SYSGEN_TRANSACTION)
					.map(ModuleBasePojo::identity)
					.collect(Collectors.toList());
			addProgramDependencies(context.getProjectId(), builder, rootModuleBuilder, transactionModules);
		});
	}
	
	private void determineTransactionName(final ModuleBuilder rootModuleBuilder, final Optional<MfsMfldStatement> mfsMfldStatement,
			final MfsStatementLabel statementLabel, final ModuleBuilder midModule) {
		if (mfsMfldStatement.isPresent()) {
			final MfsDeviceFieldOperand deviceFieldOperand = mfsMfldStatement.get().getDeviceField();
			if (deviceFieldOperand != null) {
				final String transactionName = MetricsUtility.trimQuotesSpaces(deviceFieldOperand.getLiteral());
				if (StringUtils.isNotBlank(transactionName)) {
					collectMfsMidDependencies(midModule, transactionName);
				} else {
					rootModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
							String.format("Unable to determine transaction name for %s", statementLabel.getLabel()));
				}
			}
		}
	}
	
	private void addProgramDependencies(final EntityId projectId, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModuleBuilder,
			final List<EntityId> transactionModules) {
		transactionModules.forEach(transactionModuleId -> {
			/*
			 * This will get all the target modules from the dependencies of IMS_SYSGEN_TRANSACTION module.
			 */
			final List<ModuleBasePojo> targetModulesForSysgenTransaction = getTargetModules(projectId, transactionModuleId);
			targetModulesForSysgenTransaction.stream().filter(targetModule -> targetModule.getModuleType() == ModuleType.IMS_SYSGEN_APPLICATION)
					.forEach(sysgenApplication -> {
						/*
						 * This will get all the target modules from the dependencies of IMS_SYSGEN_APPLICATION module.
						 */
						final List<ModuleBasePojo> targetModulesForApplication = getTargetModules(projectId, sysgenApplication.identity());

						final List<EntityId> potentialImsProgramModuleIds = targetModulesForApplication.stream()
								.filter(targetForApplication -> IMS_PROGRAM_TYPES.contains(targetForApplication.getModuleType())).map(ModuleBasePojo::identity)
								.collect(Collectors.toList());
						if ( ! potentialImsProgramModuleIds.isEmpty()) {
							final var potentialImsProgramModuleFilter = new ModuleFilter().setModuleIds(potentialImsProgramModuleIds);
							final AnchorToBuilder potentialImsProgramModuleBuilder = builder.anchorTo(potentialImsProgramModuleFilter,
									ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL);
							potentialImsProgramModuleBuilder.declareDependency(RelationshipType.INCLUDES, rootModuleBuilder).setBinding(Binding.EARLY);
						}
					});
		});
	}
	
	private void collectMfsMidDependencies(final ModuleBuilder midModule, final String transactionName) {
		final ModuleFilter sysgenTransactionFilter = new ModuleFilter().setNames(transactionName).setTypes(ModuleType.IMS_SYSGEN_TRANSACTION);
		/*
		 * This is to declare the dependency between the child modules of type IMS_MFS_MID of current root module and IMS_SYSGEN_TRANSACTION module
		 *  with given transaction name .
		 */
		midModule.declareDependency(RelationshipType.REFERENCES, sysgenTransactionFilter, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL).setBinding(Binding.EARLY);
	}

	private List<ModuleBasePojo> getTargetModules(final EntityId projectId, final EntityId moduleId) {
		final List<ModuleRelationshipPojo> dependencies = moduleService
				.findRelationship(q -> q.ofProject(projectId).ofModuleInDirection(moduleId, RelationshipDirection.OUT).withTypes(RELATIONSHIPS)
						.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION, RelationshipField.TYPE).includeModuleDetails(false, true));
		return dependencies.stream().map(ModuleRelationshipPojo::getDstModuleDetails).filter(Optional::isPresent).map(Optional::get)
				.collect(Collectors.toList());
	}
}
