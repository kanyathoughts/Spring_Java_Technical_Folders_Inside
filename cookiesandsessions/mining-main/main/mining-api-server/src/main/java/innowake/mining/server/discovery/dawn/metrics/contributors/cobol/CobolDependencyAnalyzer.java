/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.cobol;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_OPERATION;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_TYPE;
import static innowake.mining.server.discovery.metrics.exec.ExecStatementUtility.getDbAccess;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.dawn.metrics.contributors.ims.ImsMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.pl1.Pl1ImsCollector;
import innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol.DefaultCobolReferenceResolver;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.metrics.cobol.CobolDependency;
import innowake.mining.server.discovery.metrics.cobol.CobolSourceObjectManager;
import innowake.mining.server.discovery.metrics.cobol.dependency.CobolDependencyNodeCollector;
import innowake.mining.server.discovery.metrics.cobol.dependency.calls.DCLDependencyResolver;
import innowake.mining.server.discovery.metrics.cobol.dependency.calls.FMSDependencyResolver;
import innowake.mining.server.discovery.metrics.cobol.dependency.calls.IFDLDependencyResolver;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.cobol.parser.ast.model.CobolConstant;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolExpression;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEnterStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEnterStmt.LanguageType;
import innowake.ndt.cobol.parser.ast.statement.adaprep.AdaDeclareStmt;
import innowake.ndt.cobol.parser.ast.statement.exec.adabas.AbstractExecAdabasQueryNode;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsCreateTransaction;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeleteQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeleteQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFile;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsLink;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReadQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReadQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReturn;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsSendReceive;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsStatement;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsXctl;
import innowake.ndt.core.parsing.LexerConfiguration;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecSqlNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTable;
import innowake.ndt.core.parsing.cobol.CobolRegionType;
import software.amazon.awssdk.utils.StringUtils;

/**
 * This will analyze the Cobol files and creates the dependencies.
 */
public class CobolDependencyAnalyzer {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	private static final Pattern ADDITIONAL_CHARS_PATTERN = Pattern.compile("['\"]([^\"]*)['\"]");
	private static final Pattern COLON_PATTERN = Pattern.compile(":");
	
	private static final EnumSet<ModuleType> DEFAULT_PROGRAM_TARGET_TYPES = EnumSet.of(ModuleType.COBOL_PROGRAM, ModuleType.IMS_MFS,
			ModuleType.ASSEMBLER_PROGRAM, ModuleType.ASSEMBLER_MACRO, ModuleType.PL1_PROGRAM, ModuleType.PL1_MAINPROGRAM, ModuleType.PL1_SUBROUTINE,
			ModuleType.PL1_FUNCTION, ModuleType.C_PROGRAM, ModuleType.C_FUNCTION, ModuleType.BASIC_FUNCTION, ModuleType.BASIC_SUBROUTINE,
			ModuleType.BASIC_PROGRAM, ModuleType.CDO_FILE, ModuleType.CDO_RECORD, ModuleType.SQLMOD, ModuleType.SQLMOD_PROC, ModuleType.VAX_MACRO_ENTRY);
	public static final String SQLCA_UTILITY = "SQLCA";

	private final DiscoveryContext context;
	private final SourceObjectResolver sourceObjectResolver;
	private final CobolSourceObjectManager sourceObjectManager;
	private final CobolDependencyNodeCollector cobolDependencyNodeCollector;
	private final DefaultCobolReferenceResolver dawnCobolReferenceResolver;
	private final DawnOriginResolver dawnOriginResolver;
	private final ModuleService moduleService;

	private static final LexerConfiguration LEXER_CONFIGURATION = new LexerConfiguration();
	static {
		LEXER_CONFIGURATION.addRegionTypeToLex(CobolRegionType.TYPE_CODE_STANDARD);
		LEXER_CONFIGURATION.addRegionTypeToLex(CobolRegionType.TYPE_CODE_LITERAL);
	}
	
	public CobolDependencyAnalyzer(final DiscoveryContext context, final SourceObjectResolver sourceObjectResolver,
			final CobolSourceObjectManager sourceObjectManager, final DawnOriginResolver dawnOriginResolver,
			final CobolDependencyNodeCollector cobolDependencyNodeCollector, final DefaultCobolReferenceResolver dawnCobolReferenceResolver,
			final ModuleService moduleService) {
		this.context = context;
		this.sourceObjectResolver = sourceObjectResolver;
		this.sourceObjectManager = sourceObjectManager;
		this.dawnOriginResolver = dawnOriginResolver;
		this.cobolDependencyNodeCollector = cobolDependencyNodeCollector;
		this.dawnCobolReferenceResolver = dawnCobolReferenceResolver;
		this.moduleService = moduleService;
	}
	
	/**
	 * This will analyze the given {@link SourcePojo} and creates the dependencies.
	 * 
	 * @param builder {@link DiscoveryBuilderFromSource}
	 * @param rootModule the root {@link ModuleBuilder}
	 * @param sourceObject the {@link SourcePojo}
	 */
	public void analyze(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final SourcePojo sourceObject) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("analyzing dependencies for {} ...", sourceObject.getPath());
		}
		final var dawnCobolFileDependencies = new CobolFileDependencies(dawnCobolReferenceResolver, cobolDependencyNodeCollector.getDataFields());
		findCopies(rootModule, sourceObject);
		findCalls(builder, rootModule, sourceObject);
		handleResourceFileDependencies(dawnCobolFileDependencies, builder, rootModule);
		handleCobolToNaturalDependencies(rootModule);
	}

	private void handleCobolToNaturalDependencies(final ModuleBuilder rootModule) {
		cobolDependencyNodeCollector.getAdaDeclareStmts().forEach(adaDeclareStmt -> adaprep(adaDeclareStmt, rootModule));
		cobolDependencyNodeCollector.getExecAdabasQueryNodes().forEach(execAdabasQueryNodes -> adabas(execAdabasQueryNodes, rootModule));
	}

	private void findCalls(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final SourcePojo sourceObject) {
		handleSqlDependencies(builder, rootModule);
		handleCallStatements(sourceObject, builder, rootModule);
		handleEnterStatements(sourceObject, builder, rootModule);
		handleExecCicsLink(sourceObject, builder, rootModule);
		handleExecCicsXctl(sourceObject, builder, rootModule);
		handleExecCicsSendReceiveMap(sourceObject, builder, rootModule);
		handleExecCicsFile(builder, rootModule);
		handleExecCicsReturn(builder, rootModule);
		handleExecCicsCreateTransaction(builder, rootModule);
		handleExecCicsQueue(builder, rootModule);
	}
	
	private void adabas(@Nullable final AbstractExecAdabasQueryNode adabas, final ModuleBuilder rootModule) {
		if (adabas == null) {
			return;
		}
		resolveDependencyForNatural(rootModule, adabas.getFile().toString());
	}

	private void adaprep(@Nullable final AdaDeclareStmt declare, final ModuleBuilder rootModule) {
		if (declare == null) {
			return;
		}
		resolveDependencyForNatural(rootModule, declare.getFileName());
	}

	private void resolveDependencyForNatural(final ModuleBuilder rootModule, final String fileName) {
		final var moduleFilter = new ModuleFilter().setNames(fileName).setTypes(ModuleType.NATURAL_DDM);
		rootModule.declareDependency(RelationshipType.ACCESSES, moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MERGE_DUPLICATES)
		.setBinding(Binding.LATE)
		.createIfMissing(fileName, ModuleType.NATURAL_DDM);
	}
	
	private void handleResourceFileDependencies(final CobolFileDependencies dawnCobolFileDependencies,
			final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		cobolDependencyNodeCollector.getCobolSelectStatements().forEach(dawnCobolFileDependencies::addFilesFromSelectStatement);
		cobolDependencyNodeCollector.getCobolFileDefinitions().forEach(dawnCobolFileDependencies::addFilesFromFileDefinition);
		dawnCobolFileDependencies.getFileResources().forEach(cobolFileDependency -> {
			if (StringUtils.isBlank(cobolFileDependency.getName())) {
				return;
			}
			final ModuleBuilder resourceModule = builder.declareExternalModule(cobolFileDependency.getName(), cobolFileDependency.getModuleType());
			final var binding = dawnCobolFileDependencies.getBinding(cobolFileDependency);
			final DependencyBuilder dependency = rootModule.declareDependency(RelationshipType.ACCESSES, resourceModule)
					.setBinding(binding);
			final ModelAttributeMap<Object> attributeMap = dawnCobolFileDependencies.getAttributes(cobolFileDependency);
			if ( ! attributeMap.isEmpty()) {
				final Object attributes = attributeMap.get(ModelAttributeKey.FILE_ACCESS_TYPE);
				dependency.addAttribute(ModelAttributeKey.FILE_ACCESS_TYPE, attributes);
			}
			
		});
	}
	
	private void handleSqlDependencies(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		cobolDependencyNodeCollector.getSqlDeclareTempTables()
		.forEach(sqlDeclareTempTable -> handleSqlDeclareTempTableDependencies(sqlDeclareTempTable, builder, rootModule));
		cobolDependencyNodeCollector.getSqlDeclareSchema()
				.forEach(sqlDeclareSchema -> handleSqlDeclareSchemaDependencies(sqlDeclareSchema, builder, rootModule));
		cobolDependencyNodeCollector.getExecSqlCalls().forEach(sqlCall -> handleSqlCallDependencies(sqlCall, builder, rootModule));
		
		cobolDependencyNodeCollector.getSqlDeletes().forEach(sqlDelete -> handleSqlDependenciesWithFilter(sqlDelete, builder, rootModule));
		cobolDependencyNodeCollector.getSqlInserts().forEach(sqlInsert -> handleSqlDependenciesWithFilter(sqlInsert, builder, rootModule));
		cobolDependencyNodeCollector.getSqlSelects().forEach(sqlSelect -> handleSqlDependenciesWithFilter(sqlSelect, builder, rootModule));
		cobolDependencyNodeCollector.getSqlUpdates().forEach(sqlUpdate -> handleSqlDependenciesWithFilter(sqlUpdate, builder, rootModule));
		cobolDependencyNodeCollector.getSqlLockTables()
				.forEach(sqlLockTable -> handleSqlDependenciesWithFilter(sqlLockTable, builder, rootModule));
		cobolDependencyNodeCollector.getSqlCursors()
				.forEach(sqlDeclareCursor -> handleSqlDependenciesWithFilter(sqlDeclareCursor, builder, rootModule));
	}
	
	private void handleSqlDeclareSchemaDependencies(final ExecNode<?> sqlNode, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		handleSqlDependencies(sqlNode, builder, rootModule, ModuleType.RDB_DATABASE, RelationshipType.REFERENCES);
	}
	
	private void handleSqlCallDependencies(final ExecNode<?> sqlNode, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		if (sqlNode.getParent() instanceof ExecSqlNode) {
			return;
		}
		final var sourceModuleBuilder = dawnOriginResolver.resolve(sqlNode, builder, rootModule);
		final ModuleType targetType = ModuleType.SQL_STORED_PROCEDURE;
		final List<String> sqlStrings = resolveSql(sqlNode, rootModule, targetType);
		sqlStrings.forEach(storedProcedureName -> {
			final var dependencyBuilder = DawnMetricsUtility.declareDependencyToSqlStoredProcedureWithSchema(sourceModuleBuilder, storedProcedureName,
					 ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
			dependencyBuilder.setLocation(dawnOriginResolver.resolveLocation(sqlNode));
			addAttributes(storedProcedureName, dependencyBuilder, targetType);
		});
	}
	
	private void handleSqlDeclareTempTableDependencies(final ExecNode<?> sqlNode, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		handleSqlDependencies(sqlNode, builder, rootModule, ModuleType.SQL_TEMPORARY_TABLE, RelationshipType.ACCESSES);
	}
	
	private void handleSqlDependencies(final ExecNode<?> sqlNode, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule,
			final ModuleType targetType, final RelationshipType relationship) {
		if (sqlNode.getParent() instanceof ExecSqlNode) {
			return;
		}
		final var sourceModuleBuilder = dawnOriginResolver.resolve(sqlNode, builder, rootModule);
		final List<String> sqlStrings = resolveSql(sqlNode, rootModule, targetType);
		sqlStrings.forEach(sqlString -> {
			builder.declareExternalModule(sqlString, targetType);
			final var dependencyBuilder = sourceModuleBuilder
					.declareDependency(relationship, new ModuleFilter().setNames(sqlString).setTypes(targetType), ResolutionFlag.MERGE_DUPLICATES)
					.setBinding(Binding.LATE).setLocation(dawnOriginResolver.resolveLocation(sqlNode));
			addAttributes(sqlString, dependencyBuilder, targetType);
		});
	}
	
	private void handleSqlDependenciesWithFilter(final ExecNode<?> sqlNode, final DiscoveryBuilderFromSource builder,
			final ModuleBuilder rootModule) {
		if (sqlNode.getParent() instanceof ExecSqlNode) {
			return;
		}
		final var sourceModuleBuilder = dawnOriginResolver.resolve(sqlNode, builder, rootModule);
		final ModuleType targetType = sqlNode instanceof ExecSqlDeclareTable ? ModuleType.SQL_TABLE : ModuleType.SQL_VIEW;
		final List<String> sqls = resolveSql(sqlNode, rootModule, targetType);
		for (final String sql : sqls) {
			final List<DatabaseAccessType> dbAccess = new ArrayList<>();
			final List<String> statementInfos = new ArrayList<>();
			final Optional<Tuple2<List<DatabaseAccessType>, List<String>>> optionalAttributes = getAttributes(sql, targetType);
			if (optionalAttributes.isPresent()) {
				final Tuple2<List<DatabaseAccessType>, List<String>> attributes = optionalAttributes.get();
				dbAccess.addAll(attributes.e1);
				statementInfos.addAll(attributes.e2);
			}

			final var dependencyBuilder = DawnMetricsUtility.declareDependencyToSqlTableWithSchema(sourceModuleBuilder, sql,
					ResolutionFlag.MERGE_DUPLICATES, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
			dependencyBuilder.setLocation(dawnOriginResolver.resolveLocation(sqlNode));
			if ( ! statementInfos.isEmpty()) {
				dependencyBuilder.addAttribute(DB_ACCESS_OPERATION, statementInfos);
			}
			if ( ! dbAccess.isEmpty()) {
				dependencyBuilder.addAttribute(DB_ACCESS_TYPE, dbAccess);
			}
		}
	}
	
	private List<String> resolveSql(final ExecNode<?> sqlNode, final ModuleBuilder sourceModuleBuilder, final ModuleType targetType) {
		List<String> sqlNames = dawnCobolReferenceResolver.resolveExecSQL(sqlNode, sourceModuleBuilder, cobolDependencyNodeCollector.isDebugging());
		if (sqlNames.isEmpty()) {
			addUnresolveDependencyError(sourceModuleBuilder, targetType);
		}
		if (targetType != ModuleType.SQL_STORED_PROCEDURE) {
			sqlNames = sqlNames.stream().filter(table -> ! cobolDependencyNodeCollector.getCommonTableExpressions().contains(table))
					.collect(Collectors.toList());
		}
		return sqlNames;
	}

	private void addAttributes(final String sql, final DependencyBuilder dependencyBuilder, final ModuleType moduleType) {
		final Optional<Tuple2<List<DatabaseAccessType>, List<String>>> optionalAttributes = getAttributes(sql, moduleType);
		if (optionalAttributes.isPresent()) {
			final Tuple2<List<DatabaseAccessType>, List<String>> attributes = optionalAttributes.get();
			final List<DatabaseAccessType> dbAccess = attributes.e1;
			final List<String> statementInfos = attributes.e2;
			if ( ! statementInfos.isEmpty()) {
				dependencyBuilder.addAttribute(DB_ACCESS_OPERATION, statementInfos);
			}
			if ( ! dbAccess.isEmpty()) {
				dependencyBuilder.addAttribute(DB_ACCESS_TYPE, dbAccess);
			}
		}
	}
	
	private Optional<Tuple2<List<DatabaseAccessType>, List<String>>> getAttributes(final String sql, final ModuleType moduleType) {
		final Map<String, List<Object>> referencedTables = dawnCobolReferenceResolver.getReferencedTables();		
		if ((moduleType == ModuleType.SQL_TABLE || moduleType == ModuleType.SQL_TEMPORARY_TABLE || moduleType == ModuleType.SQL_VIEW)
				&& referencedTables.containsKey(sql)) {
			final List<Object> dbAccesses = referencedTables.get(sql);
			final List<DatabaseAccessType> dbAccess = getDbAccess(dbAccesses);
			final List<String> statementInfos = addStatementInfo(dbAccesses);
			final Tuple2<List<DatabaseAccessType>, List<String>> attributes = new Tuple2<>(dbAccess, statementInfos);
			return Optional.of(attributes);
		}
		return Optional.empty();
	}
	
	private List<String> addStatementInfo(@Nullable final List<Object> accessTypes) {
		final List<String> statementInfos = new ArrayList<>();
		if (accessTypes != null) {
			accessTypes.forEach(access -> {
				if (access instanceof SqlStatementType) {
					final SqlStatementType accessType = (SqlStatementType) access;
					switch (accessType) {
						case DECLARE_TABLE:
						case LOCK_TABLE:
							if ( ! statementInfos.contains(accessType.name())) {
								statementInfos.add(accessType.name());
							}
							break;
						default:
							break;
					}
				}
			});
			
		}
		return statementInfos;
	}
	
	private void handleCallStatements(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		for (final CobolCallStmt call : cobolDependencyNodeCollector.getCallStatements()) {
			final var sourceModuleBuilder = dawnOriginResolver.resolve(call, builder, rootModule);
			final Set<String> resolvedNames = new HashSet<>(dawnCobolReferenceResolver.resolve(sourceModuleBuilder, call.getTarget()));
			if (resolvedNames.isEmpty()) {
				addUnresolveDependencyError(sourceModuleBuilder, ModuleType.COBOL_PROGRAM);
				return;
			}
			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();		
			final List<ModuleFilter> targetModulefilters = new ArrayList<>();
			final var imsUtility = resolvedNames.stream()
					.map(ImsMetricsUtil::resolveImsUtility)
					.filter(Objects::nonNull)
					.findAny();
			if (context.getConfig().isCobolImsDependencyEnabled() && imsUtility.isPresent()) {
				attributes.put(ModelAttributeKey.CALL_TYPE, ModelAttributeValue.CallType.IMS_DB);
				handleImsCalls(call, attributes);
			} else {
				attributes.put(ModelAttributeKey.CALL_TYPE, Collections.singleton(ModelAttributeValue.CallType.CALL));
				targetModulefilters.addAll(new FMSDependencyResolver().getDependencies(dawnCobolReferenceResolver, call));
				targetModulefilters.addAll(new DCLDependencyResolver().getDependencies(dawnCobolReferenceResolver, call));
				targetModulefilters.addAll(new IFDLDependencyResolver().getDependencies(dawnCobolReferenceResolver, call));
			}
			final Binding bindingType = call.getTarget() instanceof CobolConstantReference ? Binding.EARLY : Binding.LATE;
			final ModuleLocation location = dawnOriginResolver.resolveLocation(call);
			resolvedNames.forEach(targetName -> {
				final boolean isImsUtility = ImsMetricsUtil.resolveImsUtility(targetName) != null;
				final DependencyBuilder dependencyBuilder;
				if (isImsUtility) {
					final ModuleBuilder utilityModule = builder.declareExternalModule(targetName, ModuleType.UNKNOWN_UTILITY);
					dependencyBuilder = sourceModuleBuilder.declareDependency(RelationshipType.CALLS, utilityModule);
				} else {
					final var targetFilter = resolveModuleFilter(sourceObject, targetName, ModuleType.COBOL_PROGRAM,
							DEFAULT_PROGRAM_TARGET_TYPES.toArray(ModuleType[]::new));
					dependencyBuilder = sourceModuleBuilder.declareDependency(RelationshipType.CALLS, targetFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
				}
				addAdditionalPropertiesForDependency(dependencyBuilder, attributes, bindingType, location);
			});
			targetModulefilters.stream()
					.map(targetFilter -> sourceModuleBuilder.declareDependency(RelationshipType.CALLS, targetFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE))
					.forEach(dependency -> addAdditionalPropertiesForDependency(dependency, attributes, bindingType, location));
		}
	}

	private void addAdditionalPropertiesForDependency(final DependencyBuilder dependencyBuilder, final ModelAttributeMap<Object> attributes,
			final Binding bindingType, final ModuleLocation location) {
		dependencyBuilder
		.setBinding(bindingType)
				.setLocation(location)
				.setAttributes(attributes);
	}
	
	private ModuleFilter resolveModuleFilter(final SourcePojo sourceObject, final String name, final ModuleType type, final ModuleType... targetTypes) {

		/* use cobolclipse to resolve object. this considers search order */
		final var targetObject = sourceObjectResolver.resolveObject(sourceObject, name, new SourceObjectMatcher(type.getTechnology(), type.getType()));

		final var moduleFilter = new ModuleFilter();
		/* Try to resolve as Cobol Program first */
		if (targetObject != null) {
			moduleFilter.setNames(targetObject.getName()).setPaths(targetObject.getPath())
					.setTypes(ModuleType.fromTechnologyAndType(targetObject.getTechnology(), targetObject.getType()))
					.setPhysical(true);
		} else {
			if (targetTypes.length > 0) {
				moduleFilter.setNames(name).setTypes(targetTypes);
			} else {
				/* if the target type is not provided, fall back to default target types */
				moduleFilter.setNames(name).setTypes(DEFAULT_PROGRAM_TARGET_TYPES.toArray(ModuleType[]::new));
			}
		}
		if (context.getFeatureMap().get(FeatureId.INCREMENTAL_SCAN).booleanValue()) {
			moduleFilter.setIdentification(Identification.IDENTIFIED);
		}
		return moduleFilter;
	}
	
	private void handleImsCalls(final CobolCallStmt callStmt, final ModelAttributeMap<Object> attributes) {
		if ( ! callStmt.getUsings().isEmpty() && callStmt.getUsings().get(0).getExpression() instanceof CobolReferenceExpression) {
			final CobolReferenceExpression refExpression = assertNotNull((CobolReferenceExpression) callStmt.getUsings().get(0).getExpression());
			final List<String> functions = dawnCobolReferenceResolver.resolve(refExpression.getOp1());
			final List<DatabaseAccessType> accessTypes = functions.stream().map(Pl1ImsCollector::mapDbAccessType).collect(Collectors.toList());
			attributes.put(DB_ACCESS_TYPE, accessTypes);
			attributes.put(DB_ACCESS_OPERATION, functions);
		}
		
		getAttributesFromUsings(callStmt, attributes);
	}

	private void getAttributesFromUsings(final CobolCallStmt callStmt, final ModelAttributeMap<Object> attributes) {
		if (callStmt.getUsings().size() >= 4) {
			final List<String> ssa = new ArrayList<>();
			for (int i = 3; i < callStmt.getUsings().size(); i++) {
				final CobolExpression exp = callStmt.getUsings().get(i).getExpression();
				if (exp instanceof CobolReferenceExpression) {
					final CobolReferenceExpression refExp = (CobolReferenceExpression) exp;
					getdbds(callStmt, ssa, refExp);
				}
			}
			attributes.put(ModelAttributeKey.IMS_SSA, ssa);
		}
	}

	private void getdbds(final CobolCallStmt callStmt, final List<String> ssa, final CobolReferenceExpression refExp) {
		if (refExp.getOp1() instanceof CobolFieldReference) {
			final CobolFieldReference fieldReference = (CobolFieldReference) refExp.getOp1();
			final String dbdSsa;
			if ( ! fieldReference.getField().isGroup()) {
				/* not group field */
				dbdSsa = resolveCallArgumentFieldValue(callStmt, fieldReference, fieldReference.getField());
			} else {
				/* group field */
				dbdSsa = fieldReference.getField().getChildren().stream()
						.filter(CobolDataField.class::isInstance)
						.map(CobolDataField.class::cast)
						.map(f -> resolveCallArgumentFieldValue(callStmt, fieldReference, f))
						.collect(Collectors.joining());
			}
			ssa.add(dbdSsa);
		} else if (refExp.getOp1() instanceof CobolConstantReference) {
			final CobolConstantReference constantReference = (CobolConstantReference) refExp.getOp1();
			final Object value = constantReference.getValue();
			if (value != null && ! CobolConstant.stringIsCobolConstant(value.toString())) {
				ssa.add(MetricsUtility.trimQuotesSpaces(value.toString()));
			}
		}
	}
	
	/**
	 * Resolve the value of an argument to a CALL statement.
	 *
	 * @param callStmt the CALL statement
	 * @param fieldReference the field reference as it appears in the USING clause of the CALL statement
	 * @param dataField either the field that is pointed to by {@code fieldReference} or one of its child fields,
	 *                     in case {@code fieldReference} points to a group
	 * @return the resolved value of the {@code dataField}, taking into account the location of {@code fieldReference}
	 */
	private String resolveCallArgumentFieldValue(final CobolCallStmt callStmt, final CobolFieldReference fieldReference, final CobolDataField dataField) {
		if ("FILLER".equals(dataField.getName())) {
			/* we can not create a "fake reference" to a FILLER field, because if we do so, strange things happen (FILLERs can not be referenced)
			 * therefore we resolve the FILLER value directly from its VALUE clause */
			return dataField.getValues().stream().filter(CobolConstantReference.class::isInstance)
					.map(CobolConstantReference.class::cast)
					.map(CobolConstantReference::getValue)
					.filter(Objects::nonNull)
					.map(Object::toString)
					.filter(str -> ! CobolConstant.stringIsCobolConstant(str)) /* filter out SPACE, SPACES, etc. */
					.map(MetricsUtility::trimQuotesSpaces)
					.collect(Collectors.joining());
		} else {
			/* in order to find relevant MOVEs and SETs to the field, in addition to VALUE clauses,
			 * we create a "fake" reference to the field at the location of the CALL */
			final var fakeReference = new CobolFieldReference(fieldReference.getStartToken());
			/* for whatever reason, CobolReferenceResolver only takes the offset of the MOVE statement into account
			 * if the parent of the reference is a CobolCallStatement - this is probably to avoid creating incorrect
			 * CALL dependencies. Let's make the parent a CALL statement so that we get correct results. */
			fakeReference.setParent(callStmt);
			fakeReference.setField(dataField);
			final Optional<String> resolvedName = dawnCobolReferenceResolver.resolve(fakeReference).stream().findFirst();
			return resolvedName.orElse("$" + dataField.getName() + "$");
		}
	}
	
	private void handleEnterStatements(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		for (final CobolEnterStmt enter : cobolDependencyNodeCollector.getEnters()) {
			final var matcher = ADDITIONAL_CHARS_PATTERN.matcher(enter.getTarget().toString());
			final String nameWithoutQuotes = matcher.find() ? matcher.group(1) : enter.getTarget().toString();
			final var sourceModuleBuilder = dawnOriginResolver.resolve(enter, builder, rootModule);
			final var targetModuleFilter =  resolveEnter(sourceObject, nameWithoutQuotes, enter.getLanguageType());
			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.CALL_TYPE, Collections.singleton(ModelAttributeValue.CallType.ENTER));
			
			sourceModuleBuilder.declareDependency(RelationshipType.CALLS, targetModuleFilter)
			.setBinding(Binding.EARLY)
			.setAttributes(attributes)
			.setLocation(dawnOriginResolver.resolveLocation(enter));
		}
	}
	
	/**
	 * Resolves target {@link ModuleFilter} from a Cobol CobolEnterStatement.
	 *
	 * @param sourceObject a {@link SourcePojo}
	 * @param targetName a program object name
	 * @param targetType the language type
	 * @return the target {@link ModuleFilter}
	 */
	private ModuleFilter resolveEnter(final SourcePojo sourceObject, final String targetName, final LanguageType targetType) {
		final ModuleType targetModuleType;
		switch (targetType.toString()) {
			case "PL1":
				targetModuleType = ModuleType.PL1_MAINPROGRAM;
				break;
			case "ASM":
				targetModuleType = ModuleType.ASSEMBLER_PROGRAM;
				break;
			default:
				targetModuleType = ModuleType.UNKNOWN;
				break;
		}

		return resolveEntryForceType(sourceObject, targetName, targetModuleType);
	}
	
	private ModuleFilter resolveEntryForceType(final SourcePojo sourceObject, final String targetName, final ModuleType targetModuleType) {
		/* use cobolclipse to resolve object. this considers search order */
		final var targetObject = sourceObjectResolver.resolveObject(sourceObject, targetName,
				new SourceObjectMatcher(Technology.COBOL, targetModuleType.getType()));

		final var moduleFilter = new ModuleFilter();
		/* Try to resolve as Cobol Program first */
		if (targetObject != null) {
			moduleFilter.setNames(targetObject.getName())
			.setPaths(targetObject.getPath())
			.setTypes(ModuleType.fromTechnologyAndType(targetObject.getTechnology(), targetObject.getType()))
			.setPhysical(true);
		} else {
			moduleFilter
			.setNames(targetName)
			.setTypes(targetModuleType);
		}

		return moduleFilter;
	}
	
	private void handleExecCicsLink(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		cobolDependencyNodeCollector.getExecCicsLinks().forEach(link -> handleExecCicsXctlLink(sourceObject, builder, rootModule, link));
	}

	private void handleExecCicsXctl(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		cobolDependencyNodeCollector.getExecCicsXctls().forEach(xctl -> handleExecCicsXctlLink(sourceObject, builder, rootModule, xctl));
	}

	private void handleExecCicsXctlLink(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule,
			final AstNode astNode) {
		final var sourceModuleBuilder = dawnOriginResolver.resolve(astNode, builder, rootModule);
		final Set<ModelAttributeValue.CallType> modelAttribute = new HashSet<>(1);
		final CobolExpression expression;
		if (astNode instanceof ExecCicsXctl) {
			final ExecCicsXctl xctl = (ExecCicsXctl) astNode;
			expression = xctl.getProgram();
			modelAttribute.add(ModelAttributeValue.CallType.EXECCICSXCTL);
		} else {
			final ExecCicsLink link = (ExecCicsLink) astNode;
			expression = link.getProgram();
			modelAttribute.add(ModelAttributeValue.CallType.LINK);
		}
		final Set<String> resolvedNames = new HashSet<>(dawnCobolReferenceResolver.resolve(sourceModuleBuilder, expression));
		if (resolvedNames.isEmpty()) {
			addUnresolveDependencyError(sourceModuleBuilder, ModuleType.COBOL_PROGRAM);
			return;
		}
		final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
		attributes.put(ModelAttributeKey.CALL_TYPE, modelAttribute);
		resolvedNames.forEach(name -> {
			final var resolveModuleFilter = resolveModuleFilter(sourceObject, name, ModuleType.COBOL_PROGRAM,
					DEFAULT_PROGRAM_TARGET_TYPES.toArray(ModuleType[]::new));
			sourceModuleBuilder.declareDependency(RelationshipType.CALLS, resolveModuleFilter, ResolutionFlag.MERGE_DUPLICATES)
			.setBinding(Binding.LATE)
			.setAttributes(attributes)
			.setLocation(dawnOriginResolver.resolveLocation(astNode));
		});
	}
	
	private void handleExecCicsSendReceiveMap(final SourcePojo sourceObject, final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		for (final ExecCicsSendReceive map : cobolDependencyNodeCollector.getExecCicsSendReceives()) {
			final var sourceModuleBuilder = dawnOriginResolver.resolve(map, builder, rootModule);
			final Optional<List<ModuleFilter>> targetModuleFilter = resolveBmsMap(sourceObject, sourceModuleBuilder, map.getMap(), map.getMapSet());

			if (targetModuleFilter.isEmpty()) {
				addUnresolveDependencyError(sourceModuleBuilder, ModuleType.CICS_BMS_MAP);
				continue;
			}

			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.CALL_TYPE, Collections.singleton(ModelAttributeValue.CallType.MAP));
			/* check if send or receive and set attribute */
			final var operation = map.getOperation();
			if( operation != null ) {
				switch( operation ) {
					case SEND_MAP:
						attributes.put(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE, Collections.singleton(ModelAttributeValue.SendReceiveAccess.SEND));
						break;
					case RECEIVE_MAP:
						attributes.put(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE, Collections.singleton(ModelAttributeValue.SendReceiveAccess.RECEIVE));
						break;
					default:
						/* default send ?? */
						attributes.put(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE, Collections.singleton(ModelAttributeValue.SendReceiveAccess.UNDEFINED));
						break;
				}
			} else {
				/* default send ?? */
				attributes.put(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE, Collections.singleton(ModelAttributeValue.SendReceiveAccess.UNDEFINED));
			}
			
			sourceModuleBuilder.declareDependency(RelationshipType.INCLUDES, targetModuleFilter.get())
			.setBinding(Binding.LATE)
			.setAttributes(attributes)
			.setLocation(dawnOriginResolver.resolveLocation(map));
		}
	}

	private void addUnresolveDependencyError(final ModuleBuilder sourceModuleBuilder, final ModuleType targetType) {
		final var errorMessage = String.format("Unable to determine name of dependency target. The target likely has the type %s",
				targetType);
		sourceModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, errorMessage);
	}
	
	private Optional<List<ModuleFilter>> resolveBmsMap(final SourcePojo sourceObject, final ModuleBuilder moduleBuilder, @Nullable final CobolExpression map,
			@Nullable final CobolExpression mapset) {
		final List<ModuleFilter> mapSetModuleFilters = new ArrayList<>();
		/* If mapset is null, should replace with value for map (according to documentation) */
		final CobolExpression tempMapset = mapset == null ? map : mapset;
		if (tempMapset instanceof CobolReferenceExpression) {
			final List<String> resolvedNames = dawnCobolReferenceResolver.resolve(moduleBuilder, tempMapset);
			resolvedNames.forEach(name -> {
				final var resolveModuleFilter = resolveModuleFilter(sourceObject, name, ModuleType.CICS_BMS_MAPSET,
						DEFAULT_PROGRAM_TARGET_TYPES.toArray(ModuleType[]::new));
				mapSetModuleFilters.add(resolveModuleFilter);
			});
		}

		final List<String> mapNames = map instanceof CobolReferenceExpression
				? dawnCobolReferenceResolver.resolve(((CobolReferenceExpression) map).getOp1())
				: emptyList();
		if (mapSetModuleFilters.size() == 1 && mapNames.size() == 1) {
			final var mapsetModuleFilter = mapSetModuleFilters.get(0);
			final String mapName = mapNames.get(0);
			/*
			 * First, we will resolve the CICS_BMS_MAP which is in Parent module of type CICS_BMS_MAPSET or with possible parents of types 
			 * DEFAULT_PROGRAM_TARGET_TYPES.
			 */
			final ModuleFilter mapFilterWithParentMapset = new ModuleFilter()
			.setNames(mapName)
			.setTypes(ModuleType.CICS_BMS_MAP)
			.setContainedIn(mapsetModuleFilter);
			
			/*
			 * If we don't have target module then We will resolve the CICS_MAP directly with the given target name.
			 */
			final ModuleFilter mapFilter = new ModuleFilter()
					.setNames(mapName)
					.setTypes(ModuleType.CICS_BMS_MAP);
			return Optional.of(Arrays.asList(mapFilterWithParentMapset, mapFilter));
		} else if (mapSetModuleFilters.isEmpty() && mapNames.size() == 1) {
			return Optional.of(Collections.singletonList(new ModuleFilter()
					.setNames(mapNames.get(0))
					.setTypes(ModuleType.CICS_BMS_MAP)));
		} else {
			final String errorMessage = "Cannot resolve BMS map uniquely: map " + (map != null ? map.toString() : "") + " mapset "
					+ (tempMapset != null ? tempMapset.toString() : "");
			LOG.error(errorMessage);
			moduleBuilder.addError(Severity.ERROR, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, errorMessage);
		}

		return Optional.empty();
	}
	
	private void handleExecCicsFile(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		for (final ExecCicsFile file : cobolDependencyNodeCollector.getExecCicsFiles()) {
			final var sourceModuleBuilder = dawnOriginResolver.resolve(file, builder, rootModule);
			final List<String> resolveNames = dawnCobolReferenceResolver.resolve(((CobolReferenceExpression) file.getFile()).getOp1());
			final String resolvedTargetName = resolveNames.isEmpty() ? file.getFile().toString() : resolveNames.iterator().next();
			final var csdFileModuleFilter = new ModuleFilter().setNames(resolvedTargetName).setTypes(ModuleType.CSD_FILE);
			final var resourceFilter = new ModuleFilter().setNames(resolvedTargetName).setTypes(ModuleType.RESOURCE_FILE);
			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.CALL_TYPE, Collections.singleton(ModelAttributeValue.CallType.EXECCICSFILE));
			/* TODO: if we find multiple module ,creating dependency with the first module is impossible need to find the way to resolve.*/
			sourceModuleBuilder.declareDependency(RelationshipType.ACCESSES, Arrays.asList(csdFileModuleFilter, resourceFilter))
			.setBinding(Binding.LATE)
			.setAttributes(attributes)
			.setLocation(dawnOriginResolver.resolveLocation(file));
		}
	}
	
	private void handleExecCicsReturn(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		/* handle all exec cics return resources collected */
		for (final ExecCicsReturn node : cobolDependencyNodeCollector.getExecCicsReturn()) {
			final var sourceModuleBuilder = dawnOriginResolver.resolve(node, builder, rootModule);
			final Optional<String> transId = dawnCobolReferenceResolver.resolveReturnTransID(node, sourceModuleBuilder);
			if (transId.isEmpty()) {
				continue;
			}
			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.CALL_TYPE, Collections.singleton(ModelAttributeValue.CallType.EXECCICSRETURN));
			final var targetModuleFilter = new ModuleFilter().setNames(transId.get()).setTypes(ModuleType.CSD_TRANSACTION);
			/* TODO: if we find multiple module ,creating dependency with the first module is impossible need to find the way to resolve.*/
			sourceModuleBuilder.declareDependency(RelationshipType.REFERENCES, targetModuleFilter)
			.setBinding(Binding.LATE)
			.setAttributes(attributes)
			.setLocation(dawnOriginResolver.resolveLocation(node));
		}
	}
	
	private void handleExecCicsCreateTransaction(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		for (final ExecCicsCreateTransaction transaction : cobolDependencyNodeCollector.getExecCicsTransaction()) {
			/* get file resource from repo if exists */
			final Optional<String> target = dawnCobolReferenceResolver.resolveReturnTransID(transaction);
			if( ! target.isPresent() ) {
				continue;
			}
			final var targetModuleBuilder = builder.declareExternalModule(MetricsUtility.trimQuotesSpaces(target.get()), ModuleType.CSD_TRANSACTION);
			final var sourceModuleBuilder = dawnOriginResolver.resolve(transaction, builder, rootModule);
			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.CALL_TYPE, Collections.singleton(ModelAttributeValue.CallType.EXECICSCREATE));
			/* TODO: if we find multiple module ,creating dependency with the first module is impossible need to find the way to resolve.*/
			sourceModuleBuilder.declareDependency(RelationshipType.REFERENCES, targetModuleBuilder)
			.setBinding(Binding.LATE)
			.setAttributes(attributes)
			.setLocation(dawnOriginResolver.resolveLocation(transaction));
		}
	}
	
	/**
	 * Handles analyzing the Exec CICS TDQ/TSQ statements.
	 *
	 * @param builder the {@link DiscoveryBuilderFromSource}
	 * @param rootModule the root {@link ModuleBuilder}
	 */
	private void handleExecCicsQueue(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {
		handleExecCicsQueueHelper(cobolDependencyNodeCollector.getExecCicsQTds(), ModuleType.CICS_TDQ, builder, rootModule);
		handleExecCicsQueueHelper(cobolDependencyNodeCollector.getExecCicsQTs(), ModuleType.CICS_TSQ, builder, rootModule);
	}

	private void handleExecCicsQueueHelper(final List<ExecCicsStatement> execCicsQueueStatements, final ModuleType moduleType,
			final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule) {

		if (execCicsQueueStatements.isEmpty()) {
			return;
		}
		
		final Map<String, Set<ModelAttributeValue.QueueAccess>> targets = new HashMap<>();
		final var sourceModuleBuilder = dawnOriginResolver.resolve(execCicsQueueStatements.iterator().next(), builder, rootModule);
		
		for (final ExecCicsStatement queueTs : execCicsQueueStatements) {
			final String target = dawnCobolReferenceResolver.resolveQueue(queueTs);
			if (StringUtils.isNotBlank(target)) {
				targets.computeIfAbsent(target, key -> new LinkedHashSet<>())
							.add(getQueueAccess(queueTs));
			}
		}

		for (final Map.Entry<String, Set<ModelAttributeValue.QueueAccess>> target : targets.entrySet()) {
			final ModelAttributeMap<Object> attributes = new ModelAttributeMap<>();
			attributes.put(ModelAttributeKey.QUEUE_ACCESS, target.getValue());
			final var targetModuleBuilder = builder.declareExternalModule(target.getKey(), moduleType);
			sourceModuleBuilder.declareDependency(RelationshipType.ACCESSES, targetModuleBuilder)
			.setBinding(Binding.LATE)
			.setAttributes(attributes);
		}
	}

	/**
	 * Resolves the type of queue access.
	 *
	 * @param queueCicsStatement The statement to resolve the access type from.
	 * @return The type of queue access type of the {@link ExecCicsStatement}, or UNEDFINED.
	 */
	private ModelAttributeValue.QueueAccess getQueueAccess(final ExecCicsStatement queueCicsStatement) {
		if (queueCicsStatement instanceof ExecCicsDeleteQTd || queueCicsStatement instanceof ExecCicsDeleteQTs) {
			return ModelAttributeValue.QueueAccess.DELETEQ;
		}

		if (queueCicsStatement instanceof ExecCicsReadQTd || queueCicsStatement instanceof ExecCicsReadQTs) {
			return ModelAttributeValue.QueueAccess.READQ;
		}

		if (queueCicsStatement instanceof ExecCicsWriteQTd || queueCicsStatement instanceof ExecCicsWriteQTs) {
			return ModelAttributeValue.QueueAccess.WRITEQ;
		}

		return ModelAttributeValue.QueueAccess.UNDEFINED;
	}
	
	private void findCopies(final ModuleBuilder rootModule, final SourcePojo module) {
		for (final CobolDependency dependency : sourceObjectManager.getOutgoingDependencies(module)) {
			final String targetName = dependency.getTargetName();
			final String libraryName = dependency.getLibraryName();
			final var copyProcModuleFilter = new ModuleFilter();
			if (libraryName != null) {
				final var containedInModuleFilter = new ModuleFilter().setNames(libraryName.toUpperCase()).setTypes(ModuleType.COBOL_COPYLIB);
				copyProcModuleFilter.setNames(targetName).setTypes(ModuleType.COBOL_COPYPROC).setContainedIn(containedInModuleFilter);
			} else {
				copyProcModuleFilter.setNames(targetName).setTypes(ModuleType.COBOL_COPYPROC);
			}
			final Set<SourcePojo> targets = dependency.getTargets();
			SourcePojo targetSourceObject = null;
			if ( ! targets.isEmpty()) {
				targetSourceObject = targets.iterator().next();
			}
			declareDependencyForCopybook(rootModule, dependency, targetName, libraryName, targetSourceObject, copyProcModuleFilter);
		}
	}

	private void declareDependencyForCopybook(final ModuleBuilder rootModule, final CobolDependency dependency, final String targetName,
			@Nullable final String libraryName, @Nullable final SourcePojo targetSourceObject, final ModuleFilter copyProcModuleFilter) {
		final List<ModuleFilter> moduleFilterList = new ArrayList<>();
		final RelationshipType relationship;
		var noTargetSourceObjectExist = true;
		if (dependency.isFromDictionary()) {
			/* Try resolving a CDO record */
			final var cdoRecordFilter = new ModuleFilter().setNames(targetName).setTypes(ModuleType.CDO_RECORD);
			final String targetNameWithDot = COLON_PATTERN.matcher(targetName).replaceFirst("\\.");
			final var cdoRecordFilterForNameWithDot = new ModuleFilter().setNames(targetNameWithDot).setTypes(ModuleType.CDO_RECORD);
			moduleFilterList.add(cdoRecordFilter);
			moduleFilterList.add(cdoRecordFilterForNameWithDot);
			relationship = RelationshipType.REFERENCES;
		} else if (targetSourceObject != null) {
			final var targetType = ModuleType.fromTechnologyAndType(targetSourceObject.getTechnology(), targetSourceObject.getType());
			final var targetFilter = new ModuleFilter().setNames(targetSourceObject.getName()).setTypes(targetType)
					.setPaths(targetSourceObject.getPath()).setPhysical(true);
			moduleFilterList.add(targetFilter);
			relationship = RelationshipType.INCLUDES;
			noTargetSourceObjectExist = false;
		} else {
			/*
			 * This is to resolve dependency with COBOL_COPYPROC first when there is a library present of type COBOL_COPYLIB.
			 * If there is no target module found, it will create a missing dependency with COBOL_COPYPROC.
			 */
			if (libraryName != null) {
				final List<EntityId> copyLibs = moduleService
						.findModuleIds(q -> q.withName(libraryName.toUpperCase()).withTechnology(Technology.COBOL).withType(Type.COPYLIB));
				if ( ! copyLibs.isEmpty()) {
					final var copyProcFilter = new ModuleFilter().setNames(targetName).setTypes(ModuleType.COBOL_COPYPROC);
					moduleFilterList.add(copyProcFilter);
				}
			}
			final var copybookFilter = new ModuleFilter().setNames(targetName).setTypes(ModuleType.COBOL_COPYBOOK);
			final var easytrieveMacroFileFilter = new ModuleFilter().setNames(targetName).setTypes(ModuleType.EASYTRIEVE_MACRO_FILE);
			moduleFilterList.add(copybookFilter);
			moduleFilterList.add(easytrieveMacroFileFilter);
			relationship = RelationshipType.INCLUDES;
		}
		moduleFilterList.add(copyProcModuleFilter);
		final var dependencyBuilder = rootModule
				.declareDependency(relationship, moduleFilterList, ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MERGE_DUPLICATES)
				.setBinding(Binding.EARLY);
		if (targetName.equals(SQLCA_UTILITY) && noTargetSourceObjectExist) {
			dependencyBuilder.createIfMissing(targetName, ModuleType.COBOL_COPYBOOK, Origin.ENVIRONMENT);
		}
		if (dependency.getModuleLocation() != null) {
			dependencyBuilder.setLocation(assertNotNull(dependency.getModuleLocation()));
		}
	}
}
