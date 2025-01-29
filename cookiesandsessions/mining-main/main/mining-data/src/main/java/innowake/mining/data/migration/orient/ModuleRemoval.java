/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.stream.Collectors;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.FutureUtil;

/**
 * Migrates all entities in OrientDB that are linked with modules and deletes all modules.
 * <p>
 * Please note:
 * <ul>
 * <li>When dropping the {@code HasAnnotation_in_idx} index, Orient always reported an error which resulted in an {@link SQLException}. But when I looked into the 
 * schema the index was not present anymore</li>
 * <li>When dropping the edge classes I was unable to drop the {@code Module} class. The {@code NullPointerException} in Orient said that a cluster 497 was null.
 * This was the cluster used for storing {@code Calls} edges.</li>
 * </ul>
 */
public class ModuleRemoval extends OrientSchemaMigration {

	private static final String[] EDGES = { "Calls", "ContainsModule", "Includes", "None", "ReadsWrites", "EdgeReference",
											"HasTaxonomy", "HasAst", "HasAnnotation", "HasDataDictionaryEntry",
											"HasBusinessRule", "References" };
	private static final String[] VERTICES = { "SourceMetrics", "DependencyDefinition", "MiningJobInfo", "Statement",
											   "ExcelSheetErrors", "ExcelSheetDeadCode", "ModuleUnit",
											   "AnnotationMetaData", "AnnotationMetaDataReasonEnum", "Annotation", "AnnotationCategory", "AnnotationTypeEnum",
											   "Taxonomy", "TaxonomyEnum", "TaxonomyCategory", "DataDictionaryEntry", "DataDictionaryEntryScopeAttributes",
											   "DataDictionaryVariableScopeEnum", "DataDictionaryOtherScopeEnum" };
	private static final int DELETE_BATCH_SIZE = 10_000;
	
	public ModuleRemoval(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		migrateAstNodes(dbOrient);
		migrateJclControlFlowNodeMetadata(dbOrient);
		migrateDataFlowNode(dbOrient);
		migrateProxyContainer(dbOrient);
		migrateFieldInfo(dbOrient);

		ifDeletionRequested(() -> {
				deleteEntities("edge", EDGES);
				deleteEntities("edge", "Reference");
				deleteEntities("vertice", VERTICES);
				dropClasses(dbOrient);
			}, () -> "Skipping deletion of orient classes and entities for: "
					+ Arrays.stream(EDGES).collect(Collectors.joining(", "))
					+ ", Module, SqlStatement, "
					+ Arrays.stream(VERTICES).collect(Collectors.joining(", "))
			);
	}

	private void migrateAstNodes(final Connection connection) throws SQLException {
		LOG.info("Processing AstNodes for Module migration");

		dropIndexSafe("AstNode_module_idx");

		final List<String> statements = new ArrayList<>();
		
		statements.add("UPDATE Module set entryPointLink = Null WHERE entryPointLink IS NOT Null AND entryPointLink.@class is Null");
		statements.add("UPDATE Module set returnPointLink = Null WHERE returnPointLink IS NOT Null AND returnPointLink.@class is Null");
		statements.add("UPDATE Module set haltPointLink = Null WHERE haltPointLink IS NOT Null AND haltPointLink.@class is Null");
		
		statements.add("CREATE PROPERTY AstNode.inclusionCalleeModuleId IF NOT EXISTS LONG UNSAFE");
		statements.add("CREATE PROPERTY AstNode.moduleId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		statements.add("CREATE PROPERTY AstNode.hasAstModuleId IF NOT EXISTS LONG UNSAFE");
		/* required for CALLS hot spot query */
		statements.add("CREATE PROPERTY AstNode.projectId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");

		executeStatements(connection, statements);

		updateBatchwise(dbOrient, "UPDATE AstNode SET projectId = module.projectId, inclusionCalleeModuleId = inclusionCalleeModule.id, moduleId = module.id,"
								+ "hasAstModuleId = first(in_HasAst.out.id) WHERE projectId is Null LIMIT 100000");

		statements.clear();

		addRemovePropertyStatements(statements, "AstNode", "inclusionCalleeModule", "module", "in_HasAst");
		statements.add("CREATE INDEX AstNode_moduleId_idx IF NOT EXISTS ON AstNode (moduleId) NOTUNIQUE");
		statements.add("CREATE PROPERTY EntryPoint.moduleId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		statements.add("CREATE PROPERTY HaltPoint.moduleId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		statements.add("CREATE PROPERTY ReturnPoint.moduleId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		executeStatements(connection, statements);

		updateBatchwise(dbOrient, "UPDATE Module SET entryPointLink.moduleId=id WHERE entryPointLink IS NOT Null AND entryPointLink.moduleId is Null LIMIT 100000");
		updateBatchwise(dbOrient, "UPDATE Module SET haltPointLink.moduleId=id WHERE haltPointLink IS NOT Null AND haltPointLink.moduleId is Null LIMIT 100000");
		updateBatchwise(dbOrient, "UPDATE Module SET returnPointLink.moduleId=id WHERE returnPointLink IS NOT Null AND returnPointLink.moduleId is Null LIMIT 100000");
	}

	private void migrateFieldInfo(final Connection connection) throws SQLException {
		LOG.info("Processing FieldInfos for Module migration");

		final List<String> statements = new ArrayList<>();
		statements.add("CREATE PROPERTY FieldInfo.moduleId IF NOT EXISTS LONG UNSAFE");
		executeStatements(connection, statements);

		updateBatchwise(dbOrient, "UPDATE FieldInfo SET moduleId = first(in_HasAdditionalInfo.out.id) WHERE moduleId is Null LIMIT 100000");

		statements.clear();

		/* Delete all HasAdditionalInfo edges between FieldInfo and edges */
		statements.add("DELETE FROM (SELECT in_HasAdditionalInfo From FieldInfo) UNSAFE");
		/* Remove superclass AdditionalInfo which also removes in_HasAdditionalInfo from schema */
		statements.add("ALTER CLASS FieldInfo SUPERCLASS -AdditionalInfo");
		/* Remove property anyway to ensure there are no leftovers in OrientDB */
		addRemovePropertyStatements(statements, "FieldInfo", "in_HasAdditionalInfo");

		executeStatements(connection, statements);
	}

	private void migrateJclControlFlowNodeMetadata(final Connection connection) throws SQLException {
		LOG.info("Processing JclControlFlowNodeMetadata for Module migration");
		final List<String> statements = new ArrayList<>();

		statements.add("CREATE PROPERTY JclControlFlowNodeMetadata.inputFileIds IF NOT EXISTS EMBEDDEDLIST LONG UNSAFE");
		statements.add("CREATE PROPERTY JclControlFlowNodeMetadata.outputFileIds IF NOT EXISTS EMBEDDEDLIST LONG UNSAFE");

		statements.add("UPDATE JclControlFlowNodeMetadata SET inputFileIds = inputFiles.id, outputFileIds = outputFiles.id");

		addRemovePropertyStatements(statements, "JclControlFlowNodeMetadata", "inputFiles", "outputFiles");

		executeStatements(connection, statements);
	}

	private void migrateDataFlowNode(final Connection connection) throws SQLException {
		LOG.info("Processing DataFlowNode for Module migration");

		dropIndexSafe("DataFlowNode_module_idx");
		dropIndexSafe("DataFlowNode_module_ast_idx");

		final List<String> statements = new ArrayList<>();
		statements.add("CREATE PROPERTY DataFlowNode.moduleId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		statements.add("CREATE PROPERTY DataFlowNode.projectId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		executeStatements(connection, statements);

		updateBatchwise(dbOrient, "UPDATE DataFlowNode SET projectId = module.projectId, moduleId = module.id WHERE moduleId is Null LIMIT 100000");

		statements.clear();
		addRemovePropertyStatements(statements, "DataFlowNode", "module");

		statements.add("CREATE INDEX DataFlowNode_moduleId_idx IF NOT EXISTS ON DataFlowNode (moduleId) NOTUNIQUE");
		statements.add("CREATE INDEX DataFlowNode_moduleId_ast_idx IF NOT EXISTS ON DataFlowNode (moduleId, astNode) NOTUNIQUE");

		executeStatements(connection, statements);
	}

	private void migrateProxyContainer(final Connection connection) throws SQLException {
		LOG.info("Processing ProxyContainer for Module migration");

		dropIndexSafe("ProxyContainer_module_idx");
		dropIndexSafe("ProxyContainer_module_type_idx");
		/* The index name actually starts with "DataFlowNode" :( */
		dropIndexSafe("DataFlowNode_module_statement_idx");

		final List<String> statements = new ArrayList<>();
		statements.add("ALTER CLASS ProxyContainer STRICTMODE false");

		statements.add("CREATE PROPERTY ProxyContainer.moduleId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		/* DataFlowDao does a delete by project: module.projectId so we have to create a projectId property as well for better performance */
		statements.add("CREATE PROPERTY ProxyContainer.projectId IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE) UNSAFE");
		executeStatements(connection, statements);

		updateBatchwise(dbOrient, "UPDATE ProxyContainer SET projectId = module.projectId, moduleId = module.id WHERE moduleId is Null LIMIT 100000");

		statements.clear();

		addRemovePropertyStatements(statements, "ProxyContainer", "module");

		statements.add("CREATE INDEX ProxyContainer_moduleId_idx IF NOT EXISTS ON ProxyContainer (moduleId) NOTUNIQUE");
		statements.add("CREATE INDEX ProxyContainer_moduleId_type_idx IF NOT EXISTS ON ProxyContainer (moduleId, type) NOTUNIQUE");
		statements.add("CREATE INDEX ProxyContainer_moduleId_statement_idx IF NOT EXISTS ON ProxyContainer (moduleId, statement) NOTUNIQUE");

		statements.add("ALTER CLASS ProxyContainer STRICTMODE TRUE");

		executeStatements(connection, statements);
	}

	private void deleteEntities(final String type, final String... names) {
		final BlockingQueue<Future<?>> futures = new LinkedBlockingQueue<>();
		final var executorService = Executors.newFixedThreadPool(Math.max(1, Runtime.getRuntime().availableProcessors()));

		for (final var name : names) {
			futures.add(executorService.submit(() -> {
				try (final var connection = context.getOrient().getConnection()) {
					LOG.info("Deleting {}: {}", type, name);
					final String query = "DELETE FROM " + name + " LIMIT " + DELETE_BATCH_SIZE + " UNSAFE";

					try (final var delete = connection.prepareStatement(query)) {
						var deleted = 0;
						do {
							deleted = delete.executeUpdate();
							LOG.info("Deleted {} {} record", Integer.valueOf(deleted), name);
						} while (deleted >= DELETE_BATCH_SIZE);
					}
					LOG.info("Deletion of {}: {} done", type, name);
				} catch (final SQLException exc) {
					LOG.warn("Error while deleting " + name + " entities", exc);
				}
			}));
		}

		futures.add(executorService.submit(() -> {
			try (final var connection = context.getOrient().getConnection()) {
				LOG.info("Deleting edges of SourceMetrics");
				final String query = "DELETE FROM (SELECT FROM HasAdditionalInfo WHERE in.@class = \"SourceMetrics\" LIMIT " + DELETE_BATCH_SIZE + ") UNSAFE";

				try (final var delete = connection.prepareStatement(query)) {
					var deleted = 0;
					do {
						deleted = delete.executeUpdate();
						LOG.info("Deleted {} edges of SourceMetrics", Integer.valueOf(deleted));
					} while (deleted >= DELETE_BATCH_SIZE);
				}
				LOG.info("Deletion of edges of SourceMetrics done");
			} catch (final SQLException exc) {
				LOG.warn("Error while deleting SourceMetrics entities", exc);
			}
		}));

		try {
			FutureUtil.awaitAll(futures);
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException(e);
		} catch (final Exception exc) {
			throw new IllegalStateException(exc);
		}
	}

	private void dropClasses(final Connection connection) throws SQLException {
		LOG.info("Dropping all classes");
		final List<String> statements = new ArrayList<>(2 + EDGES.length + VERTICES.length);

		/* First drop these before dropping their super-classes get dropped */
		statements.add("DROP CLASS SqlStatement IF EXISTS UNSAFE");
		statements.add("DROP CLASS Module IF EXISTS UNSAFE");

		for (final var name : EDGES) {
			statements.add("DROP CLASS " + name + " IF EXISTS UNSAFE");
		}

		statements.add("DROP CLASS Reference IF EXISTS UNSAFE");

		for (final var name : VERTICES) {
			statements.add("DROP CLASS " + name + " IF EXISTS UNSAFE");
		}

		executeStatements(connection, statements);
	}

	private static void addRemovePropertyStatements(final List<String> statements, final String clazz, final String... properties) {
		for (final var property : properties) {
			statements.add("DROP PROPERTY " + clazz + "." + property + " IF EXISTS FORCE");

			/* Drop is fast but removing a property is performed on all entities of the class */
			if ( ! Boolean.getBoolean("mining.schema.migration.skip.orient.deletion")) {
				statements.add("UPDATE " + clazz + " REMOVE " + property);
			} else {
				LOG.info("System property 'mining.schema.migration.skip.orient.deletion' is set to true, skipping deletion of orient property: " + clazz + "." + property);
			}
		}
	}
}
