/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.spi.Document;

import java.util.List;

/**
 * Utility class for dawn metrics
 */
public class DawnMetricsUtility {

	public static final String WILDCARD_PATTERN = "%.";
	private static final List<ModuleType> SQL_TABLE_TARGET_TYPES = List.of(ModuleType.SQL_VIEW, ModuleType.SQL_TABLE);
	
	private DawnMetricsUtility() {
		/* no instances */
	}

	/**
	 * Determine and set the lines of code for an Artifact.
	 * 
	 * @param sourceObject the {@link SourcePojo} to be processed.
	 * @param rootModule the {@link ModuleBuilder} module that represents the entire source file.
	 * @param commentIdentifier Prefix marking a line as a comment.
	 */
	public static void collectLinesOfCode(final SourcePojo sourceObject, final ModuleBuilder rootModule, final String commentIdentifier) {
		final SourceMetrics metricsV2 = new SourceMetrics();
		final String sourceObjectContent = sourceObject.getContent().toString();
		final String[] content = new Document(sourceObjectContent).lines();
		
		int linesOfComment = 0;
		int linesOfCode = 0;
		for (final String line : content) {
			final String trimmedLine = line.trim();
			if (trimmedLine.startsWith(commentIdentifier)) {
				linesOfComment++;
			} else if ( ! trimmedLine.isEmpty()) {
				linesOfCode++;
			}
		}
		if (linesOfComment == 0 && linesOfCode == 0) {
			rootModule.addError(Severity.WARNING, ErrorKey.EMPTY_FILE, "Found empty file: " + sourceObject.getName());
		}
		metricsV2.setCodeLines(Integer.valueOf(linesOfCode));
		metricsV2.setCommentLines(Integer.valueOf(linesOfComment));
		rootModule.addAdditionalInfo(metricsV2);
	}

	/**
	 * Declare a dependency to a module for Sql Stored Procedure which can have schema names included in them.
	 *
	 * @param sourceModuleBuilder the {@link ModuleBuilder} of the source module.
	 * @param name the name of the target module.
	 * @param resolutionFlags the {@link ResolutionFlag}s to be used for resolving the dependency.
	 * @return the {@link DiscoveryBuilder.DependencyBuilder} for the declared dependency.
	 */
	public static DiscoveryBuilder.DependencyBuilder declareDependencyToSqlStoredProcedureWithSchema(final ModuleBuilder sourceModuleBuilder, final String name,
			final ResolutionFlag... resolutionFlags) {
		final var index = name.indexOf(".");
		/* If sql target is invoked with qualifier but the target with qualifier not found,
		 * therefore search by removing the qualifier
		 * If target invoked without qualifier, and we are not able to find the target without qualifier,
		 * regex is used to search.
		 */
		final var potentialStoredProcedureName = index > -1 ? name.substring(index + 1) : WILDCARD_PATTERN + name;
		final var moduleFilter = new ModuleFilter().setNames(name).setTypes(ModuleType.SQL_STORED_PROCEDURE);
		final var potentialModuleFilter = new ModuleFilter().setNames(potentialStoredProcedureName).setTypes(ModuleType.SQL_STORED_PROCEDURE);
		return sourceModuleBuilder
				.declareDependency(RelationshipType.CALLS, List.of(moduleFilter, potentialModuleFilter), resolutionFlags)
				.setBinding(Binding.LATE);
	}

	/**
	 * Declare a dependency to a module for Sql Table which can have schema names included in them.
	 * This will create the dependency to a SQL_VIEW module contained in a SCHEMA which are imported from DB-Crawler,
	 * if the schema name is mentioned.
	 *
	 * @param sourceModuleBuilder the {@link ModuleBuilder} of the source module.
	 * @param name the name of the target module.
	 * @param resolutionFlags the {@link ResolutionFlag}s to be used for resolving the dependency.
	 * @return the {@link DiscoveryBuilder.DependencyBuilder} for the declared dependency.
	 */
	public static DiscoveryBuilder.DependencyBuilder declareDependencyToSqlTableWithSchema(final ModuleBuilder sourceModuleBuilder, final String name,
			final ResolutionFlag... resolutionFlags) {
		final var index = name.indexOf(".");

		if (index == -1) {
			/* If no schema name is present, directly search for the table */
			return sourceModuleBuilder.declareDependency(RelationshipType.ACCESSES, new ModuleFilter().setNames(name).setTypes(SQL_TABLE_TARGET_TYPES),
							resolutionFlags)
					.setBinding(Binding.LATE)
					.createIfMissing(name, ModuleType.SQL_TABLE);
		}
		final var schemaName = name.substring(0, index);
		final var tableName = name.substring(index + 1);
		/* First, try to find with name without schema and "contained in" schema module (this would be the case if the db-crawler schema was imported) */
		final var moduleFilterWithSchema = new ModuleFilter().setNames(tableName)
				.setTypes(SQL_TABLE_TARGET_TYPES).setContainedIn(new ModuleFilter().setNames(schemaName).setTypes(ModuleType.SQL_SCHEMA));
		/* otherwise try to find with name without schema module (this is the case if no schema was imported) */
		final var moduleFilterWithoutSchema = new ModuleFilter().setNames(tableName).setTypes(SQL_TABLE_TARGET_TYPES);
		return sourceModuleBuilder
				.declareDependency(RelationshipType.ACCESSES, List.of(moduleFilterWithSchema, moduleFilterWithoutSchema),
						resolutionFlags)
				.setBinding(Binding.LATE)
				.createIfMissing(tableName, ModuleType.SQL_TABLE);

	}
}
