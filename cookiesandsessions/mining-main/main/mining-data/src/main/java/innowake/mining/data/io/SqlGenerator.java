/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.model.Technology;

/**
 * Creates the SQL sheet of a Discovery result file.
 */
abstract class SqlGenerator implements ModuleMappingConsumer {

	@Autowired
	private ModuleService moduleService;

	/**
	 * Processes all SQL Statements for a Discovery result file.
	 *
	 * @param projectId the project ID
	 * @param moduleMapping mapping between Module ID and a {@link Tuple2} of UID and Discovery type
	 * @param sorted if {@code true}, sorts the result
	 * @throws IOException if an error occurs
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void process(final EntityId projectId, final Map<Long, String> moduleMapping, final DiscoveryExportOptions options, final boolean sorted)
			throws IOException {

		final var data = moduleService.getModuleStatementExport(projectId, true, sorted);
		if (data.isPresent()) {
			final var iterator = data.get().iterator();
			while (iterator.hasNext()) {
				final var row = iterator.next();
				//stopped here
				final var properties = (Map<String, Object>) row.getOrDefault("properties", Collections.emptyMap());

				createRow(row.getNonNull("nid"),
						  row.getNonNull("name"),
						  row.getNonNull("type"),
						  properties.getOrDefault(StatementPojo.PROPERTY_KEY_SQL_LENGTH, "0"),
						  properties.getOrDefault(StatementPojo.PROPERTY_KEY_TABLES, "0"),
						  properties.getOrDefault(StatementPojo.PROPERTY_KEY_DISTINCT_TABLES, "0"),
						  properties.getOrDefault(StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY, "0"),
						  properties.getOrDefault(StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY, "0.00"),
						  properties.getOrDefault(StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY, "0.00"),
						  row.getNonNull("text"));
			}
		}
	}

	/**
	 * Returns the number of sql queries associated with the given project. 
	 *
	 * @param projectId the ID of the project to search for sql queries
	 * @return the number of sql queries
	 */
	public long getSQLCount(final EntityId projectId) {
		return moduleService.countStatements(q -> q.ofModule(projectId).withTechnology(Technology.SQL));
	}

}
