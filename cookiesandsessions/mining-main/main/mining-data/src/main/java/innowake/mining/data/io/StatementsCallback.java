/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.mining.shared.io.WorkbookDefinition.SQL_COLUMN_CUSTOM_COMPLEXITY;
import static innowake.mining.shared.io.WorkbookDefinition.SQL_COLUMN_DISTINCT_TABLES;
import static innowake.mining.shared.io.WorkbookDefinition.SQL_COLUMN_HALSTEAD_COMPLEXITY;
import static innowake.mining.shared.io.WorkbookDefinition.SQL_COLUMN_HALSTEAD_DIFFICULTY;
import static innowake.mining.shared.io.WorkbookDefinition.SQL_COLUMN_SQL_LENGTH;
import static innowake.mining.shared.io.WorkbookDefinition.SQL_COLUMN_TABLES;
import static innowake.mining.shared.io.WorkbookDefinition.STATEMENTS_COLUMN_STATEMENT;
import static innowake.mining.shared.io.WorkbookDefinition.STATEMENTS_COLUMN_STRING;
import static innowake.mining.shared.io.WorkbookDefinition.UID;

import java.util.HashMap;
import java.util.Map;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;

/**
 * A {@link RowCallback} implementation for Statements sheet.
 */
class StatementsCallback extends AbstractRowCallback {

	private final Map<Long, EntityId> modulesNidToEid;
	private final Map<Long, Technology> moduleTechnologyMap;
	private final ModuleService moduleService;
	private final boolean isSql;

	StatementsCallback(final EntityId projectId, final String fileId, final ModuleService moduleService, final Map<Long, EntityId> modulesNidToEid,
			final Map<Long, Technology> moduleTechnologyMap, final boolean isSql) {
		super(projectId, fileId);
		this.modulesNidToEid = modulesNidToEid;
		this.moduleTechnologyMap = moduleTechnologyMap;
		this.moduleService = moduleService;
		this.isSql = isSql;
	}

	@Override
	public void rowComplete(final int rowNum, final String[] row) {
		final var uid = Long.valueOf(getMandatoryValue(row, UID));
		final var module = getModuleId(modulesNidToEid, row);
		final var type = StatementType.fromName(getMandatoryValue(row, STATEMENTS_COLUMN_STATEMENT));
		final var string = getMandatoryValue(row, STATEMENTS_COLUMN_STRING);

		final var statement = new StatementPojoPrototype()
										.setModule(module)
										.setType(type)
										.setText(string);

		if (isSql) {
			final Map<String, Object> properties = new HashMap<>();
			properties.put(StatementPojo.PROPERTY_KEY_SQL_LENGTH, getMandatoryValue(row, SQL_COLUMN_SQL_LENGTH));
			properties.put(StatementPojo.PROPERTY_KEY_TABLES, getMandatoryValue(row, SQL_COLUMN_TABLES));
			properties.put(StatementPojo.PROPERTY_KEY_DISTINCT_TABLES, getMandatoryValue(row, SQL_COLUMN_DISTINCT_TABLES));
			properties.put(StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY, getMandatoryValue(row, SQL_COLUMN_CUSTOM_COMPLEXITY));
			properties.put(StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY, getMandatoryValue(row, SQL_COLUMN_HALSTEAD_COMPLEXITY).replace(",", "."));
			properties.put(StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY, getMandatoryValue(row, SQL_COLUMN_HALSTEAD_DIFFICULTY).replace(",", "."));

			statement.setProperties(properties);
			statement.setTechnology(Technology.SQL);
		} else {
			final var technology = moduleTechnologyMap.get(uid);
			if (technology == null) {
				throw new IllegalArgumentException("Unable to get module technology for imported uid: " + uid);
			}

			statement.setTechnology(technology);
		}

		moduleService.createStatement(statement, false);
	}
}