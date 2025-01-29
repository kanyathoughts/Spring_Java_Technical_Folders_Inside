/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.mining.shared.io.WorkbookDefinition.DEAD_CODE_COLUMN_DEADCODE;
import static innowake.mining.shared.io.WorkbookDefinition.DEAD_CODE_COLUMN_NUMBER_OF_LINES;
import static innowake.mining.shared.io.WorkbookDefinition.DEAD_CODE_COLUMN_STARTING_LINE;

import java.util.Map;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;

/**
 * A {@link RowCallback} implementation for Dead Code sheet.
 */
class DeadCodeCallback extends AbstractRowCallback {

	private final ModuleService moduleService;
	private final Map<Long, EntityId> modulesNidToEid;

	DeadCodeCallback(final EntityId projectId, final String fileId, final ModuleService moduleService, final Map<Long, EntityId> modulesNidToEid) {
		super(projectId, fileId);
		this.moduleService = moduleService;
		this.modulesNidToEid = modulesNidToEid;
	}

	@Override
	public void rowComplete(final int rowNum, final String[] row) {
		final var deadCode = getMandatoryValue(row, DEAD_CODE_COLUMN_DEADCODE);
		final var startingLine = Integer.valueOf(getMandatoryValue(row, DEAD_CODE_COLUMN_STARTING_LINE));
		final var numberOfLines = Integer.valueOf(getMandatoryValue(row, DEAD_CODE_COLUMN_NUMBER_OF_LINES));

		moduleService.createDeadCode(new ModuleDeadCodePojoPrototype()
				.setModule(getModuleId(modulesNidToEid, row))
				.setDeadCode(deadCode)
				.setStartingLine(startingLine)
				.setNumberOfLines(numberOfLines));
	}
}