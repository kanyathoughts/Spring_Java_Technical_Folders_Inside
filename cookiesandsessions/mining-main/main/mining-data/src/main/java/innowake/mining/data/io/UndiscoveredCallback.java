/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.mining.shared.io.WorkbookDefinition.UNDISCOVERED_COLUMN_NAME;
import static innowake.mining.shared.io.WorkbookDefinition.UNDISCOVERED_COLUMN_PATH;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;

/**
 * A {@link RowCallback} implementation for Undiscovered sheet.
 */
class UndiscoveredCallback extends AbstractRowCallback {

	private final ModuleService moduleService;

	UndiscoveredCallback(final EntityId projectId, final String fileId, final ModuleService moduleService) {
		super(projectId, fileId);
		this.moduleService = moduleService;
	}

	@Override
	public void rowComplete(final int rowNum, final String[] row) {
		final String name = getMandatoryValue(row, UNDISCOVERED_COLUMN_NAME);
		final String path = getMandatoryValue(row, UNDISCOVERED_COLUMN_PATH);

		moduleService.createUndiscovered(new ModuleUndiscoveredPojoPrototype()
												.setProject(projectId)
												.setName(name)
												.setPath(path));
	}
}