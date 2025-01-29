/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.io.WorkbookDefinition.ERRORS_COLUMN_CAUSE;
import static innowake.mining.shared.io.WorkbookDefinition.ERRORS_COLUMN_FROM_LENGTH;
import static innowake.mining.shared.io.WorkbookDefinition.ERRORS_COLUMN_FROM_OFFSET;
import static innowake.mining.shared.io.WorkbookDefinition.ERRORS_COLUMN_KEY;
import static innowake.mining.shared.io.WorkbookDefinition.ERRORS_COLUMN_SEVERITY;
import static innowake.mining.shared.io.WorkbookDefinition.UID;

import java.util.Map;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * A {@link RowCallback} implementation for Errors sheet.
 */
class ErrorsCallback extends AbstractRowCallback {
	
	private final Map<Long, EntityId> modulesNidToEid;
	private final ModuleService moduleService;

	ErrorsCallback(final EntityId projectId, final String fileId, final ModuleService moduleService, final Map<Long, EntityId> modulesNidToEid) {
		super(projectId, fileId);
		this.moduleService = moduleService;
		this.modulesNidToEid = modulesNidToEid;
	}

	@Override
	public void rowComplete(final int rowNum, final String[] row) {
		final var nid = Long.valueOf(getMandatoryValue(row, UID));
		final var module = modulesNidToEid.get(nid);
		final var severity = getMandatoryValue(row, ERRORS_COLUMN_SEVERITY);
		final var length = getOptionalIntegerValue(row, ERRORS_COLUMN_FROM_LENGTH) == null ? -1 :
				assertNotNull(getOptionalIntegerValue(row, ERRORS_COLUMN_FROM_LENGTH)).intValue();
		final var offset = getOptionalIntegerValue(row, ERRORS_COLUMN_FROM_OFFSET) == null ? -1 :
				assertNotNull(getOptionalIntegerValue(row, ERRORS_COLUMN_FROM_OFFSET)).intValue();
		final var key = getMandatoryValue(row, ERRORS_COLUMN_KEY);
		final var cause = getMandatoryValue(row, ERRORS_COLUMN_CAUSE);

		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
				.setProject(projectId)
				.setModule(module)
				.setSeverity(Severity.fromString(severity))
				.setKey(ErrorKey.fromString(key))
				.setCause(cause)
				.setLocation(new AstNodeLocation(offset, length, offset, length, offset, length, -1, -1)));
	}
}
