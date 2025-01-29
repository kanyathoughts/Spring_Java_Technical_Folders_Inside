/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_CODE_LINES;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_COMMENT_LINES;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_COMPLEXITY;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_LANGUAGE;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_LENGTH;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_NAME;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_OFFSET;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_PARENT_UID;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_PHYSICAL_LINES;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_REPRESENTATION;
import static innowake.mining.shared.io.WorkbookDefinition.MODULE_COLUMN_TYPE;
import static innowake.mining.shared.io.WorkbookDefinition.UID;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;

/**
 * A {@link RowCallback} implementation for Modules sheet.
 */
class ModulesCallback extends AbstractRowCallback {

	private final DiscoveryModuleImporter moduleImporter;
	private final Map<Long, EntityId> modulesNidToEid = new HashMap<>();
	private final Map<Long, Technology> modulesNidToTechnology = new HashMap<>();
	/* key is uid of child module, value is nid of parent Module */
	private final Map<EntityId, Long> unresolvedParentModules = new HashMap<>();
	private final ModuleParameters moduleParameters;
	private final DiscoveryReferenceImporter referenceImporter;

	ModulesCallback(final EntityId projectId, final String fileId, final ModuleService moduleService, final ModuleParameters moduleParameters) {
		super(projectId, fileId);
		moduleImporter = new DiscoveryModuleImporter(moduleService);
		this.moduleParameters = moduleParameters;
		referenceImporter = new DiscoveryReferenceImporter(moduleService, modulesNidToEid, null, null, moduleParameters);
	}

	@Override
	public void rowComplete(final int rowNum, final String[] row) {
		final var name = getMandatoryValue(row, MODULE_COLUMN_NAME);
		final var nid = Long.valueOf(getMandatoryValue(row, UID));
		final var parentUidInString = getOptionalValue(row, MODULE_COLUMN_PARENT_UID);
		final Long parentNid = StringUtils.isNotBlank(parentUidInString) ? Long.valueOf(parentUidInString) : null;
		final var technology = getTechnology(getMandatoryValue(row, MODULE_COLUMN_LANGUAGE));
		final var type = getType(getMandatoryValue(row, MODULE_COLUMN_TYPE));
		final var storage = getStorage(technology, type);
		/* Path can be null, e.g. for virtual artifacts */
		final var path = StringUtils.trimToEmpty(getOptionalValue(row, WorkbookDefinition.MODULE_COLUMN_PATH));
		final String representation = getMandatoryValue(row, MODULE_COLUMN_REPRESENTATION);
		final var complexity = Integer.valueOf(getMandatoryValue(row, MODULE_COLUMN_COMPLEXITY));
		final var linesOfCode = Integer.valueOf(getMandatoryValue(row, MODULE_COLUMN_CODE_LINES));
		final var linesOfComment = Integer.valueOf(getMandatoryValue(row, MODULE_COLUMN_COMMENT_LINES));

		/* compatibility: below columns were added lately */
		@Nullable
		final var offset = getOptionalIntegerValue(row, MODULE_COLUMN_OFFSET);
		@Nullable
		final var length = getOptionalIntegerValue(row, MODULE_COLUMN_LENGTH);
		@Nullable
		final ModuleLocation location = offset == null || length == null ? null : new ModuleLocation(offset, length);
		@Nullable
		final var physicalLines = getOptionalIntegerValue(row, MODULE_COLUMN_PHYSICAL_LINES);

		final var sourceMetrics = new SourceMetrics();
		sourceMetrics.setCodeLines(linesOfCode);
		sourceMetrics.setCommentLines(linesOfComment);
		sourceMetrics.setComplexityMcCabe(complexity);
		sourceMetrics.setPhysicalLines(physicalLines);
		sourceMetrics.setDeadCodeLines(Integer.valueOf(-1)); /* this field is not present in the CSV. We update it as a post import process */

		final EntityId moduleId = moduleImporter.importModule(projectId, name, path, technology, type, storage, sourceMetrics,
				Representation.valueOf(representation), location, moduleParameters);

		if (parentNid != null) {
			/* Either the child or the parent may occur first in the sheet - if child is found first, we don't know the rid of the parent yet,
			 * so we can't importContainsModule() directly. Instead, we add the child rid and unresolved parent uid to unresolvedParentModules */
			if (modulesNidToEid.containsKey(parentNid)) {
				referenceImporter.importContainsModule(modulesNidToEid.get(parentNid), moduleId);
			} else {
				unresolvedParentModules.put(moduleId, parentNid);
			}
		}
		modulesNidToEid.put(nid, moduleId);
		modulesNidToTechnology.put(nid, technology);
	}

	Map<Long, EntityId> getModuleIdMap() {
		return Collections.unmodifiableMap(modulesNidToEid);
	}

	Map<Long, Technology> getModuleTechnologyMap() {
		return Collections.unmodifiableMap(modulesNidToTechnology);
	}

	@Override
	public void sheetComplete() {
		/* process previously unresolved parents */
		unresolvedParentModules.entrySet().stream()
				.forEach(entry -> referenceImporter.importContainsModule(modulesNidToEid.get(entry.getValue()), entry.getKey()));
	}
}