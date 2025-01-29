/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;

import static innowake.mining.shared.io.WorkbookDefinition.*;

/**
 * A {@link RowCallback} implementation for Dependencies sheet.
 */
class DependenciesCallback extends AbstractRowCallback {
	
	private final DiscoveryReferenceImporter referenceImporter;
	private final Map<Long, EntityId> modulesNidToEid;

	DependenciesCallback(
			final EntityId projectId,
			final String fileId,
			final ModuleService moduleService,
			final Map<Long, EntityId> modulesNidToEid,
			final ModuleParameters moduleParameters) {
		
		super(projectId, fileId);
		this.modulesNidToEid = modulesNidToEid;
		referenceImporter = new DiscoveryReferenceImporter(moduleService, modulesNidToEid, null, null, moduleParameters);
	}

	@Override
	public void rowComplete(final int rowNum, final String[] row) {
		final var uid = Long.valueOf(getMandatoryValue(row, UID));
		final var targetNid = Long.valueOf(getMandatoryValue(row, DEPENDENCIES_COLUMN_TARGET_UID));
		final var targetName = getMandatoryValue(row, DEPENDENCIES_COLUMN_TARGET_NAME);
		final var targetTechnology = getTechnology(getMandatoryValue(row, DEPENDENCIES_COLUMN_TARGET_LANGUAGE));
		final var targetType = getType(getMandatoryValue(row, DEPENDENCIES_COLUMN_TARGET_TYPE));
		final String uids = getOptionalValue(row, DEPENDENCIES_COLUMN_REACHED_FROM_UIDS);
		final Long[] reachedFromUids;
		if (uids == null || uids.isEmpty()) {
			reachedFromUids = new Long[0];
		} else {
			reachedFromUids = new Gson().fromJson(uids, Long[].class);
		}
		final List<EntityId> validIfReachedFrom = Arrays.stream(reachedFromUids)
				.map(modulesNidToEid::get)
				.collect(Collectors.toList());
		final var binding = getMandatoryValue(row, DEPENDENCIES_COLUMN_BINDING);
		final var attributes = getMandatoryValue(row, DEPENDENCIES_COLUMN_ATTRIBUTES);
		
		RelationshipType relationship;
		String relationshipFromImportData = null;
		try {
			relationshipFromImportData = getOptionalValue(row, DEPENDENCIES_COLUMN_RELATIONSHIP);
			relationship = relationshipFromImportData == null ? getReferenceType(targetTechnology, targetType) : RelationshipType.from(relationshipFromImportData);
		} catch (final IllegalArgumentException e) {
			relationship = getReferenceType(targetTechnology, targetType);
			LOG.warn("Invalid relationship provided {} with target module as {} with technology {} and type {} respectively. Relationship: {} assigned based on the technology and type.", relationshipFromImportData, targetName, targetTechnology, targetType, relationship.toString());
		}
		

		/* compatibility: below columns was added lately */
		final var fromOffset = getOptionalIntegerValue(row, DEPENDENCIES_COLUMN_FROM_OFFSET);
		final var fromLength = getOptionalIntegerValue(row, DEPENDENCIES_COLUMN_FROM_LENGTH);
		final var toOffset = getOptionalIntegerValue(row, DEPENDENCIES_COLUMN_TO_OFFSET);
		final var toLength = getOptionalIntegerValue(row, DEPENDENCIES_COLUMN_TO_LENGTH);
		final var fromLocation = fromOffset != null && fromLength != null ? new ModuleLocation(fromOffset, fromLength) : null;
		final var toLocation = toOffset != null && toLength != null ? new ModuleLocation(toOffset, toLength) : null;

		/* Backwards compatibility: The importer and exporter are using "0" for utility and "-1" for missing modules when writing and reading the module uid */
		final Origin origin = DiscoveryUidUtils.isUtility(targetNid.longValue()) ? Origin.ENVIRONMENT : Origin.CUSTOM;
		final Identification identification = DiscoveryUidUtils.isMissing(targetNid.longValue()) ? Identification.MISSING : Identification.IDENTIFIED;

		referenceImporter.importReference(projectId, uid, targetNid, targetName, null, targetTechnology, targetType, origin, identification, binding, attributes,
				relationship, fromLocation, toLocation, false, validIfReachedFrom);
	}

}
