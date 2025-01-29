/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.Collections;
import java.util.UUID;

import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Data Dictionary entry for testing purposes.
 */
public class DataDictionaryPojoDummy extends DataDictionaryPojoPrototype {
	
	public DataDictionaryPojoDummy prepare(final BuildingConsumer<DataDictionaryPojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public DataDictionaryPojo build() {
		return build(this);
	}
	
	public static DataDictionaryPojo build(DataDictionaryPojoPrototype proto) {
		return new DataDictionaryPojo(
				proto.uid.orElseNonNull(UUID::randomUUID),									/* uid (UUID) */
				-1L,																		/* nid (Long) */
				proto.module.orElseNonNull(EntityId.VOID), null, null,						/* module (EntityId) */
				proto.location.orElse(null),												/* location [ModuleLocation] */
				proto.name.orElseNonNull("DUMMY DDE"),										/* name (String) */
				proto.description.orElseNonNull("DUMMY"),									/* description (String) */
				proto.format.orElse(null),													/* format [String] */
				proto.scopes.orElseNonNull(Collections::emptyMap),							/* scope (Map<String>) */
				proto.length.orElse(null),													/* length [Long] */
				proto.createdByUserId.orElseNonNull(() -> "DUMMY_USER"),					/* createdBy (String) */
				proto.updatedByUserId.orElse(null),											/* updatedBy [String] */
				proto.picClause.orElse(null),												/* picClause [String] */
				proto.definedLocation.orElse(null),											/* definedLocation [String] */
				proto.state.orElse(null),													/* state [WorkingState] */
				proto.isBusiness.orElse(null),												/* isBusiness [boolean] */
				proto.fieldTransformation.orElse(null),										/* fieldTransformation [String] */
				proto.sourceInput.orElse(null),												/* sourceInput [String] */
				proto.targetOutput.orElse(null),											/* targetOutput [String] */
				proto.isReferenced.orElse(null),											/* isReferenced [boolean] */
				proto.usage.orElse(null),													/* usage [String] */
				proto.isCandidate.orElseNonNull(false),										/* isCandidate (boolean) */
				proto.fieldLevel.orElse(null),												/* fieldLevel [Long] */
				proto.parentGroup.orElse(null),												/* parentGroup [String] */
				proto.groupPath.orElse(null),												/* groupPath [String] */
				proto.indentation.orElse(null),												/* indentation [Long] */
				proto.initialValue.orElse(null),											/* initialValue [String] */
				proto.translatedFieldValue.orElse(null),									/* translatedFieldValue */
				Collections.emptyList(),													/* annotations (List<EntityId>) */
				new CustomPropertiesMap(proto.customProperties.orElse(null))				/* customProperties (CustomPropertiesMap) */
			);
	}
	
}
