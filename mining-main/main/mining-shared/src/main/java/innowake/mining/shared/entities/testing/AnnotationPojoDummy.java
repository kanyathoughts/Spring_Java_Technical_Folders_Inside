/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.Collections;
import java.util.UUID;

import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;

/**
 * Annotation for testing purposes.
 */
public class AnnotationPojoDummy extends AnnotationPojoPrototype {
	
	public AnnotationPojoDummy prepare(final BuildingConsumer<AnnotationPojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public AnnotationPojo build() {
		return build(this);
	}
	
	public static AnnotationPojo build(AnnotationPojoPrototype proto) {
		return new AnnotationPojo(
				proto.uid.orElseNonNull(UUID::randomUUID), /* uid (UUID) */
				proto.nid.orElseNonNull(-1L), /* nid (Long) */
				EntityId.VOID, null, null, /* project (EntityId) */
				proto.name.orElseNonNull("DUMMY"), /* name (String) */
				proto.state.orElseNonNull(WorkingState.FOR_REVIEW), /* state (WorkingState) */
				proto.type.orElseNonNull(AnnotationType.RULE), /* type (AnnotationType) */
				proto.categoryId.orElse(null), /* categoryId [Long] */
				null, /* categoryName [String] */
				proto.createdByUserId.orElseNonNull("DUMMY_USER"), /* createdBy (String) */
				proto.updatedByUserId.optional().orElse(null), /* updatedBy [String] */
				proto.module.orElseNonNull(EntityId.VOID), null, null, /* module (EntityId) */
				"" , /* moduleName (String) */
				null, /* modulePath [String] */
				proto.location.orElseNonNull(() -> new ModuleLocation()), /* location (ModuleLocation) */
				null, /* source [UUID] */
				proto.sourceAttachment.orElse(null), /* sourceAttachment [String] */
				proto.englishTranslation.orElse(null), /* englishTranslation [String] */
				proto.reasons.orElseNonNull(Collections::emptyList), /* reasons (Collection<String>) */
				Collections.emptyList(),
				new CustomPropertiesMap(proto.customProperties.orElse(null)) /* customProperties (Map<String, Object>) */
			);
	}
	
}
