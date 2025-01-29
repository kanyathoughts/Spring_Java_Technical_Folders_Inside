/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.FieldName;

/**
 * Enum for fields that can be used in aggregations of {@link FunctionalBlockPojo}.
 */
public enum FunctionalBlockFieldName implements FieldName {
	/**
	 * UUID of the functional blocks - used mostly for counting
	 */
	UID,
	/**
	 * The {@link FunctionalBlockType} of a functional block..
	 * @see innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder#withType(FunctionalBlockType)
	 */
	TYPE,
	/**
	 * The project the functional block belongs to.
	 * @see innowake.mining.shared.access.FunctionalBlockService.FunctionalBlockInquiryBuilder#ofProject(EntityId)
	 */
	PROJECT_ID,
	/**
	 * The technology of referenced modules of the functional blocks.
	 */
	REFERENCED_MODULE_TECHNOLOGY,
	/**
	 * The type of referenced modules of the functional blocks.
	 */
	REFERENCED_MODULE_TYPE
}
