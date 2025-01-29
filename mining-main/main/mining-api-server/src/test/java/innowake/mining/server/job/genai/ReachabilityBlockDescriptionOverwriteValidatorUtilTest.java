/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.job.genai;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;

/**
 * Test behavior of {@linkplain BlockDescriptionOverwriteValidatorUtil}.
 */
class ReachabilityBlockDescriptionOverwriteValidatorUtilTest {
	
	private static final UUID BLOCK_UUID = UUID.fromString("b44ca2ce-6b77-458f-b871-d2110bcc029e");
	private static final Long PROJECT_ID = 1l;

	@Test
	void overwriteEmptyDescription() {
		final FunctionalBlockPojo block = new FunctionalBlockPojo(BLOCK_UUID, null, EntityId.of(PROJECT_ID), null, null, null, "block", "", null,
				null);
		assertTrue(BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, false),
				"Empty descriptions should be overwritten.");
	}

	@Test
	void overwriteNullDescription() {
		final FunctionalBlockPojo block = new FunctionalBlockPojo(BLOCK_UUID, null, EntityId.of(PROJECT_ID), null, null, null, "block", null, null,
				null);
		assertTrue(BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, false),
				"Null descriptions should be overwritten.");
	}

	@Test
	void doesNotOverwriteExistingDescription() {
		final FunctionalBlockPojo block = new FunctionalBlockPojo(BLOCK_UUID, null, EntityId.of(PROJECT_ID), null, null, null, "block", "some description",
				null, null);
		assertFalse(BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, false),
				"Existing descriptions should not be overwritten.");
	}

	@Test
	void overwriteWithOverwriteFlag() {
		FunctionalBlockPojo block = new FunctionalBlockPojo(BLOCK_UUID, null, EntityId.of(PROJECT_ID), null, null, null, "block", "some description", null, null);
		assertTrue(BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, true),
				"Existing descriptions should be overwritten when overwrite flag is set to true.");

		block = new FunctionalBlockPojo(BLOCK_UUID, null, EntityId.of(PROJECT_ID), null, null, null, "block", "", null, null);
		assertTrue(BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, true),
				"Empty descriptions should be overwritten when overwrite flag is set to true.");

		block = new FunctionalBlockPojo(BLOCK_UUID, null, EntityId.of(PROJECT_ID), null, null, null, "block", null, null, null);
		assertTrue(BlockDescriptionOverwriteValidatorUtil.shouldOverwriteDescription(block, true),
				"Null descriptions should be overwritten when overwrite flag is set to true.");
	}

}
