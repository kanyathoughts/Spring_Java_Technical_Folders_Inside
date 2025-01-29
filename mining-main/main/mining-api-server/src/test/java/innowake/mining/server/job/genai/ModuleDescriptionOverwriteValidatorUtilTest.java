/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.job.genai;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;

/**
 * Tests behavior of {@linkplain ModuleDescriptionOverwriteValidatorUtil} for various different {@linkplain Module Modules}.
 */
class ModuleDescriptionOverwriteValidatorUtilTest {

	@Test
	void overwriteEmptyDescription() {
		final ModulePojo module = ModulePojoDummy.build(new ModulePojoPrototype()
				.setDescription(""));
		assertTrue(ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module, false), "Empty descriptions should be overwritten.");
	}

	@Test
	void overwriteNullDescription() {
		final ModulePojo module = ModulePojoDummy.build(new ModulePojoPrototype());
		assertTrue(ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module, false), "Null descriptions should be overwritten.");
	}

	@Test
	void doesNotOverwriteExistingDescription() {
		final ModulePojo module = ModulePojoDummy.build(new ModulePojoPrototype()
				.setDescription("Some existing description"));
		assertFalse(ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module, false), "Existing descriptions should not be overwritten.");
	}

	@Test
	void overwriteWithOverwriteFlag() {
		ModulePojo module = ModulePojoDummy.build(new ModulePojoPrototype()
				.setDescription("Some existing description"));
		assertTrue(ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module, true), "Existing descriptions should be overwritten when overwrite flag is set to true.");

		module = ModulePojoDummy.build(new ModulePojoPrototype()
				.setDescription(""));
		assertTrue(ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module, true), "Empty descriptions should be overwritten when overwrite flag is set to true.");
		
		module = ModulePojoDummy.build(new ModulePojoPrototype());
		assertTrue(ModuleDescriptionOverwriteValidatorUtil.shouldOverwriteModuleDescription(module, true), "Null descriptions should be overwritten when overwrite flag is set to true.");
	}

}
