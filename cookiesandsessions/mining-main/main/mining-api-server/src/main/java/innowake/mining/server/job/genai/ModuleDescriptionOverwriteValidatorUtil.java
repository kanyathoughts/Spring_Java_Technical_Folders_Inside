/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import innowake.mining.shared.entities.ModulePojo;

/**
 * Helper class for checking whether we can overwrite existing descriptions with GenAI generated descriptions.
 */
public class ModuleDescriptionOverwriteValidatorUtil {
	
	private ModuleDescriptionOverwriteValidatorUtil() {
		/* hide implicit constructor */
	}

	/**
	 * Checks whether a {@linkplain Module Modules} description can be overwritten by a GenAI generated description.
	 *
	 * @param module the {@linkplain Module} we want to generate a description for
	 * @param overwriteAll flag that can be set to overwrite all descriptions
	 * @return {@code true} if:
	 * <li>overwriteAll is {@code true}</li>
	 * <li>the existing module description is empty or</li>
	 * <li>the existing module description is null</li>
	 * {@code false} otherwise
	 */
	public static boolean shouldOverwriteModuleDescription(final ModulePojo module, final boolean overwriteAll) {
		if (overwriteAll) {
			return true;
		}
		final String description = module.getDescription().orElse(null);
		return description == null || description.isBlank();
	}
}
