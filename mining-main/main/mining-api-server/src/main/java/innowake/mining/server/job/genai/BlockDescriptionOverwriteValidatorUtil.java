/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;

/**
 * Utility class for checking whether we can overwrite existing block descriptions with GenAI generated descriptions.
 */
public class BlockDescriptionOverwriteValidatorUtil {
	
	private BlockDescriptionOverwriteValidatorUtil() {
		/* hide implicit constructor */
	}
	/**
	 * Checks whether a description of a block can be overwritten by a GenAI generated description.
	 *
	 * @param block the {@linkplain FunctionalBlockPojo} we want to generate a description for
	 * @param overwriteAll flag that can be set to overwrite all descriptions
	 * @return {@code true} if:
	 * <li>overwriteAll is {@code true}</li>
	 * <li>the existing block description is empty or</li>
	 * <li>the existing block description is null</li>
	 * {@code false} otherwise
	 */
	public static boolean shouldOverwriteDescription(final FunctionalBlockPojo block, final boolean overwriteAll) {
		if (overwriteAll) {
			return true;
		}
		final String description = block.getDescription();
		return description == null || description.isBlank();
	}

}
