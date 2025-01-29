/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.permission;

import innowake.mining.shared.entities.ModulePojo;

/**
 * Interface for checking whether it is permitted to submit the source code of a module to GenAI. A @Component annotated class implementing this interface
 * can be added to overwrite the default logic.
 */
public interface GenAIModulePermissionChecker {

	/**
	 * Checks whether a Module is allowed to be processed by GenAI. The default implementation permits all modules.
	 *
	 * @param module the Module that is supposed to be processed
	 * @return {@code true} if module is allowed to be processed; {@code false} if not
	 */
	default boolean allowedToBeProcessed(final ModulePojo module) {
		return true;
	}

	/**
	 * Determines the reason why a module is not allowed to be processed by GenAI. The reason will be presented to the user as part of an error message.
	 *
	 * @param module the Module that is supposed to be processed
	 * @return the reason why the module is not allowed to be processed
	 */
	default String getReason(final ModulePojo module) {
		return "restricted by project-specific logic";
	}

}
