/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.error;

/**
 * Exception that can be thrown when permission to submit source code to an AI model is not there.
 */
public class PermissionException extends UserFacingException {
	
	/**
	 * Constructs a PermissionException for a specific module.
	 * 
	 * @param moduleId the ID of the module of which submitting source code is not permitted
	 */
	public PermissionException(final Long moduleId, final String moduleName, final String reason) {
		super("Submitting source code of the module with ID '" + moduleId + "' and name '" + moduleName + "' is not permitted " + "(" + reason + ").");
	}
	
	/**
	 * Constructs a PermissionException for a specific reason.
	 * 
	 * @param reason the reason for throwing the Exception
	 */
	public PermissionException(final String reason) {
		super(reason);
	}

}
