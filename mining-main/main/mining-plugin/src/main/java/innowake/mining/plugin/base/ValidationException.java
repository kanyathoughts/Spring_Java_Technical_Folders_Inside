/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

import innowake.lib.core.api.lang.Nullable;

/**
 * Exception signaling an invalid state.
 */
public class ValidationException extends Exception {

	private final String title;
	
	/**
	 * Creates a new validation exception.
	 * 
	 * @param title the title, can be used when an error dialog is shown 
	 * @param message the detail message 
	 */
	public ValidationException(final String title, final String message) {
		this(title, message, null);
	}
	
	/**
	 * Creates a new validation exception.
	 * 
	 * @param title the title, can be used when an error dialog is shown 
	 * @param message the detail message 
	 * @param cause the root cause of the validation exception
	 */
	public ValidationException(final String title, final String message, final @Nullable Throwable cause) {
		super(message, cause);
		this.title = title;
	}
	
	/**
	 * Returns the title of the exception, which can be used for message dialogs.
	 * 
	 * @return the title of the exception
	 */
	public String getTitle() {
		return title;
	}
}
