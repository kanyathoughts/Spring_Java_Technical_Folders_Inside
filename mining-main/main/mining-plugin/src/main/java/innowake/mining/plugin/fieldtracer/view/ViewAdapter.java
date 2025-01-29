/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import innowake.ndt.fieldtracing.model.Model;

/**
 * Adapter for methods to change or add event listener to the ui.
 * 
 * @param <O> type of program object
 */
public interface ViewAdapter<O> {

	/**
	 * Set access to the tracing model.
	 *
	 * @param model The model instance. Must not be null.
	 */
	void setModel(Model<O> model);
	
	/**
	 * Used to display an error dialog to the user with the details of
	 * an error.
	 *
	 * @param message The message text to display
	 * @param exception the exception with the error
	 */
	void handleException(String message, Exception exception);
	
}
