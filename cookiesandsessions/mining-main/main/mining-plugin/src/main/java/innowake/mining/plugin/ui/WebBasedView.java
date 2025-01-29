/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.ui;

import org.eclipse.ui.IWorkbenchPart;

/**
 * View interacting with the Mining UI through a web based component.
 * 
 * @param <T> the type of the entity to show
 */
public interface WebBasedView<T> extends IWorkbenchPart {

	/**
	 * Initializes the web view, by loading the associated URL.
	 */
	void init();
	
	/**
	 * Initializes the web view with the given value, by loading the associated URL.
	 *
	 * @param value the value this view should be initialized with
	 */
	void init(T value);

	/**
	 * Refreshes the web view.
	 */
	void refresh();
}
