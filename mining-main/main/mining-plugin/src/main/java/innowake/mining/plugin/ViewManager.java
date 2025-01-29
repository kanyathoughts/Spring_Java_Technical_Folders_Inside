/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

/**
 * Interface used to manage views. Currently it has only one method used to refresh views.
 */
public interface ViewManager {
	
	/**
	 * Refreshes the view based on a certain event.
	 */
	void refreshView();
}
