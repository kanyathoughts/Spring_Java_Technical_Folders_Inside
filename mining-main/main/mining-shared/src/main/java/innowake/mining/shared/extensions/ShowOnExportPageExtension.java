/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.extensions;

import innowake.mining.shared.io.ShowOnExportPage;

/**
 * This interface can be implemented to optionally display extensions on the Export page.
 */
public interface ShowOnExportPageExtension {

	/**
	 * Get {@link ShowOnExportPage} object for implementing extension.
	 *
	 * @return {@link ShowOnExportPage} object
	 */
	ShowOnExportPage getShowOnExportPage();
}