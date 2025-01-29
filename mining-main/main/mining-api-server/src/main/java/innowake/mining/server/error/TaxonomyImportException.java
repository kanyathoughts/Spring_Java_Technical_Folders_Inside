/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.error;

/**
 * Exception thrown if import of taxonomy file failed due to invalid taxonomy data.    
 */
public class TaxonomyImportException extends Exception {

	/**
	 * Constructs a TaxonomyImportException with the specified message.
	 * 
	 * @param message the detail message
	 */
	public TaxonomyImportException(final String message) {
		super(message);
	}
}
