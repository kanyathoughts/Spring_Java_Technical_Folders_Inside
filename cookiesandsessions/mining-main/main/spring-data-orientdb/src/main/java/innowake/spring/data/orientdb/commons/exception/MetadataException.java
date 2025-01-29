/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.exception;

/**
 * Exception thrown when there is an user error in entity metadata such as conflicting annotations,
 * missing id fields etc or when no proper matching schema is found.
 */
public class MetadataException extends RuntimeException {
	
	/**
	 * Constructor for a custom metadata exception when there is a user error in entity metadata.
	 */
	public MetadataException() {
		super("Error in the database schema");
	}
	
	/**
	 * Constructor for a custom metadata exception when there is a user error in entity metadata.
	 * 
	 * @param message to be displayed in the case of metadata exception
	 */
	public MetadataException(final String message) {
		super(message);
	}
}
