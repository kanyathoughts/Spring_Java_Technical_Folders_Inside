/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.generic;

/**
 * This enum holds a constant for the specific language and the corresponding content-type-id
 */
public enum ContentType {

	BASIC_CONTENT_TYPE("mining-plugin.contentType.basic");

	private final String id;
	
	private ContentType(final String id) {
		this.id = id;
	}
	
	/**
	 * Gets the id for the selected content type
	 *
	 * @return content type id string
	 */
	public String getId() {
		return id;
	}
}
