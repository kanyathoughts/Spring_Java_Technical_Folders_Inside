/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.opensearch.index.model;

/**
 * Model class for indexing
 */
public class DataPointDefinition {
	
	private String path;
	private String type;
	
	/**
	 * Returns the path
	 *
	 * @return Returns the path
	 */
	public String getPath() {
		return path;
	}
	
	/**
	 * Sets the path
	 *
	 * @param path The path to set
	 */
	public void setPath(String path) {
		this.path = path;
	}
	
	/**
	 * Returns the type
	 *
	 * @return Returns the type
	 */
	public String getType() {
		return type;
	}
	
	/**
	 * Sets the type
	 *
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}
	
	

}
