/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.io;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.SourcePojo;

/**
 * REST data transfer object for references between {@link SourcePojo SourcePojos}.
 */
public class SourceObjectReferences {

	private final String sourcePath;
	private final String[] targetPaths;
	
	/**
	 * Constructor.
	 * 
	 * @param sourcePath the path of the source object 
	 * @param targetPaths the paths of the referenced target objects
	 */
	@JsonCreator
	public SourceObjectReferences(@JsonProperty("sourcePath") final String sourcePath, @JsonProperty("targetPaths") final String[] targetPaths) {
		this.sourcePath = sourcePath;
		this.targetPaths = targetPaths;
	}
	
	/**
	 * Returns the path of the source object.
	 *
	 * @return the path of the source object
	 */
	public String getSourcePath() {
		return sourcePath;
	}
	
	/**
	 * Returns the paths of the referenced target objects.
	 *
	 * @return the paths of the referenced target objects
	 */
	public String[] getTargetPaths() {
		return targetPaths;
	}
}
