/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.job;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * Container that holds the actual {@link Serializable} object of the job specific result.
 * This is required to be able to include the type information of the original object into the generated JSON.
 */
public class ResultContainer {

	@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "className")
	private final Serializable object;
	
	/**
	 * Constructor.
	 * 
	 * @param object the result object
	 */
	@JsonCreator
	public ResultContainer(@JsonProperty("object") final Serializable object) {
		this.object = object;
	}
	
	/**
	 * @return the result object
	 */
	public Serializable getObject() {
		return object;
	}
}
