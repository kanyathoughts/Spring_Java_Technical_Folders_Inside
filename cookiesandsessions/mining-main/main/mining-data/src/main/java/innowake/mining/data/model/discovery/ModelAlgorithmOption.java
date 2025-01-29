/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.model.discovery;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.annotations.SerializedName;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Algorithm options used.
 * Provides a description of the algorithm options.
 */
@MiningDataType(name = "ModelAlgorithmOption")
public class ModelAlgorithmOption {

	@SerializedName("name")
	private String name;
	@SerializedName("title")
	private String title;
	@SerializedName("value")
	private String value;

	/**
	 * Parameterized Constructor for ModelAlgorithmOption.
	 * 
	 * @param title Algorithm option title. (Display name)
	 * @param name Algorithm option key. 
	 * @param value Algorithm option value. 
	 */
	@JsonCreator
	public ModelAlgorithmOption(@JsonProperty("name") final String name, @JsonProperty("title") final String title, @JsonProperty("value") final String value) {
		this.name = name;
		this.title = title;
		this.value = value;
	}

	/**
	 * Retrieves the Algorithm options key.
	 *
	 * @return Algorithm options key.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the algorithm options key.
	 *
	 * @param name to set.
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Retrieves the Algorithm options title. It is the display name.
	 *
	 * @return Algorithm options title.
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the algorithm options title. It is the display name.
	 *
	 * @param title to set.
	 */
	public void setTitle(final String title) {
		this.title = title;
	}

	/**
	 * Retrieves the Algorithm options value.
	 *
	 * @return Algorithm options value.
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Sets the algorithm options value.
	 *
	 * @param value to set.
	 */
	public void setValue(final String value) {
		this.value = value;
	}

}
