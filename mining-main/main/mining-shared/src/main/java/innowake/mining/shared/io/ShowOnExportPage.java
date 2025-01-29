/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.io;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Determines whether the extension format should be displayed on export page.
 */
public class ShowOnExportPage {
	
	/**
	 * Will show extension on export page if {@code true}
	 */
	public final boolean show;
	/**
	 * Category of the extension.
	 */
	public final String category;
	/**
	 * Label of the extension.
	 */
	public final String label;
	
	/**
	 * Initialize an object of {@link ShowOnExportPage} with parameters.
	 * 
	 * @param show {@code true} if to be shown on export page, {@code false} otherwise
	 * @param category of export
	 * @param label the description
	 */
	@JsonCreator
	public ShowOnExportPage(
			@JsonProperty("show") final boolean show,
			@JsonProperty("category") final String category,
			@JsonProperty("label") final String label
			) {
		this.show = show;
		this.category = category;
		this.label = label;
	}
	
	@Override
	public String toString() {
		return "ShowOnExportPage [show=" + show + ", category=" + category + ", label=" + label + "]";
	}
}
