/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.extensions;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Model for holding response for the {@link CustomTableExtension}.
 */
public class CustomTableExtensionDescription {
	private final String identifier;
	private final String name;
	private final String description;
	private final String queryName;
	private final String rootTypeName;
	private final String usage;

	@JsonCreator
	public CustomTableExtensionDescription(@JsonProperty("identifier") final String identifier,
										   @JsonProperty("name") final String name,
										   @JsonProperty("description") final String description,
										   @JsonProperty("queryName") final String queryName,
										   @JsonProperty("rootTypeName") final String rootTypeName,
										   @JsonProperty("usage") final String usage) {
		this.identifier = identifier;
		this.name = name;
		this.description = description;
		this.queryName = queryName;
		this.rootTypeName = rootTypeName;
		this.usage = usage;
	}

	public static CustomTableExtensionDescription fromCustomTableExtension(final CustomTableExtension extension) {
		return new CustomTableExtensionDescription(extension.getIdentifier(), extension.getName(), extension.getDescription(),
				extension.getQueryName(), extension.getRootTypeName(), extension.getUsage());
	}

	/**
	 * An identifier for this extension.
	 * <p>
	 * This identifier must be unique because it is used in the URL for the custom page
	 *
	 * @return a unique identifier
	 */
	public String getIdentifier() {
		return identifier;
	}

	/**
	 * A short name for the custom table which is shown in the menu.
	 * @return a short name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Short descriptive text of this extension.
	 *
	 * @return a short description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Name of the GraphQL root query that is used to get the data for the table.
	 * @return the name of the GraphQL query
	 */
	public String getQueryName() {
		return queryName;
	}

	/**
	 * The mining data point (and GraphQL) schema type that is returned by the query. Used to retrieve the applicable data points.
	 *
	 * @return name of the root type of the query
	 */
	public String getRootTypeName() {
		return rootTypeName;
	}

	/**
	 * The usage for data points that are to be displayed on the table.
	 *
	 * @return the data point usage
	 */
	public String getUsage() {
		return usage;
	}
}