/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.universalsearch;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Domain class for Universal search link.
 */
public class UniversalSearchLink {

	private final Type type;
	private final Map<String, String> properties;

	/**
	 * Search result types.
	 */
	public enum Type {
		MODULE_DETAILS, CODE_VIEWER, DATA_DICTIONARY_TABLE
	}

	@JsonCreator
	private UniversalSearchLink(@JsonProperty("type") final Type type, @JsonProperty("properties") final Map<String, String> properties) {
		this.type = type;
		this.properties = properties;
	}

	/**
	 * Returns a link to module details page.
	 *
	 * @param moduleId the moduleId
	 * @return a link to module details page
	 */
	public static UniversalSearchLink forModuleDetailsPage(final Long moduleId) {
		return new UniversalSearchLink(Type.MODULE_DETAILS, Map.of("moduleId", moduleId.toString()));
	}

	/**
	 * Returns a link for code viewer page with offset location.
	 *
	 * @param moduleId the module id
	 * @param location the module location offsets
	 * @return a link for code viewer page with offset location
	 */
	public static UniversalSearchLink forCodeViewer(final Long moduleId, @Nullable final ModuleLocation location) {
		final Map<String, String> properties = new HashMap<>();
		properties.put("moduleId", moduleId.toString());
		if (location != null) {
			properties.put("offset", location.getOffset().toString());
			properties.put("length", location.getLength().toString());
		}
		return new UniversalSearchLink(Type.CODE_VIEWER, properties);
	}

	/**
	 * Gets the link type.
	 *
	 * @return the link type
	 */
	public Type getType() {
		return type;
	}

	/**
	 * Gets the properties required to build the link result. 
	 *
	 * @return the properties required to build the link result
	 */
	public Map<String, String> getProperties() {
		return properties;
	}
}
