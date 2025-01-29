/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.lang.reflect.Constructor;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.shared.LegacyJsonUtil;

/**
 * Utilities for conversion of DTO object from {@code discovery-shared} into a neutral string representation used for property storage.
 */
public class PropertyUtils {

	private PropertyUtils() {}
	
	/**
	 * Converts a {@link ProjectData} to a neutral string representation independent from type {@link ProjectData}. 
	 * The resulting string can again be converted to a {@link ProjectData} using #fromJson(String, Class)}.
	 *
	 * @param object a {@code ProjectDTO} object
	 * @return the string representation
	 */
	@Nullable
	public static String toString(@Nullable final ProjectData object) {
		if (object == null) {
			return null;
		}
		try {
			return LegacyJsonUtil.getMapper().writeValueAsString(new Property(object));
		} catch (final JsonProcessingException e) {
			throw new IllegalStateException(e);
		}
	}
	
	/**
	 * Converts a string representation to an instance of type {@code T}. 
	 * The string must be created before using {@link #toString(ProjectData)}.
	 *
	 * @param json the string representation
	 * @param classOfT the target type
	 * @return an instance of type {@code T}
	 * @throws CoreException of no instance can be created
	 */
	@Nullable
	public static <T> T fromString(final String json, final Class<T> classOfT) throws CoreException {
		try {
			if (StringUtils.isNotBlank(json)) {
				final Property property = LegacyJsonUtil.getMapper().readValue(json, Property.class);
				return property != null ? property.to(classOfT) : null;
			} else {
				return null;
			}
		} catch (final JsonProcessingException e) {
			throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Cannot process JSON: " + json, e));
		}
	}
	
	private static class Property {
		
		private Long projectIdProperty;
		private String projectNameProperty;
		private Long clientIdProperty;
		private String clientNameProperty;
		
		@JsonCreator
		private Property(@JsonProperty("projectIdProperty") final Long projectIdProperty, @JsonProperty("projectNameProperty") final String projectNameProperty,
				@JsonProperty("clientIdProperty") final Long clientIdProperty, @JsonProperty("clientNameProperty") final String clientNameProperty) {
			this.projectIdProperty = projectIdProperty;
			this.projectNameProperty = projectNameProperty;
			this.clientIdProperty = clientIdProperty;
			this.clientNameProperty = clientNameProperty;
		}
		
		private Property(final ProjectData object) {
			projectIdProperty = object.getProjectId();
			projectNameProperty = object.getProjectName();
			clientIdProperty = object.getClientId();
			clientNameProperty = object.getClientName();
		}
		
		private <T> T to(final Class<T> classOfT) throws CoreException {
			try {
				final Constructor<T> constructor = classOfT.getConstructor(Long.class, String.class, Long.class, String.class);
				return constructor.newInstance(projectIdProperty, projectNameProperty, clientIdProperty, clientNameProperty);
			} catch (final Exception e) {
				throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "Cannot instantiate class " + classOfT.getName(), e));
			}
		}
		
	}
}
