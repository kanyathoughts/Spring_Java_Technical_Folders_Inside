/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;

/**
 * POJO class for a unique Mining entity.
 */
public abstract class MiningPojo {
	
	private final EntityId identity;
	
	@MiningDataPoint(scalarType = ScalarType.JSON)
	private final CustomPropertiesMap customProperties;
	
	@Nullable
	private Map<String, Object> dynamicProperties;
	
	protected MiningPojo(final EntityId identity, final CustomPropertiesMap customProperties) {
		super();
		this.identity = identity;
		this.customProperties = customProperties;
	}
	
	/**
	 * Gets the ID of the entity.
	 * @return Wrapped identifier(s) of the entity.
	 */
	public EntityId identity() {
		return identity;
	}
	
	/**
	 * Gets the unique ID of the entity.
	 * @return unique id
	 */
	public UUID getUid() {
		return identity.getUid();
	}
	
	/**
	 * Gets the numeric ID of the entity.
	 * @return Sequential ID.
	 */
	public Long getId() {
		return identity.getNid();
	}
	
	/**
	 * Returns the {@link Map} containing {@link List} of {@link Set}'s.
	 * @return the {@link Map} containing {@link List} of {@link Set}'s.
	 */
	public CustomPropertiesMap getCustomProperties() {
		return customProperties;
	}
	
	/**
	 * Adds a flexible set of additional properties to the Pojo.
	 * @param fields Map of fields and values.
	 */
	public void defineDynamics(@Nullable final Map<String, Object> fields) {
		if (this.dynamicProperties != null) {
			throw new IllegalStateException("Dynamic properties already defined");
		}
		this.dynamicProperties = fields;
	}
	
	/**
	 * Retrieves a property dynamically fetched along with the Pojo.
	 * @param <T>  Type of the value.
	 * @param field Name of the property.
	 * @return Optional value.
	 */
	@SuppressWarnings("unchecked")
	@Nullable
	public <T> T dynamicNullable(String field) {
		final var props = dynamicProperties;
		if (props != null) {
			return (T) props.get(field);
		}
		return null;
	}
	
	/**
	 * Retrieves a property dynamically fetched along with the Pojo.
	 * @param <T>  Type of the value.
	 * @param field Name of the property.
	 * @return The value.
	 * @throws NoSuchElementException If the property is {@code null} or not present.
	 */
	public <T> T dynamic(String field) {
		final T value = dynamicNullable(field);
		if (value == null) {
			throw new NoSuchElementException(field);
		}
		return value;
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("id", identity.toString());
		builder.append("custom", customProperties);
		if (dynamicProperties != null) {
			builder.append("dynamicProperties", dynamicProperties);
		}
		return builder.toString();
	}
	
}
