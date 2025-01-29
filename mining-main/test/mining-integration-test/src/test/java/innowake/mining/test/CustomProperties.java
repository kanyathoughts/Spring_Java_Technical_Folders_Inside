/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import java.util.Collection;
import java.util.Map;

import org.junit.Assert;

import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.model.Entity;

/**
 * CustomProperty helper functions for test purposes.
 */
public interface CustomProperties {

	/**
	 * Returns the custom property with the given name from the entity.
	 *
	 * @param <T> the concrete {@link Entity} type
	 * @param name the name of the custom property
	 * @param entity the entity from which the custom property should be extracted from
	 * @return the custom property with the given name
	 * @throws IllegalArgumentException if the custom property with the given name is not defined, this should mitigate any typos
	 */
	public static <T extends Entity> Object getCustomPropertyByName(final String name, final T entity) {
		final Object value = entity.getCustomProperties().get(name);

		if (value != null) {
			return value;
		}

		throw new IllegalArgumentException(String.format("The custom property with the name %s does not exist on the entity %s", name, entity));
	}

	public static Object getCustomPropertyByName(final String name, final MiningPojo entity) {
		//get the values directly, as the first level just specifies the custom property Nature, e.g. ModulePojo -> ModuleCustomProperties
		Collection<Object> customProperties = entity.getCustomProperties().values();

		for (final Object prop : customProperties) {
			if (prop instanceof Map) {
				final Object value = ((Map<?, ?>) prop).get(name);
				if (value != null) {
					return value;
				}
			}
		}

		throw new IllegalArgumentException(String.format("The custom property with the name %s does not exist on the entity %s", name, entity));
	}

	/**
	 * Verifies that the given entity has the number of expected custom properties.
	 *
	 * @param <T> the concrete {@link Entity} type
	 * @param entity the entity from which the custom properties should be verified from
	 * @param expectedNumberOfProperties the expected number of custom properties
	 */
	public static <T extends Entity> void verifyNumberOfCustomProperties(final T entity, final int expectedNumberOfProperties) {
		Assert.assertEquals(expectedNumberOfProperties, getNumberOfCustomProperties(entity));
	}
	
	/**
	 * Verifies that the given entity has the number of expected custom properties.
	 *
	 * @param entity the entity from which the custom properties should be verified from
	 * @param expectedNumberOfProperties the expected number of custom properties
	 */
	public static void verifyNumberOfCustomProperties(final MiningPojo entity, final int expectedNumberOfProperties) {
		Assert.assertEquals(expectedNumberOfProperties, getNumberOfCustomProperties(entity));
	}
	
	/**
	 * Derive the number of CustomProperties available in the provided entity
	 *
	 * @param <T> the concrete {@link Entity} type
	 * @param entity the entity from which the custom properties counted
	 * @return number of CustomProperties available in the provided entity
	 */
	public static <T extends Entity> int getNumberOfCustomProperties(final T entity) {
		return entity.getCustomProperties().values().stream().mapToInt(x -> x instanceof Collection ? ((Collection<?>) x).size() : 1).sum();
	}
	
	/**
	 * Derive the number of Custom Properties available in the provided entity
	 *
	 * @param entity the entity from which the custom properties counted
	 * @return number of CustomProperties available in the provided entity
	 */
	@SuppressWarnings("rawtypes")
	public static int getNumberOfCustomProperties(final MiningPojo entity) {
		int count = 0;
		for (final var value : entity.getCustomProperties().values()) {
			if (value instanceof Map) {
				count += ((Map) value).size();
			} else {
				count++;
			}
		}
		return count;
	}
}
