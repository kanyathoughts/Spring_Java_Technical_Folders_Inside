/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.support;

import java.lang.reflect.Field;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.support.AbstractEntityInformation;
import org.springframework.lang.Nullable;

import innowake.lib.core.lang.Assert;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.spring.data.orientdb.commons.exception.MetadataException;

/**
 * Extension of {@link EntityInformation} to capture additional OrientDb specific information about entities.
 * @param <T> type of the entity.
 */
public class OrientEntityInformation<T> extends AbstractEntityInformation<T, String> {

	/**
	 * Initializes the Entity information which contains the orient db specific information about entities.
	 * 
	 * @param domainClass the type of the entity class
	 */
	public OrientEntityInformation(final Class<T> domainClass) {
		super(domainClass);
	}

	@Override
	@Nullable public String getId(final T entity) {
		final T entityRef = Assert.assertNotNull(entity);
		final Set<Field> recordId = Stream.of(entityRef.getClass().getDeclaredFields()).filter(field -> field.isAnnotationPresent(RId.class))
				.collect(Collectors.toSet());
		if (recordId.size() != 1) {
			throw new MetadataException("Only one @RId annotation is allowed in a class hierarchy. Please check annotations in the class " + entityRef.getClass().getSimpleName());
		}
		try {
			final Object id = recordId.iterator().next().get(entity);
			if (id instanceof String) {
				return (String) recordId.iterator().next().get(entity);
			} else {
				throw new IllegalArgumentException("The Id of the record is not a type of string");
			}
		} catch (final IllegalArgumentException | IllegalAccessException e) {
			throw new IllegalStateException("Error accessing the RId.", e);
		}
	}

	@Override
	public Class<String> getIdType() {
		return String.class;
	}

}
