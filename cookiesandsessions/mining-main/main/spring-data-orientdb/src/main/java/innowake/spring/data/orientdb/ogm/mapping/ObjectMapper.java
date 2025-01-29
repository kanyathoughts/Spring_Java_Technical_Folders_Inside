/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.mapping;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.beans.IntrospectionException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.commons.exception.AccessorMethodNotFoundException;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Maps the entity class with {@link ClassDefinition} to prepare a map of fields based on its type. 
 * It also prepares an {@link ObjectStruct} to hold the values of based on class definition.
 */
public class ObjectMapper {

	private static @Nullable ObjectMapper instance;
	private static final Logger LOGGER = LoggerFactory.getLogger(ObjectMapper.class);

	private ObjectMapper() {
	}

	/**
	 * Instance of {@link ObjectMapper}.
	 * @return an instance of {@link ObjectMapper}
	 */
	public static synchronized ObjectMapper getObjectMapper() {
		if (instance == null) {
			instance = new ObjectMapper();
		}
		return assertNotNull(instance);
	}

	/**
	 * Maps the values of the entity based on class definition. 
	 *
	 * @param objectEntity object to analyze
	 * @return an object with the structure of the object analyzed
	 */
	public ObjectStruct computeObjectStruct(final Object objectEntity) {
		final ObjectStruct oStruct = new ObjectStruct();
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(objectEntity);
		/* Computes object structure for fields of primitive, enum or enum collection data types */
		computeObjectStructureForDefinedTypes(objectEntity, oStruct, classDefinition);

		/* Process embedded fields */
		final Map<String, Object> embeddedLinks = new HashMap<>();
		classDefinition.getEmbeddedFields().values().forEach(field -> putValue(embeddedLinks, field, objectEntity, null));
		oStruct.setEmbeddedFields(embeddedLinks);

		/* Process all the links */
		final Map<String, Object> links = new HashMap<>();
		classDefinition.getLinks().values().forEach(field -> putValue(links, field, objectEntity, null));
		oStruct.setLinks(links);

		/* Process all the linkLists */
		final Map<String, Object> linkLists = new HashMap<>();
		classDefinition.getLinkLists().values().forEach(field -> putValue(linkLists, field, objectEntity, null));
		oStruct.setLinkList(linkLists);

		/* Process all the edge links */
		final Map<String, Object> edgeLinks = new HashMap<>();
		classDefinition.getEntityEdges().values().forEach(field -> putValue(edgeLinks, field, objectEntity, null));
		oStruct.setEdgeLinks(edgeLinks);

		/* Process all the edge link lists */
		final Map<String, Object> edgeLinkLists = new HashMap<>();
		classDefinition.getCollectionOfEntityEdges().values().forEach(field -> putValue(edgeLinkLists, field, objectEntity, null));
		oStruct.setEdgeLinkList(edgeLinkLists);
		return oStruct;
	}

	/**
	 * Returns the type of {@link List}.
	 *
	 * @param listField the list data field in entity class
	 * @return the type of {@link List}
	 */
	public Class<?> getListType(final Field listField) {
		final ParameterizedType listType = (ParameterizedType) listField.getGenericType();
		return (Class<?>) listType.getActualTypeArguments()[0];
	}

	/**
	 * Iterates through embedded and enum fields, sets suitable proxy instance for the collection type.
	 * 
	 * @param proxyObject proxy instance of an entity class
	 */
	public void convertPrimitiveCollectionsToEmbedded(final IEntityProxy proxyObject) {
		final ClassDefinition classDef = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxyObject);
		classDef.getEmbeddedFields().values().forEach(field -> mapEmbeddedCollectionFields(proxyObject, field));
		classDef.getEnumCollectionFields().values().forEach(field -> mapEmbeddedCollectionFields(proxyObject, field));
	}
	
	private void mapEmbeddedCollectionFields(final IEntityProxy proxyObject, final Field field) {
		final Object value = collectionToEmbedded(field.getType(), proxyObject.__getFieldValue(field));
		if (value != null) {
			proxyObject.__setFieldValue(value, field);
		}
	}

	private void computeObjectStructureForDefinedTypes(final Object objectEntity, final ObjectStruct oStruct, final ClassDefinition classDefinition) {
		/* Process all the fields */
		final Map<String, Object> primitiveFields = new HashMap<>();
		/* Process all the primitive fields. */
		classDefinition.getPrimitiveFields().values().forEach(field -> putValue(primitiveFields, field, objectEntity, null));
		oStruct.setFields(primitiveFields);

		/* Process all the enums */
		final Map<String, Object> enumFields = new HashMap<>();
		classDefinition.getEnumFields().values()
				.forEach(field -> putValue(enumFields, field, objectEntity, enumValue -> ((Enum<?>) enumValue).name()));
		oStruct.setEnumLinks(enumFields);

		/* Process all the enum collections */
		final Map<String, Object> enumListFields = new HashMap<>();
		classDefinition.getEnumCollectionFields().values().forEach(field -> putValue(enumListFields, field, objectEntity,
				value -> ((Collection<?>) value).stream().map(enumValue -> ((Enum<?>) enumValue).name()).collect(Collectors.toList())));
		oStruct.setEnumCollectionLinks(enumListFields);
	}

	@SuppressWarnings("unchecked")
	@Nullable
	private Object collectionToEmbedded(final Class<?> fieldType, @Nullable final Object value) {
		try {
			final boolean isProxyFieldValueNotNull = value != null;
			if (List.class.isAssignableFrom(fieldType)) {
				final List<Object> listValue;
				if (isProxyFieldValueNotNull) {
					listValue = (List<Object>) value;
				} else {
					listValue = new ArrayList<>();
				}
				return listValue;
			} else if (Map.class.isAssignableFrom(fieldType)) {
				final Map<Object, Object> mapValue;
				if (isProxyFieldValueNotNull) {
					mapValue = (Map<Object, Object>) value;
				} else {
					mapValue = new HashMap<>();
				}
				return mapValue;
			}
		} catch (final IllegalArgumentException ex) {
			throw new EntityProxyMappingException("Error converting collections to embedded for field ", ex);
		}
		return null;
	}

	private void putValue(final Map<String, Object> valuesMap, final Field field, final Object entityObject, @Nullable final UnaryOperator<Object> transform) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(entityObject.getClass());
		try {
			@Nullable
			final Object value = classDefinition.getReadMethod(field).invoke(entityObject);
			if (value != null) {
				valuesMap.put(field.getName(), transform != null ? transform.apply(value) : value);
			}
		} catch (final IntrospectionException | IllegalArgumentException | IllegalAccessException | InvocationTargetException e) {
			throw new AccessorMethodNotFoundException("Error Accessing getter method " + field, e);
		}
	}

}
