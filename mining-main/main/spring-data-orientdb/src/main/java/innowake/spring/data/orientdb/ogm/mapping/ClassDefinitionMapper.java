/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.WeakHashMap;
import org.apache.commons.lang3.reflect.FieldUtils;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.springdata.annotations.CustomProperties;
import innowake.mining.shared.springdata.annotations.Embedded;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.Relationship;
import innowake.mining.shared.springdata.annotations.Transient;
import innowake.spring.data.orientdb.ogm.proxy.CollectionProxyType;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import org.springframework.data.annotation.PersistenceCreator;

/**
 * Class for computing class definition of an entity class.
 */
public class ClassDefinitionMapper {
	
	private static final Class<?>[] EMPTY_CLASS_ARRAY = new Class<?>[0];
	
	private static final ClassDefinitionMapper instance = new ClassDefinitionMapper();
	private static final Logger LOGGER = LoggerFactory.getLogger(ClassDefinitionMapper.class);
	private final Map<Class<?>, ClassDefinition> classDefintionCache = new WeakHashMap<>();
	
	private ClassDefinitionMapper() {
	}

	/**
	 * Instance of {@link ClassDefinitionMapper}.
	 * @return an instance of {@link ClassDefinitionMapper}
	 */
	public static ClassDefinitionMapper getClassDefinitionMapper() {
		return assertNotNull(instance);
	}

	
	/**
	 * Returns the class definition for the object passed by parameter. 
	 * It returns the class definition from the cache, if it's not found in the cache, then it computes it and adds to the cache.
	 *
	 * @param object reference entity object
	 * @return {@link ClassDefinition} for the given entity object
	 */
	public ClassDefinition getClassDefinition(final Object object) {
		final ClassDefinition defintion = classDefintionCache.get(object);
		if (defintion == null) {
			final Class<?> entityClass;
			if (object instanceof IEntityProxy) {
				entityClass = ((IEntityProxy) object).__getBaseClass();
			} else if (object instanceof Class<?>) {
				entityClass = (Class<?>) object;
			} else if (object.getClass().isAnnotationPresent(Entity.class) || object.getClass().isAnnotationPresent(Embedded.class)) {
				entityClass = object.getClass();
			} else {
				throw new IllegalStateException("The object instance is neither a proxy nor an entity");
			}
			return classDefintionCache.computeIfAbsent(entityClass, this::computeClassDefinition);
		} else {
			return defintion;
		}
	}


	/**
	 * Analyze the class and return a mapped {@link ClassDefinition} with the field definitions.
	 *
	 * @param baseClass reference class
	 * @param classDefinition mapped class definition object
	 */
	private ClassDefinition computeClassDefinition(final Class<?> baseClass) {
		final ClassDefinition classDefinition;
		try {
			final Optional<Constructor<?>> constructor = getPersistenceConstructor(baseClass);
			classDefinition = new ClassDefinition(baseClass, 
					constructor.isPresent() ? constructor.get().getParameterTypes(): EMPTY_CLASS_ARRAY);
		} catch (final SecurityException e) {
			throw new IllegalArgumentException("Could not fetch the constructor for " + baseClass.getSimpleName(), e);
		}
		final Field[] fields;
		if (baseClass.isEnum()) {
			fields = baseClass.getFields();
		} else {
			fields = FieldUtils.getAllFields(baseClass);
		}
		for (final Field field : fields) {
			/* Transient and static final fields are not processed */
			classDefinition.setPropertyField(field);
			if ( ! (field.isAnnotationPresent(Transient.class) || Modifier.isTransient(field.getModifiers()) || Modifier.isStatic(field.getModifiers())
					&& Modifier.isFinal(field.getModifiers())) && ! field.getName().startsWith("$SWITCH")) {				
				if (field.isAnnotationPresent(RId.class) && field.getType().equals(String.class)) {
					classDefinition.setRidField(field);
					continue;
				} else if (field.isAnnotationPresent(Id.class)) {
					classDefinition.addGenerateId(field);
				}
				
				final String fieldName = field.getName();				
				final Class<?> fieldType = field.getType();
				classDefinition.setObjectField(fieldName, field);
				/* mapping data types */
				mapDataTypesToClassDefinition(classDefinition, field, fieldName, fieldType);
			}
		}
		return classDefinition;
	}
	 
	
	private static Optional<Constructor<?>> getPersistenceConstructor(final Class<?> baseClass) {
		final Constructor<?>[] constructors = baseClass.getConstructors();
		if (constructors.length == 1) {
			return Optional.of(constructors[0]);
		}
		final Optional<Constructor<?>> persistanceConstructor = Arrays.stream(constructors)
												.filter(constructor -> constructor.isAnnotationPresent(PersistenceCreator.class))
												.findAny();
		if (persistanceConstructor.isPresent()) {
			return persistanceConstructor;
		} else {
			return Arrays.stream(constructors).filter(constructor -> constructor.getParameterCount() == 0).findAny();
		}
	}

	private void mapDataTypesToClassDefinition(final ClassDefinition classDefinition, final Field field, final String fieldName, final Class<?> fieldType) {
		if (field.isAnnotationPresent(CustomProperties.class)) {
			classDefinition.setCustomPropertyField(field);
		} else if (Primitives.isSupportedDataType(fieldType)) {	
			classDefinition.setPrimitiveField(fieldName, field);
		} else if (fieldType.isEnum()) {
			classDefinition.setEnumField(fieldName, field);
		} else if (CollectionProxyType.isCollectionTypeSupported(fieldType)) {
			mapCollectionToClassDefintion(classDefinition, field);
		} else if (fieldType.isAnnotationPresent(Embedded.class)) {
			classDefinition.setEmbeddedField(fieldName, field);
		} else if (field.isAnnotationPresent(Relationship.class)) {
			classDefinition.setEdgeLink(fieldName, field);
		} else {
			classDefinition.setLink(fieldName, field);
		}
	}

	private void mapCollectionToClassDefintion(final ClassDefinition classDefinition, final Field field) {
		final String fieldName = field.getName();
		final Class<?> fieldType = field.getType();
		boolean setAsRelationship = false;
		boolean isEnumCollection = false;
		if (List.class.isAssignableFrom(fieldType) || Set.class.isAssignableFrom(fieldType)) {
			LOGGER.debug(() -> "Field : " + fieldName + " is of type Set or List. Class :" + classDefinition.getEntityName());
			final Class<?> collectionClass = ObjectMapper.getObjectMapper().getListType(field);
			if (collectionClass.isEnum()) {
				isEnumCollection = true;
			} else if (Primitives.isSupportedDataType(collectionClass)) {
				setAsRelationship = true;
			}
		} else if (Map.class.isAssignableFrom(fieldType)) {
			LOGGER.debug(() -> "Field is of type Map");
			final ParameterizedType listType = (ParameterizedType) field.getGenericType();
			/* the key should always be string only */
			final Class<?> keyClass = (Class<?>) listType.getActualTypeArguments()[0];
			
			final Class<?> valClass;
			if (listType.getActualTypeArguments()[1] instanceof java.lang.reflect.ParameterizedType) {
				final Type[] types = ((ParameterizedType) listType.getActualTypeArguments()[1]).getActualTypeArguments();
				int noOfParameters = types.length;
				if (noOfParameters > 1) {
					throw new IllegalArgumentException("the value parameter of the map type " + fieldName + " is not supported");
				}
				valClass = (Class<?>) types[0];
			} else {
				valClass = (Class<?>) listType.getActualTypeArguments()[1];
			}

			/* For a map to be relationship it must have the key: string and if the value is a primitive directly embed it.*/
			if (keyClass == String.class && Primitives.isSupportedDataType(valClass)) {
				LOGGER.debug(() -> "Relationship class: " + valClass.getSimpleName());
				setAsRelationship = true;
			}
		}
		if (setAsRelationship) {
			/* Is a collection of primitives. Treat it like a common field */
			classDefinition.setPrimitiveField(fieldName, field);
		} else if (isEnumCollection) {
			classDefinition.setEnumCollectionField(fieldName, field);
		} else if (field.isAnnotationPresent(Relationship.class)) {
			/* It is a collection of objects that generates Vertexes and Edges. */
			classDefinition.setEdgeLinkList(fieldName, field);
		} else {
			classDefinition.setLinkList(fieldName, field);
		}
	}
	
}
