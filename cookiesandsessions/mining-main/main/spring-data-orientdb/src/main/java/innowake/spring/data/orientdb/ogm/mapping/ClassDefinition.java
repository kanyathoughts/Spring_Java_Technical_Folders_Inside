/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.mapping;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import com.orientechnologies.orient.core.record.ODirection;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.CustomProperties;
import innowake.mining.shared.springdata.annotations.Embedded;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.Property;
import innowake.mining.shared.springdata.annotations.Relationship;

/**
 * Class definition for a given entity class. It maps the field objects, links(i.e., vertex & edge ) information. 
 */
public class ClassDefinition {

	private final Map<Field, PropertyDescriptor> propertyDescriptorCache = new ConcurrentHashMap<>();
	private final Map<Field, EdgeInformation> edgeRelationShipCache = new WeakHashMap<>();
	
	/**
	 * Entity class.
	 */
	private Class<?> entityClass;

	/**
	 * Field to be used to inject the RID if the appropriate annotation exists.
	 */
	@Nullable private Field ridField;
	
	/**
	 * Fields to be used to fetch objects by custom Id.
	 */
	private final List<Field> generateIds = new ArrayList<>();
	
	/**
	 * Field to be used to fetch custom properties of the entity class.
	 */
	@Nullable private Field customPropertyField;

	/**
	 * Map of all Field objects (doesn't include the ridField). All fields have
	 * already set by the private access (setAccessible as true).
	 */
	private final Map<String, Field> objectFields = new HashMap<>();

	/**
	 * Map of basic attributes (includes the embeddedFields)
	 */
	private final Map<String, Field> primitiveFields = new HashMap<>();

	/**
	 * Map of enum attributes.
	 */
	private final Map<String, Field> enumFields = new HashMap<>();

	/**
	 * Map of attributes that are collections of enums (only Lists).
	 */
	private final Map<String, Field> enumCollectionFields = new HashMap<>();

	/**
	 * Map of embedded fields whose type is annotated with @Embedded.
	 */
	private final Map<String, Field> embeddedFields = new HashMap<>();

	/**
	 * Map of links to other objects.
	 */
	private final Map<String, Field> entityLinks = new HashMap<>();

	/**
	 * Map of links to  collection of other objects.
	 */
	private final Map<String, Field> collectionOfEntityLinks = new HashMap<>();
	
	/**
	 * Map of entities to other objects linked via edges.
	 */
	private final Map<String, Field> entityEdges = new HashMap<>();

	/**
	 * Map of edge links to  collection of other objects.
	 */
	private final Map<String, Field> collectionOfEntityEdges = new HashMap<>();
	
	/**
	 * Map of java Field to DB entities.
	 */
	private final Map<String, String> javaFieldToDBFieldMap = new HashMap<>();
	
	/**
	 * Map of DB entities to Java Field.
	 */
	private final Map<String, String> dbFieldToJavaFieldMap= new HashMap<>();
	
	
	private final Class<?>[] parameterTypes;
	
	ClassDefinition(final Class<?> entityClass, final Class<?>[] parameterTypes) {
		this.entityClass = entityClass;
		this.parameterTypes = parameterTypes;
	}
	
	/**
	 * Returns the entity name based on @Entity annotation.
	 *
	 * @return class name
	 */
	public String getEntityName() {
		String name = null;

		@Nullable final Entity entity = entityClass.getAnnotation(Entity.class);
		if (entity == null) {
			@Nullable final Embedded embedded = entityClass.getAnnotation(Embedded.class);
			if (embedded != null) {
				name = embedded.name();
			}
		} else {
			name = entity.name();
		}

		return name == null || name.isEmpty() ? entityClass.getSimpleName() : name;
	}

	/**
	 * A field annotated with {@code @RID}.
	 * 
	 * @return the field marked with annotation @RID
	 */
	@Nullable public Field getRidField() {
		return ridField;
	}

	/**
	 * Set the value to be marked as RID.
	 *
	 * @param ridField field marked with @RID annotation
	 */
	public void setRidField(final Field ridField) {
		this.ridField = ridField;
	}

	/**
	 * Gets the fields annotated with {@link Id}.
	 *
	 * @return fields annotated with {@link Id}.
	 */
	public List<Field> getGenerateIds() {
		return generateIds;
	}

	/**
	 * Adds a field annotated with {@link Id}.
	 * 
	 * @param generateId field annotated with {@link Id}
	 */
	public void addGenerateId(final Field generateId) {
		generateIds.add(generateId);
	}

	/**
	 * Gets the field annotated with {@link CustomProperties}.
	 *
	 * @return the field annotated with {@link CustomProperties}
	 */
	@Nullable public Field getCustomPropertyField() {
		return customPropertyField;
	}

	/**
	 * Field annotated with {@link CustomProperties} in the entity class.
	 *
	 * @param customProperty the field to hold all custom properties
	 */
	public void setCustomPropertyField(final Field customProperty) {
		this.customPropertyField = customProperty;
	}

	/**
	 * All the object fields in the entity class.
	 * 
	 * @return collection of all fields in entity class
	 */
	public Collection<Field> getObjectFields() {
		return Collections.unmodifiableCollection(objectFields.values());
	}

	/**
	 * 
	 * Get the respective field name from map of all field objects.
	 *
	 * @param fieldName Field name of the Entity
	 * @return an {@link Optional} field name from map of all Field objects.
	 */
	public Optional<Field> getFieldByName(final String fieldName) {
		return Optional.ofNullable(objectFields.get(fieldName));
	}
	
	/**
	 * Set collection of all fields in entity class.
	 * 
	 * @param fieldName name of the property in entity class
	 * @param field property in entity class
	 */
	public void setObjectField(final String fieldName, final Field field) {
		objectFields.put(fieldName, field);
	}

	/**
	 * Return a map of primitive data fields by it's name.
	 * 
	 * @return collection of primitive data fields
	 */
	public Map<String, Field> getPrimitiveFields() {
		return Collections.unmodifiableMap(primitiveFields);
	}

	/**
	 * Segregates the primitive data fields in entity class.
	 *
	 * @param fieldName name of the property in entity class
	 * @param primitiveField primitive property in entity class
	 */
	public void setPrimitiveField(final String fieldName, final Field primitiveField) {
		primitiveFields.put(fieldName, primitiveField);
	}

	/**
	 * Returns a map of enum field names along with enum type.
	 * 
	 * @return  fields with type information
	 */
	public Map<String, Field> getEnumFields() {
		return Collections.unmodifiableMap(enumFields);
	}

	/**
	 * Sets enum field type information.
	 * 
	 * @param fieldName name of the enum property in entity class
	 * @param enumField enum field
	 */
	public void setEnumField(final String fieldName, final Field enumField) {
		enumFields.put(fieldName, enumField);
	}

	/**
	 * Returns a map of collection of enum field names along with enum type.
	 * 
	 * @return collection of enum fields
	 */
	public Map<String, Field> getEnumCollectionFields() {
		return Collections.unmodifiableMap(enumCollectionFields);
	}

	/**
	 * Sets collection enum fields with type information.
	 *
	 * @param fieldName name of the enum property in entity class
	 * @param enumCollectionField enum field
	 */
	public void setEnumCollectionField(final String fieldName, final Field enumCollectionField) {
		enumCollectionFields.put(fieldName, enumCollectionField);
	}

	/**
	 * Returns embedded type fields in entity class.
	 * 
	 * @return collection of embedded data fields in the entity class
	 */
	public Map<String, Field> getEmbeddedFields() {
		return Collections.unmodifiableMap(embeddedFields);
	}

	/**
	 * Sets the embedded data field.
	 *
	 * @param fieldName name of the embedded field property in entity class
	 * @param embeddedField embedded field
	 */
	public void setEmbeddedField(final String fieldName, final Field embeddedField) {
		embeddedFields.put(fieldName, embeddedField);
	}

	/**
	 * Returns a map of fields of another entity type.
	 * 
	 * @return all the relationship properties
	 */
	public Map<String, Field> getLinks() {
		return Collections.unmodifiableMap(entityLinks);
	}

	/**
	 * Set the relationship field to map.
	 * 
	 * @param fieldName name of the vertex/relationship property in entity class
	 * @param linkField relational field to another entity
	 */
	public void setLink(final String fieldName, final Field linkField) {
		entityLinks.put(fieldName, linkField);
	}

	/**
	 * Returns a map consisting of relation fields of collection type.
	 * 
	 * @return all the relation fields in the entity class
	 */
	public Map<String, Field> getLinkLists() {
		return Collections.unmodifiableMap(collectionOfEntityLinks);
	}

	/**
	 * Sets the collection relationship field to map
	 *
	 * @param fieldName name of the vertex/relationship property in entity class
	 * @param linkListsField collection field of an entity class
	 */
	public void setLinkList(final String fieldName, final Field linkListsField) {
		collectionOfEntityLinks.put(fieldName, linkListsField);
	}
	
	/**
	 * Set the relationship field to map.
	 * 
	 * @param fieldName name of the vertex/relationship property in entity class
	 * @param linkField relational field to another entity
	 */
	public void setEdgeLink(final String fieldName, final Field linkField) {
		entityEdges.put(fieldName, linkField);
		setEdgeRelationshipInfo(linkField);
	}
	
	/**
	 * Returns a map of fields of another entity type.
	 * 
	 * @return all the relationship properties
	 */
	public Map<String, Field> getEntityEdges() {
		return Collections.unmodifiableMap(entityEdges);
	}
	
	/**
	 * Sets the collection relationship field to map
	 *
	 * @param fieldName name of the vertex/relationship property in entity class
	 * @param linkListsField collection field of an entity class
	 */
	public void setEdgeLinkList(final String fieldName, final Field linkListsField) {
		collectionOfEntityEdges.put(fieldName, linkListsField);
		setEdgeRelationshipInfo(linkListsField);
	}

	/**
	 * Returns an unmodifiable map consisting of relation fields of collection type.
	 * 
	 * @return all the relation fields in the entity class
	 */
	public Map<String, Field> getCollectionOfEntityEdges() {
		return Collections.unmodifiableMap(collectionOfEntityEdges);
	}

	/**
	 * Returns the setter method for a field from the entity class.
	 *  
	 * @param field the field
	 * @return the setter method for a given field
	 * @throws IntrospectionException if a proper setter method is not found
	 */
	public Method getWriteMethod(final Field field) throws IntrospectionException {
		PropertyDescriptor propertyDescriptor = propertyDescriptorCache.get(field);
		if (propertyDescriptor == null) {
			propertyDescriptor = new PropertyDescriptor(field.getName(), entityClass);
			propertyDescriptorCache.put(field, propertyDescriptor);
		}
		return propertyDescriptor.getWriteMethod();
	}

	/**
	 * Retrieves the getter method for a field from the entity class.
	 *  
	 * @param field the field
	 * @return the getter method for a given field
	 * @throws IntrospectionException if a proper getter method is not found
	 */
	public Method getReadMethod(final Field field) throws IntrospectionException {
		PropertyDescriptor propertyDescriptor = propertyDescriptorCache.get(field);
		if (propertyDescriptor == null) {
			propertyDescriptor = new PropertyDescriptor(field.getName(), entityClass);
			propertyDescriptorCache.put(field, propertyDescriptor);
		}
		return propertyDescriptor.getReadMethod();
	}
	
	/**
	 * Returns the edge related information.
	 *
	 * @param field the field related via edge
	 * @return {@link EdgeInformation} of the particular field
	 */
	@Nullable
	public EdgeInformation getEdgeInformation(final Field field) {
		return edgeRelationShipCache.get(field);
	}

	/**
	 * Returns the property names of the class for the given edge.
	 *
	 * @param edgeDirection the direction of the edge
	 * @param edgeName the edge for which to return the corresponding property names
	 * @return list of property names which represent the given edge relationship
	 */
	public List<String> getFieldNamesFromEdgeName(final String edgeDirection, final String edgeName) {
		final ODirection oDirection;
		if (edgeDirection.equalsIgnoreCase("in")) {
			oDirection = ODirection.IN;
		} else if (edgeDirection.equalsIgnoreCase("out")) {
			oDirection = ODirection.OUT;
		} else {
			throw new IllegalArgumentException(String.format("Expected edge direction to be either in or out but was %s.", edgeDirection));
		}
		return edgeRelationShipCache.entrySet()
				.stream()
				.filter(entry -> entry.getValue().getName().equals(edgeName))
				.filter(entry -> entry.getValue().getDirection().equals(oDirection))
				.map(entry -> entry.getKey().getName())
				.collect(Collectors.toList());
	}
	
	/**
	 * Gets the constructor parameter types.
	 * @return the constructor parameter types
	 */
	public Class<?>[] getConstructorParameterTypes() {
		return parameterTypes.clone();
	}

	@Override
	public String toString() {
		return "ClassDefinition for " + getEntityName() + "class";
	}
	
	/**
	 * Sets the property relationship field to map
	 *
	 * @param field the field related via edge
	 */
	public void setPropertyField(final Field field) {
		if (field.getAnnotation(Property.class) == null) {
			return;
		}
		final Property property = assertNotNull(field.getAnnotation(Property.class));
		javaFieldToDBFieldMap.put(field.getName(), property.value());
		dbFieldToJavaFieldMap.put(property.value(), field.getName());

	}

	/**
	 * Validate if the Data base property is in Entity or not
	 *
	 * @param schemaProperty schema property
	 * @return Returns {@code true} if the objectFields or dbFieldToJavaFieldMap contains schema Property
	 */
	public boolean checkIfDBPropertyIsInEntity(final String schemaProperty) {
		return objectFields.containsKey(schemaProperty) || dbFieldToJavaFieldMap.containsKey(schemaProperty);
	}

	/**
	 * Get the data base field name from respective java field name.
	 *
	 * @param originalFieldName Field name of the Entity
	 * @return an {@link Optional} java field from data base field.
	 */
	public Optional<String> getDbFieldNameFromJavaFieldName(final String originalFieldName) {
		return Optional.ofNullable(javaFieldToDBFieldMap.get(originalFieldName));
	}

	/**
	 * Get the java field name from respective data base field name.
	 *
	 * @param originalFieldName Field name of the Entity
	 * @return an {@link Optional} data base field from java field.
	 */
	public Optional<String> getJavaFieldNameFromDBFieldName(final String originalFieldName) {
		return Optional.ofNullable(dbFieldToJavaFieldMap.get(originalFieldName));
	}
	
	private void setEdgeRelationshipInfo(final Field field) {
		final Relationship edge = assertNotNull(field.getAnnotation(Relationship.class));
		final EdgeInformation edgeInfo = new EdgeInformation(edge.name(), edge.direction(), edge.sequence());
		edgeRelationShipCache.put(field, edgeInfo);
	}
}
