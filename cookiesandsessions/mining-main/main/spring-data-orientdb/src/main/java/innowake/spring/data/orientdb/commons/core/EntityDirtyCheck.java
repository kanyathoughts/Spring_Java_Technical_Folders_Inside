/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.commons.core;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import com.orientechnologies.orient.core.db.record.OIdentifiable;
import com.orientechnologies.orient.core.db.record.ridbag.ORidBag;
import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.impl.ODocumentEmbedded;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;
import innowake.spring.data.orientdb.commons.exception.FieldNotFoundException;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinitionMapper;
import innowake.spring.data.orientdb.ogm.mapping.util.OElementAdapter;
import innowake.spring.data.orientdb.ogm.proxy.ICollectionProxy;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Checks if any of the field is modified or updated during save operation.
 * It checks if the proxy entity object's values and orient database values are same,
 * if not it is marked to be a dirty object and the data is updated in the orient database.
 */
 class EntityDirtyCheck {

	private static final Logger LOGGER = LoggerFactory.getLogger(EntityDirtyCheck.class);

	private static final String IS_DIRTY_MESSAGE = "Field for %s class's property %s is dirty.";

	private static final String UNDERSCORE = "_";

	/**
	 * Set to store ORIDs of all the entities (vertex, links and edges) which are in storedObjects
	 * yet not committed and ORIDs of those entities which are processed for isDirty check.
	 */
	private final Set<ORID> traversedEntities;
	
	/**
	 * Overloaded Constructor for the EntityIsDirtyCheck class.
	 *
	 * @param traversedEntitiesSet the set of traversedEntities i.e. those which are already processed.
	 */
	 EntityDirtyCheck(final Set<ORID> traversedEntitiesSet) {
		traversedEntities = traversedEntitiesSet; 
	}
	
	/**
	 * If the value of a field in proxy object does not match the value saved in database, then it is marked as dirty.
	 *
	 * @param proxifiedObject proxy object mapped to a vertex node
	 * @return Returns true if the vertex data does not match entity data
	 */
	 boolean isDirty(final IEntityProxy proxifiedObject) {
		final OElement oElement = proxifiedObject.__getElement();
		return isDirty(proxifiedObject, oElement);
	}
	
	
	/*	This is not a javaDoc but for understanding the flow for developer purpose.
	  
	  								isDirty
						___________|_____________________________________
					   /					 							\
					checkIfEdgeIsDirty							checkIfVertexIsDirty
		___________________|_____________					___________________|_____________
		/				|				\					/								\
	Linked Fields  Embedded fields	Other Fields		edge fields						vertex fields
									(primitives,		(list, set, map		____________________|_______________________________________
									linklists, enum)	single edge) 	   /     |			|		|		|		|					\		
																		Enum	Primitive	List	Set		Map		Unsaved Vertex	 Another Vertex (recursive)
	 */
	private boolean isDirty(final IEntityProxy proxifiedObject, final OElement oElement) {
		final ORID proxifiedObjectRid = proxifiedObject.__getRid();
		if (traversedEntities.contains(proxifiedObjectRid)) {
			return false;
		}
		traversedEntities.add(proxifiedObjectRid);
		
		if (oElement.isEdge()) {
			return checkIfEdgeIsDirty(proxifiedObject, oElement);	
		} else {
			return checkIfVertexIsDirty(proxifiedObject, oElement);
		}
	}

	private boolean checkIfEdgeIsDirty(final IEntityProxy proxifiedObject, final OElement oElement) {
		final Class<?> proxifiedObjectClass = proxifiedObject.__getBaseClass();
		/* The @Relationship can be defined on both edges or vertices yet the @RelationshipProperties annotation is absent
		 * for the vertices. So additional check for RelationshipProperties annotation to ensure the entity is really an Edge.
		 * Comparison of proxifiedObject of Vertex and oElement of related edge is not possible here. */
		if ( ! proxifiedObjectClass.isAnnotationPresent(RelationshipProperties.class)) {
			throw new IllegalStateException("The Vertices with Edge Relationship Annotation are not handled.");
		}
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxifiedObject);
		
		/* Check if the links are dirty. We are relying solely on the isDirty method here on the linked entity to check if edge is dirty.
		 * If link is modified or set explicitly, corresponding proxyFieldValue is non-empty and we continue the isDirty check for that link,
		 * else the corresponding proxyFieldValue is empty and we skip the isDirty check for that link. */
		final Map<String, Field> linkFields = classDefinition.getLinks();
		final BiPredicate<Object, OElement> linkIsDirtyFunction = 
				(proxyFieldValue, vertexOElementFieldValue) ->
					(proxyFieldValue != null &&
						( ! (proxyFieldValue instanceof IEntityProxy)
							|| isDirty((IEntityProxy) proxyFieldValue, vertexOElementFieldValue)));
		if (checkIsDirtyForFieldsOfEdge(linkFields, proxifiedObject, oElement, linkIsDirtyFunction, classDefinition)) {
			return true;
		}
		
		/* Check if the embedded fields are dirty. */
		final Map<String, Field> embeddedFields = classDefinition.getEmbeddedFields();
		final BiPredicate<Object, OElement> embeddedFieldIsDirtyFunction = 
				(proxyFieldValue, vertexOElementFieldValue) -> {
					final boolean isProxyFieldValueNull = proxyFieldValue == null;
					final boolean isVertexOElementFieldValueNull = vertexOElementFieldValue == null;
					/* If either of the proxyFieldValue or vertexFieldValue is null and other one is not null, return true.
					 * If both, the proxyFieldValue or vertexFieldValue are not null, check for isDirty. */
					return ((isProxyFieldValueNull ^ isVertexOElementFieldValueNull) ||
							 ( ! isProxyFieldValueNull
								&& ! isVertexOElementFieldValueNull
								&& ( ! (proxyFieldValue instanceof IEntityProxy)
										|| isDirty((IEntityProxy) proxyFieldValue, assertNotNull(vertexOElementFieldValue))))
							);
				};
		if (checkIsDirtyForFieldsOfEdge(embeddedFields, proxifiedObject, oElement, embeddedFieldIsDirtyFunction, classDefinition)) {
			return true;
		}
		
		/* Check if the other fields are dirty. */
		final Map<String, Field> primitiveFields = classDefinition.getPrimitiveFields();
		final Map<String, Field> linkListFields = classDefinition.getLinkLists();
		final Map<String, Field> enumCollectionFields = classDefinition.getEnumCollectionFields();
		return Stream.of(primitiveFields, linkListFields, enumCollectionFields)
				.flatMap(map -> map.entrySet().stream())
				.anyMatch(entry -> {
					final String fieldName = entry.getValue().getName();
					final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
					if (compareProxyObjectWithElement(proxifiedObject, fieldName, oElementAdapter.getProperty(oElement, fieldName), false)) {
						LOGGER.debug(() -> String.format(IS_DIRTY_MESSAGE, proxifiedObject.__getBaseClass(), fieldName));
						return true;
					}
					return false;
				});
	}
	
	private boolean checkIsDirtyForFieldsOfEdge(final Map<String, Field> fields, final IEntityProxy proxifiedObject,
			final OElement oElement, final BiPredicate<Object, OElement> isDirtyFunction, final ClassDefinition classDefinition) {
		for (final Entry<String, Field> entry : fields.entrySet()) {
			final String fieldName = entry.getKey();
			@Nullable
			final Object proxyFieldValue;
			try {
				/*
				 * We are using FieldUtils here to read the property value instead if using proxifiedObject.__getFieldValue because the latter is refreshing the
				 * proxifiedObject with values from the database thereby losing the changes.
				 */
				proxyFieldValue = FieldUtils.readField(proxifiedObject, fieldName, true);
			} catch (final IllegalAccessException e) {
				throw new FieldNotFoundException("Error while reading a field with the propertyName : " + fieldName, e);
			}
			final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
			@Nullable
			final OElement vertexOElementFieldValue =
					extractOElementFromVertexFieldValue(proxifiedObject, fieldName, oElementAdapter.getProperty(oElement, fieldName));
			if (isDirtyFunction.test(proxyFieldValue, vertexOElementFieldValue)) {
				LOGGER.debug(() -> String.format(IS_DIRTY_MESSAGE, proxifiedObject.__getBaseClass(), fieldName));
				return true;
			}
		}
		return false;
	}

	private boolean checkIfVertexIsDirty(final IEntityProxy proxifiedObject, final OElement oElement) {
		final OClass schemaClass = oElement.getSchemaType().orElseThrow(MetadataException::new);
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxifiedObject);
		final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
		return schemaClass.propertiesMap().keySet().stream()
				.filter(schemaProperty -> isSchemaPropertyEdge(schemaProperty) || classDefinition.checkIfDBPropertyIsInEntity(schemaProperty))
				.anyMatch(property -> {
					final Optional<String> javaFiledName = classDefinition.getJavaFieldNameFromDBFieldName(property);
					if (isSchemaPropertyEdge(property)) {
						if (compareProxyObjectWithElementForEdge(proxifiedObject, oElement, property)) {
							LOGGER.debug(() -> String.format(IS_DIRTY_MESSAGE, proxifiedObject.__getBaseClass(), property));
							return true;
						}
					} /* compare proxy object with element for vertex. */
					else if (compareProxyObjectWithElement(proxifiedObject, javaFiledName.isPresent() ? javaFiledName.get() : property,
							oElementAdapter.getProperty(oElement, property), false)) {
						LOGGER.debug(() -> String.format(IS_DIRTY_MESSAGE, proxifiedObject.__getBaseClass(), property));
						return true;
					}
					return false;
				});
	}

	private boolean compareProxyObjectWithElementForEdge(final IEntityProxy proxifiedObject, final OElement oElement, final String propertyName) {
		/* get field names in entity class mapped to edge names. */
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxifiedObject);
		final String edgeDirection = StringUtils.substringBefore(propertyName, UNDERSCORE);
		final String edgeName = StringUtils.substringAfter(propertyName, UNDERSCORE);
		final List<String> fieldNames = classDefinition.getFieldNamesFromEdgeName(edgeDirection, edgeName);
		final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
		@Nullable
		final Object vertexFieldValue = oElementAdapter.getProperty(oElement, propertyName);
		for (String fieldName : fieldNames) {
			if (compareProxyObjectWithElement(proxifiedObject, fieldName, vertexFieldValue, true)) {
				LOGGER.debug(() -> String.format(IS_DIRTY_MESSAGE, proxifiedObject.__getBaseClass(), fieldName));
				return true;
			}
		}
		return false;
	}

	private boolean compareProxyObjectWithElement(final IEntityProxy proxifiedObject, final String propertyName,
				@Nullable final Object vertexFieldValue, final boolean isEdge) {
			final Object proxyObjectFieldValue;
			try {
				proxyObjectFieldValue = FieldUtils.readField(proxifiedObject, propertyName, true);
			} catch (final IllegalAccessException e) {
				throw new FieldNotFoundException("Error while reading a field with the propertyName : " + propertyName, e);
			}
			
			/* If the proxy field value is null or for non-edge fields the two objects are equal, we return false for current comparison.
			 * It acts as a base check for any class capable of checking for equality for example java.util.Date, List, Set, Map etc.*/
			if (proxyObjectFieldValue == null || ( ! isEdge && proxyObjectFieldValue.equals(vertexFieldValue))) {
				return false;
			}
			if (proxyObjectFieldValue.getClass().isEnum()) {
				return ! ((OElement) assertNotNull(vertexFieldValue)).getProperty("name").equals(((Enum<?>) proxyObjectFieldValue).name());
			}
			if (isPrimitiveOrWrapper(proxifiedObject, propertyName) || isFieldOfSpecifiedType(proxifiedObject, propertyName, String.class)) {
				return ! proxyObjectFieldValue.equals(vertexFieldValue);
			}
			if (isArray(proxifiedObject, propertyName)) {
				return ! Objects.deepEquals(proxyObjectFieldValue, vertexFieldValue);
			}
			if (isFieldOfSpecifiedType(proxifiedObject, propertyName, List.class)) {
				return compareProxyObjectWithElementForList(proxifiedObject, propertyName, proxyObjectFieldValue, vertexFieldValue);
			}
			if (isFieldOfSpecifiedType(proxifiedObject, propertyName, Set.class)) {
				return compareProxyObjectWithElementForSet(proxifiedObject, propertyName, proxyObjectFieldValue, vertexFieldValue);
			}
			if (isFieldOfSpecifiedType(proxifiedObject, propertyName, Map.class)) {
				return compareProxyObjectWithElementForMap(proxifiedObject, propertyName, proxyObjectFieldValue, vertexFieldValue);
			}
			if ( ! (proxyObjectFieldValue instanceof IEntityProxy)) {
				return true;
			}
			final OElement vertexField = extractOElementFromVertexFieldValue(proxifiedObject, propertyName, vertexFieldValue);
			if (vertexField == null) {
				return true;
			}
			return isDirty((IEntityProxy) proxyObjectFieldValue, vertexField);
	}

	@SuppressWarnings("unchecked")
	private boolean compareProxyObjectWithElementForList(final IEntityProxy proxifiedObject, final String propertyName,
			final Object proxyObjectFieldValue, @Nullable final Object vertexFieldValue) {
		/* If list is not yet loaded, then it mustn't have been modified and can't be dirty. */
		if (isCollectionNotLazyLoaded(proxyObjectFieldValue)) {
			return false;
		}
		final List<?> proxifiedList = (List<?>) proxyObjectFieldValue;
		if (Objects.isNull(vertexFieldValue)) {
			return ! proxifiedList.isEmpty();
		}
		final List<Object> oList = getOrientCollectionFromVertexFieldValue(proxifiedObject, propertyName, proxyObjectFieldValue, vertexFieldValue, List.class);
		
		if (proxifiedList.size() != oList.size()) {
			return true;
		}
		/* We are ignoring the order of list elements if elements are entities, vertices, edges.
		 * That means even if the order of elements in list change, but elements are same, we compare each element with the database
		 * and if no element property is modified, we return false. If duplicate item is present for any entity, this will throw IllegalStateException. */
		if (proxifiedList.get(0) instanceof IEntityProxy && ! (oList.get(0) instanceof ODocumentEmbedded)) {
			final Map<ORID, IEntityProxy> proxifiedMap = ((List<IEntityProxy>) proxifiedList).stream()
					.collect(Collectors.collectingAndThen(Collectors.toMap(IEntityProxy::__getRid, Function.identity()), TreeMap::new));
			
			final Map<ORID, OElement> oMap = (oList).stream()
					.map(object -> extractOElementFromVertexFieldValue(proxifiedObject, propertyName, object))
					.collect(Collectors.collectingAndThen(Collectors.toMap(OElement::getIdentity, Function.identity()), TreeMap::new));
			
			return compareLists(new ArrayList<>(proxifiedMap.values()), new ArrayList<>(oMap.values()));
		}
		return compareLists(proxifiedList, oList);
	}
	
	@SuppressWarnings("unchecked")
	private boolean compareProxyObjectWithElementForSet(final IEntityProxy proxifiedObject, final String propertyName,
			final Object proxyObjectFieldValue, @Nullable final Object vertexFieldValue) {
		final Set<?> proxifiedSet = (Set<?>) proxyObjectFieldValue;
		final Set<Object> oSet = getOrientCollectionFromVertexFieldValue(proxifiedObject, propertyName, proxyObjectFieldValue, vertexFieldValue, Set.class);
		
		if (proxifiedSet.size() != oSet.size()) {
			return true;
		}
		/* We only support Set<String> in Spring-data. */
		final List<?> proxifiedList = new ArrayList<>(proxifiedSet).stream().sorted().collect(Collectors.toList());
		final List<?> oList = new ArrayList<>(oSet).stream().sorted().collect(Collectors.toList());			
		return compareLists(new ArrayList<>(proxifiedList), new ArrayList<>(oList));
	}
	
	private boolean compareProxyObjectWithElementForMap(final IEntityProxy proxifiedObject, final String propertyName,
			final Object proxyObjectFieldValue, @Nullable final Object vertexFieldValue) {
		final Map<?, ?> proxifiedMap = (Map<?, ?>) proxyObjectFieldValue;		
		final Map<?, ?> oMap = assertNotNull((Map<?, ?>) vertexFieldValue);
		if (proxifiedMap.size() != oMap.size()) {
			return true;
		}
		/* The keys of map must implement java.lang.Comparable interface and must be mutually comparable. */ 
		final Object setElement = proxifiedMap.keySet().iterator().next();
		if ( ! (setElement instanceof Comparable)) {
			final String errorMessage = String.format(
					"The keys of map must implement Comparable interface but %s class's property %s is having uncomparable keys",
					proxifiedObject.__getBaseClass(), propertyName);
			LOGGER.error(() -> errorMessage);
			throw new IllegalArgumentException(errorMessage);
		}
		/* We assume that Map keys are homogenous type otherwise we might get inconsistent results. */
		final Map<?, ?> proxifiedSortedMap = new TreeMap<>(proxifiedMap);
		final Map<?, ?> oSortedMap = new TreeMap<>(oMap);
		/* Check if keys are same in both the maps.*/
		final List<?> proxifiedSortedMapKeyList = new ArrayList<>(proxifiedSortedMap.keySet());
		final List<?> oSortedMapKeyList = new ArrayList<>(oSortedMap.keySet());
		if (compareLists(proxifiedSortedMapKeyList, oSortedMapKeyList)) {
			return true;
		}
		/* Check if values are same in both the maps for sorted keys.*/
		final List<?> proxifiedSortedMapValuesList = new ArrayList<>(proxifiedSortedMap.values());
		final List<?> oSortedMapValuesList = new ArrayList<>(oSortedMap.values());
		return compareLists(proxifiedSortedMapValuesList, oSortedMapValuesList);
	}

	@SuppressWarnings("unchecked")
	private <T extends Collection<Object>> T getOrientCollectionFromVertexFieldValue(final IEntityProxy proxifiedObject, final String propertyName,
			final Object proxyObjectFieldValue, @Nullable final Object vertexFieldValue, final Class<T> tClass) {
		if (vertexFieldValue instanceof ORidBag) {
			final T oCollection;
			if (Set.class.isInstance(proxyObjectFieldValue)) {
				oCollection = (T) new HashSet<Object>();
			} else if (List.class.isInstance(proxyObjectFieldValue)) {
				oCollection = (T) new ArrayList<Object>();
			} else {
				final String errorMessage = String.format("Proxy object field for %s class's property %s is unidentified type %s.",
						proxifiedObject.__getBaseClass(), propertyName, proxyObjectFieldValue.getClass());
				LOGGER.error(() -> errorMessage);
				throw new IllegalStateException(errorMessage);
			}
			final Iterator<OIdentifiable> ridBagIterator = ((ORidBag) vertexFieldValue).rawIterator();
			while (ridBagIterator.hasNext()) {
				final OIdentifiable oIdentifiable = ridBagIterator.next();
				oCollection.add(extractOElementFromVertexFieldValue(proxifiedObject, propertyName, oIdentifiable));
			}
			return oCollection;
		} else if (tClass.isInstance(vertexFieldValue)) {
			return (T) assertNotNull(vertexFieldValue);
		} else {
			final String errorMessage = String.format("Vertex field for %s class's property %s is unidentified type %s.",
					proxifiedObject.__getBaseClass(),
					propertyName,
					vertexFieldValue == null ? vertexFieldValue : vertexFieldValue.getClass());
			LOGGER.error(() -> errorMessage);
			throw new IllegalStateException(errorMessage);
		}
	}
	
	private boolean compareLists(final List<?> proxifiedList, final List<?> oList) {
		/* Compare the list elements preserving order in case the elements are primitives or String etc. */ 
		final BiPredicate<Object, Object> proxyFieldComparatorFunction = getProxyFieldComparatorFunction(proxifiedList.get(0));
		for (int i = 0; i < oList.size(); i++) {
			final Object proxifiedListFieldValue = proxifiedList.get(i);
			final Object vertexListFieldValue = assertNotNull(oList.get(i));
			if (proxyFieldComparatorFunction.test(proxifiedListFieldValue, vertexListFieldValue)) {
				return true;
			}
		}
		return false;
	}
	
	private BiPredicate<Object, Object> getProxyFieldComparatorFunction(final Object proxyObjectFieldValue) {
		/* This provides a comparator function for comparison between proxified list element and orientDb list element. */
		final Class<?> proxyObjectFieldClass = proxyObjectFieldValue.getClass();
		if (proxyObjectFieldClass.isEnum()) {
			return (proxifiedListFieldValue, vertexListFieldValue) -> ! ((OElement) assertNotNull(vertexListFieldValue)).getProperty("name")
					.equals(((Enum<?>) proxifiedListFieldValue).name());
		}
		if (ClassUtils.isPrimitiveOrWrapper(proxyObjectFieldClass) || proxyObjectFieldClass.isAssignableFrom(String.class)) {
			return (proxifiedListFieldValue, vertexListFieldValue) -> ! proxifiedListFieldValue.equals(vertexListFieldValue);
		}
		if ( ! (proxyObjectFieldValue instanceof IEntityProxy)) {
			return (proxifiedListFieldValue, vertexListFieldValue) -> true;
		}
		return (proxifiedListFieldValue, vertexListFieldValue) -> (vertexListFieldValue == null ||
						isDirty((IEntityProxy) proxifiedListFieldValue, (OElement) vertexListFieldValue));
	}

	@Nullable
	private static OElement extractOElementFromVertexFieldValue(final IEntityProxy proxifiedObject, final String fieldName, @Nullable final Object vertexFieldValue) {
		if (vertexFieldValue == null) {
			return null;
		}
		final OElement vertexField;
		if (vertexFieldValue instanceof OElement) {
			vertexField = (OElement) vertexFieldValue;
		} else if (vertexFieldValue instanceof ORecordId) {
			vertexField = ((ORecordId) vertexFieldValue).getRecord();
		} else if (vertexFieldValue instanceof List && ! ((List<?>) vertexFieldValue).isEmpty()) {
			vertexField = extractOElementFromVertexFieldValue(proxifiedObject, fieldName, ((List<?>) vertexFieldValue).get(0));
		} else {
			final String errorMessage = String.format("Cannot extract OElement as database value of %s property %s is of type %s",
					proxifiedObject.__getBaseClass(), fieldName, vertexFieldValue.getClass());
			throw new IllegalStateException(errorMessage);
		}
		return vertexField;
	}

	private static boolean isPrimitiveOrWrapper(final IEntityProxy proxifiedObject, final String propertyName) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxifiedObject);
		final Optional<Field> field = classDefinition.getFieldByName(propertyName);
		if (field.isPresent()) {
			return ClassUtils.isPrimitiveOrWrapper(field.get().getType());
		}
		return false;
	}

	private static boolean isArray(final IEntityProxy proxifiedObject, final String propertyName) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxifiedObject);
		final Optional<Field> field = classDefinition.getFieldByName(propertyName);
		if (field.isPresent()) {
			return field.get().getType().isArray();
		}
		return false;
	}

	private static boolean isFieldOfSpecifiedType(final IEntityProxy proxifiedObject, final String propertyName, final Class<?> className) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxifiedObject);
		final Optional<Field> field = classDefinition.getFieldByName(propertyName);
		if (field.isPresent()) {
			return field.get().getType().isAssignableFrom(className);
		}
		return false;
	}

	private static boolean isSchemaPropertyEdge(final String property) {
		return property.startsWith("in_") || property.startsWith("out_");
	}
	
	private static boolean isCollectionNotLazyLoaded(final Object proxyObjectFieldValue) {
		return proxyObjectFieldValue instanceof ICollectionProxy && ! ((ICollectionProxy) proxyObjectFieldValue).__isLazyLoaded();
	}
}
