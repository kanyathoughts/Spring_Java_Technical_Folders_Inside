/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.commons.core;

import static com.github.raymanrt.orientqb.query.Clause.clause;
import static com.github.raymanrt.orientqb.util.Joiner.andJoiner;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.orientechnologies.orient.core.db.record.OIdentifiable;
import com.orientechnologies.orient.core.sql.executor.OResult;
import com.orientechnologies.orient.core.sql.executor.OResultInternal;
import innowake.spring.data.orientdb.repository.cache.OrientRequestCacheManager;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.web.context.request.RequestContextHolder;

import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.Operator;
import com.github.raymanrt.orientqb.query.Query;
import com.github.raymanrt.orientqb.query.clause.CompositeClause;
import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal;
import com.orientechnologies.orient.core.db.ODatabaseSession;
import com.orientechnologies.orient.core.db.document.ODatabaseDocument;
import com.orientechnologies.orient.core.db.record.ORecordLazyList;
import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.ORecord;
import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinitionMapper;
import innowake.spring.data.orientdb.ogm.mapping.EdgeInformation;
import innowake.spring.data.orientdb.ogm.mapping.EntityClassMapper;
import innowake.spring.data.orientdb.ogm.mapping.ObjectMapper;
import innowake.spring.data.orientdb.ogm.mapping.ObjectStruct;
import innowake.spring.data.orientdb.ogm.mapping.Primitives;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomProperty;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomPropertyService;
import innowake.spring.data.orientdb.ogm.mapping.util.GraphUtils;
import innowake.spring.data.orientdb.ogm.mapping.util.OElementAdapter;
import innowake.spring.data.orientdb.ogm.proxy.EntityProxyFactory;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * A class to handle basic CRUD operations on Orient Database.
 * 
 * @param <T> type of entity 
 */
public abstract class OrientOperationsImpl<T> implements OrientOperations<T> {

	private static final String UNSUPPORTED_QUERY_TYPE_EXCEPTION = "Unsupported value assigned for type "; 

	/**
	 * Singleton instance of {@link ObjectMapper}.
	 */
	protected final ObjectMapper mapper = ObjectMapper.getObjectMapper();

	/**
	 * Singleton instance of {@link ClassDefinitionMapper}.
	 */
	protected final ClassDefinitionMapper classDefinitionMapper = ClassDefinitionMapper.getClassDefinitionMapper();
	
	private static final Logger LOGGER = LoggerFactory.getLogger(OrientOperationsImpl.class);

	private final SessionManager sessionManager;
	private final CustomPropertyService customPropertyService;

	@Autowired
	@Nullable
	private OrientOperations<T> self;

	/**
	 * Parameterized constructor to initialize {@link SessionManager} instances.
	 * 
	 * @param sessionManager instance used to create a orientDB session
	 * @param customPropertyService instance of the {@linkplain CustomPropertyService}
	 */
	protected OrientOperationsImpl(final SessionManager sessionManager, final CustomPropertyService customPropertyService) {
		this.sessionManager = sessionManager;
		this.customPropertyService = customPropertyService;
	}

	/**
	 * Returns a reference to this object, or a reference to the Spring AOP Proxy wrapping this object.
	 * <p>When passing a reference of this instance to another object, then this {@code getSelf()} method should be used
	 * instead of passing {@code this}.</p>
	 *
	 * @return this instance wrapped in an AOP Proxy
	 */
	private OrientOperations<T> getSelf() {
		/* self is null in unit tests where this class is instantiated via the constructor
		 * and without an AOP proxy */
		return self != null ? self : this;
	}

	@Override
	public IEntityProxy save(final T entity) {
		/* Contains the real and it's corresponding proxy object that are stored, as long as commit is not made */
		final ConcurrentMap<Object, IEntityProxy> storedObjects = new ConcurrentHashMap<>();
		final IEntityProxy entityProxy = save(entity, storedObjects);
		saveOrUpdateCustomProperties(entityProxy);
		return entityProxy;
	}

	@Override
	public IEntityProxy findById(final Class<?> domainClass, final String rid) {
		final ORID oRID = new ORecordId(rid);
		return findByIdInternal(domainClass, oRID);
	}

	@Override
	public boolean existsById(final String rid) {
		return existsById(new ORecordId(rid));
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterable<T> findAll(final Class<T> clazz) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(clazz);
		try (final OResultSet result = getDbSession().query("select * from ? ", classDefinition.getEntityName())) {
			final EntityClassMapper classMapper = new EntityClassMapper(clazz);
			return result.stream().map(record2 -> (T) mapDataToEntity(record2, classMapper))
					.collect(Collectors.toList());
		}
	}

	@Override
	public long count(final Class<T> clazz) {
		final ODatabaseDocument session = getDbSession();
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(clazz);
		return session.countClass(classDefinition.getEntityName(), true);
	}

	@Override
	public void deleteById(final String id) {
		deleteById(new ORecordId(id));
	}

	@Override
	public void delete(final T entity) {
		if ( ! (entity instanceof IEntityProxy)) {
			throw new IllegalArgumentException("Entity should be an instance of IEntityProxy");
		}
		final ORID oRID = ((IEntityProxy) entity).__getRid();
		deleteById(Assert.assertNotNull(oRID));
	}

	@Override
	public void deleteAll(final Iterable<? extends T> entities) {
		entities.forEach(this::delete);
	}

	@Override
	public void deleteAll(final Class<T> clazz) {
		try (final OResultSet result = getDbSession().command("select @rid from ? ", clazz.getSimpleName())) {
			result.stream().map(record2 -> (ORID) record2.getProperty("@rid")).forEach(this::deleteById);
		}
	}

	@Override
	public void deleteById(final ORID ridToDelete) {
		final ORecord record2 = getDbSession().load(ridToDelete, null, true);
		if (record2 instanceof OVertex) {
			final OVertex vertex = (OVertex) record2;
			final List<OVertex> outVertices = StreamSupport.stream(vertex.getVertices(ODirection.OUT).spliterator(), false).collect(Collectors.toList());
			vertex.getEdges(ODirection.BOTH).forEach(OEdge::delete);
			outVertices.stream().filter(outVertex -> IteratorUtils.toList(outVertex.getEdges(ODirection.BOTH).iterator()).isEmpty()).forEach(OVertex::delete);
			vertex.delete();
		} else if (record2 instanceof OElement) {
			record2.delete();
		}
	}

	@Override
	public boolean existsById(final ORID rid) {
		return getDbSession().load(rid, null, true) != null;
	}

	@Override
	public IEntityProxy findById(final Class<?> domainClass, final ORID oRID) {
		return findByIdInternal(domainClass, oRID);
	}

	@Override
	public IEntityProxy findByIdInternal(final Class<?> domainClass, final ORID oRID) {
		@Nullable
		final ORecord record2 = getDbSession().load(oRID);
		if (record2 == null) {
			throw new NoRecordFoundException(String.format("No record with id : %s was found", oRID.toString()));
		}
		final EntityClassMapper classMapper = new EntityClassMapper(domainClass);
		return mapDataToEntity(new OResultInternal(record2), classMapper);
	}

	private void mapObjectStructToElement(final IEntityProxy proxy, final OElement oElement, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		if (oElement.isVertex()) {
			final OVertex vertexNode = (OVertex) oElement;
			populateAllVerticesToManageOrphanVertices(vertexNode, oStruct);
			mapEntityRelationshipToEdge(proxy, vertexNode, oStruct, storedObjects);
			mapCollectionRelationshipToEdges(proxy, vertexNode, oStruct, storedObjects);
		}
		mapEntityRelationshipToLink(proxy, oElement, oStruct, storedObjects);
		mapCollectionRelationshipToLinkLists(proxy, oElement, oStruct, storedObjects);
		mapPrimitiveFields(proxy, oElement, oStruct);
		mapEnumFields(proxy, oElement, oStruct);
		mapEmbeddedRelationshipToElement(proxy, oElement, oStruct.getEmbeddedFields(), storedObjects);
		populateIds(proxy, oElement);
	}

	private void mapPrimitiveFields(final IEntityProxy proxy, final OElement oElement, final ObjectStruct oStruct) {
		final ClassDefinition classDef = classDefinitionMapper.getClassDefinition(proxy);
		oStruct.getFields().forEach((fieldName, fieldValue) -> {
			OElementAdapter.getOElementAdapter(classDef).setProperty(oElement, fieldName, fieldValue);
			final Optional<Field> fieldByName = classDef.getFieldByName(fieldName);
			proxy.__setFieldValue(fieldValue, fieldByName.orElseThrow(() -> new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldByName)));
		});
	}

	private void populateIds(final IEntityProxy proxy, final OElement element) {
		final List<Field> fields = classDefinitionMapper.getClassDefinition(proxy).getGenerateIds();
		for (final Field field : fields) {
			if (field != null && proxy.__getFieldValue(field) == null) {
				final Id idInfo = field.getAnnotation(Id.class);
				final Long generatedId = getSequenceValue(idInfo.sequence());
				OElementAdapter.getOElementAdapter(ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(proxy)).setProperty(element,
						field.getName(), generatedId);
				proxy.__setFieldValue(generatedId, field);
			} else {
				LOGGER.warn(() -> "Id field is not present on " + proxy.__getBaseClass().getSimpleName());
			}
		}
	}

	private void populateAllVerticesToManageOrphanVertices(final OVertex vertexNode, final ObjectStruct oStruct) {
		oStruct.setAllVerticesConnected(IteratorUtils.toList(vertexNode.getVertices(ODirection.OUT).iterator()));
	}

	private void mapEnumFields(final IEntityProxy proxy, final OElement oElement, final ObjectStruct oStruct) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(proxy);
		final List<ORecordId> enumRecords = new ArrayList<>();
		oStruct.getEnumCollectionLinks().forEach((fieldName, fieldValue) -> {
			@SuppressWarnings("unchecked")
			final List<String> enumList = (List<String>) fieldValue;
			final Optional<Field> fieldInProxy = classDefinition.getFieldByName(fieldName);
			if (fieldInProxy.isPresent()) {
				final Class<?> enumClass = mapper.getListType(fieldInProxy.get());
				enumList.stream().forEach(enumValue -> enumRecords.add(fetchEnumRecord(enumClass, enumValue)));
				OElementAdapter.getOElementAdapter(classDefinition).setProperty(oElement, fieldName, enumRecords);
			}
		});
		oStruct.getEnumLinks().forEach((fieldName, fieldValue) -> {
			final Optional<Field> fieldInProxy = classDefinition.getFieldByName(fieldName);
			if (fieldInProxy.isPresent()) {
				final Class<?> enumClass = fieldInProxy.get().getType();
				OElementAdapter.getOElementAdapter(classDefinition).setProperty(oElement, fieldName, fetchEnumRecord(enumClass, fieldValue));
			} else {
				throw new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldInProxy);
			}
		});
	}

	private ORecordId fetchEnumRecord(final Class<?> enumClass, final Object fieldValue) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(enumClass);
		final Query query = new Query();
		query.select("@rid").from(classDefinition.getEntityName()).where(clause("name", Operator.EQ, fieldValue));
		try {
			return (ORecordId) command(query.toString(), enumClass, "@rid");
		} catch (final NoRecordFoundException ex) {
			throw new NoRecordFoundException("Enum link does not exist for " + enumClass.getSimpleName());
		}
	}

	private void mapEntityRelationshipToLink(final IEntityProxy proxy, final OElement vertexNode, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(proxy);
		oStruct.getLinks().forEach((fieldName, fieldValue) -> {
			final Optional<Field> fieldInProxy = classDefinition.getFieldByName(fieldName);
			if (fieldInProxy.isPresent()) {
				final IEntityProxy proxiedInnerObject = saveRelationshipElement(fieldValue, storedObjects);
				OElementAdapter.getOElementAdapter(classDefinition).setProperty(vertexNode, fieldName, proxiedInnerObject.__getRid());
				proxy.__setFieldValue(proxiedInnerObject, fieldInProxy.get());
			} else {
				throw new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldInProxy);
			}
		});
	}

	private void mapEntityRelationshipToEdge(final IEntityProxy proxy, final OVertex vertexNode, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(proxy);
		oStruct.getEdgeLinks().forEach((fieldName, fieldValue) -> {
			final Optional<Field> fieldInProxy = classDefinition.getFieldByName(fieldName);
			if (fieldInProxy.isPresent()) {
				final EdgeInformation edgeInfo = assertNotNull(classDefinition.getEdgeInformation(fieldInProxy.get()));
				final IEntityProxy proxiedInnerObject = mapToEdge(vertexNode, edgeInfo, fieldValue, oStruct, storedObjects, classDefinition);
				proxy.__setFieldValue(proxiedInnerObject, fieldInProxy.get());
			} else {
				throw new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldInProxy);
			}
		});
	}

	private void mapCollectionRelationshipToLinkLists(final IEntityProxy proxy, final OElement oElement, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(proxy);
		oStruct.getLinkLists().forEach((fieldName, collectionMemberOfEntity) -> {
			final Optional<Field> fieldInProxy = classDefinition.getFieldByName(fieldName);
			if (fieldInProxy.isPresent()) {
				if (collectionMemberOfEntity instanceof List) {
					final List<ORID> directLinks = ((List<?>) collectionMemberOfEntity).stream()
							.map(listItem -> saveRelationshipElement(listItem, storedObjects).__getRid()).collect(Collectors.toList());
					OElementAdapter.getOElementAdapter(classDefinition).setProperty(oElement, fieldInProxy.get().getName(), directLinks);
				}
				proxy.__populateLazyCollectionReference(fieldName);
			} else {
				throw new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldInProxy);
			}
		});
	}

	private void mapCollectionRelationshipToEdges(final IEntityProxy proxy, final OVertex vertexNode, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects) {
			final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(proxy);
			oStruct.getEdgeLinkLists().forEach((fieldName, collectionMemberOfEntity) -> {
				final Optional<Field> fieldInProxy = classDefinition.getFieldByName(fieldName);
				if (fieldInProxy.isPresent()) {
					final EdgeInformation edgeInfo = assertNotNull(classDefinition.getEdgeInformation(fieldInProxy.get()));
					if (collectionMemberOfEntity instanceof List) {
						((List<?>) collectionMemberOfEntity)
								.forEach(listItem -> mapToEdge(vertexNode, edgeInfo, listItem, oStruct, storedObjects, classDefinition));
					} else if (collectionMemberOfEntity instanceof Map) {
						mapKeyValueMapToEdge(vertexNode, (Map<?, ?>) collectionMemberOfEntity, edgeInfo, oStruct, storedObjects, classDefinition);
					}
					proxy.__populateLazyCollectionReference(fieldName);
				} else {
					throw new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldInProxy);
				}
			});
	}

	private void mapEmbeddedRelationshipToElement(final IEntityProxy proxy, final OElement oElement, final Map<String, Object> embeddedFields,
			final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(proxy);
		embeddedFields.forEach((fieldName, fieldValue) -> {
			final IEntityProxy proxifiedObject = saveRelationshipElement(fieldValue, storedObjects);
			OElementAdapter.getOElementAdapter(classDefinition).setProperty(oElement, fieldName, proxifiedObject.__getElement());
			final Optional<Field> fieldByName = classDefinition.getFieldByName(fieldName);
			proxy.__setFieldValue(proxifiedObject,
					fieldByName.orElseThrow(() -> new UnsupportedQueryTypeException(UNSUPPORTED_QUERY_TYPE_EXCEPTION + fieldByName)));
		});
	}

	private void mapKeyValueMapToEdge(final OVertex vertexNode, final Map<?, ?> mapMemberOfEntity, final EdgeInformation edgeInfo, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects, final ClassDefinition classDefinition) {
		mapMemberOfEntity.forEach((innerMapKey, innerMapValue) -> {
			final IEntityProxy edgeObject = mapToEdge(vertexNode, edgeInfo, innerMapValue, oStruct, storedObjects, classDefinition);
			if (Primitives.isSupportedDataType(innerMapKey.getClass())) {
				final OElement element = edgeObject.__getElement();
				if (element.isEdge()) {
					OElementAdapter.getOElementAdapter(classDefinition).setProperty(element, "key", innerMapKey);
				} else {
					oStruct.getEdgesToSave().get(oStruct.getEdgesToSave().size() - 1).setProperty("key", innerMapKey);
				}
			} else {
				/* For handling scenario where key is not a primitive object. */
				throw new UnsupportedOperationException("WMIN-708: Entity as key in map is not supported");
			}
		});
	}

	private IEntityProxy mapToEdge(final OVertex vertexNode, final EdgeInformation edgeInfo, final Object listItem, final ObjectStruct oStruct,
			final ConcurrentMap<Object, IEntityProxy> storedObjects, final ClassDefinition classDefinition) {
		final IEntityProxy proxifiedObject = saveRelationshipElement(listItem, storedObjects);
		final OElement element = proxifiedObject.__getElement();
		final OElement newEdge;
		if (element.isVertex()) {
			newEdge = fetchEdge(vertexNode, edgeInfo, (OVertex) element, classDefinition);
		} else {
			newEdge = proxifiedObject.__getElement();
		}
		oStruct.getEdgesToSave().add(newEdge);
		return proxifiedObject;
	}

	private OEdge fetchEdge(final OVertex vertexNode, final EdgeInformation edgeInfo, final OVertex relatedVertexNode, final ClassDefinition classDefinition) {
		final OVertex fromVertex;
		final OVertex toVertex;
		if (edgeInfo.getDirection() == ODirection.OUT) {
			fromVertex = vertexNode;
			toVertex = relatedVertexNode;
		} else if (edgeInfo.getDirection() == ODirection.IN) {
			fromVertex = relatedVertexNode;
			toVertex = vertexNode;
		} else {
			throw new MetadataException("Invalid direction for edge");
		}
		final Optional<OEdge> retrievedEdge = retrieveEdgeIfExists(edgeInfo.getName(), fromVertex, toVertex);
		final OEdge newEdge;
		if (retrievedEdge.isPresent()) {
			newEdge = retrievedEdge.get();
		} else {
			newEdge = getDbSession().newEdge(fromVertex, toVertex, edgeInfo.getName());
		}
		if ( ! StringUtils.isAllBlank(edgeInfo.getSequenceName())) {
			OElementAdapter.getOElementAdapter(classDefinition).setProperty(newEdge, "id", getSequenceValue(edgeInfo.getSequenceName()));
		}
		return newEdge;
	}

	private IEntityProxy saveRelationshipElement(final Object entityObject, final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		@Nullable
		Object tempObject = storedObjects.get(entityObject);
		if (tempObject == null) {
			tempObject = entityObject;
		} else {
			return (IEntityProxy) tempObject;
		}
		final EntityDirtyCheck entityDirtyCheck = new EntityDirtyCheck(extractRidsFromStoredObjects(storedObjects));
		if ( ! (tempObject instanceof IEntityProxy) || entityDirtyCheck.isDirty((IEntityProxy) tempObject)) {
			tempObject = save(tempObject, storedObjects);
		}
		/* Save or update custom properties of the nested entities.*/
		if (tempObject instanceof IEntityProxy) {
			saveOrUpdateCustomProperties((IEntityProxy) tempObject);
		}
		storedObjects.put(entityObject, (IEntityProxy) tempObject);
		return (IEntityProxy) tempObject;
	}

	private Set<ORID> extractRidsFromStoredObjects(final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		return storedObjects.values().stream()
				.map(IEntityProxy::__getRid)
				.collect(Collectors.toSet());
	}

	private <S> IEntityProxy save(final S entity, final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		if (storedObjects.containsKey(entity)) {
			LOGGER.info(() -> "The original object had already been persisted. The initially created instance is returned.");
			return storedObjects.get(entity);
		}
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(entity);
		final String entityName = classDefinition.getEntityName();
		@Nullable
		final OClass schemaClass = getDbSession().getClass(entityName);
		if (schemaClass == null) {
			throw new MetadataException("No valid schema mapping found for the entity type :" + entityName);
		}
		/* Creates a object structure which holds the values from the entity object, mapped according to class definition */
		final ObjectStruct oStruct = mapper.computeObjectStruct(entity);
		final Object object = retrieveEntityByIdorRecordIdIfExists(entity, classDefinition);
		final IEntityProxy proxifiedObject = createOrUpdateElement(object, storedObjects, classDefinition, oStruct, schemaClass);
		mapper.convertPrimitiveCollectionsToEmbedded(proxifiedObject);
		GraphUtils.collectAndDeleteOrphanNodes(oStruct);	
		return proxifiedObject;
	}

	/**
	 * Inserts or updates the custom properties of V2 mining entities into respective custom property schema.
	 * <p>
	 * @param proxifiedObject object to update custom properties
	 */
	@SuppressWarnings("unchecked")
	private void saveOrUpdateCustomProperties(final IEntityProxy proxifiedObject) {
		final Field customPropertyField = classDefinitionMapper.getClassDefinition(proxifiedObject).getCustomPropertyField();
		if (null != customPropertyField) {
			final Object customProperties = proxifiedObject.__getFieldValue(customPropertyField);
			if (customProperties instanceof Map<?, ?>) {
				final ODatabaseDocumentInternal threadDatabase = sessionManager.getThreadDatabase();
				customPropertyService.saveOrUpdate(proxifiedObject.__getElement(), (Map<String, List<CustomProperty>>) customProperties, threadDatabase);
			} else {
				final String errorMessage = String.format("Custom property field '%s' of entity '%s' value should be an instance of Map",
						customPropertyField.getName(), proxifiedObject.getClass());
				LOGGER.error(errorMessage);
				throw new EntityProxyMappingException(errorMessage);
			}
		}
	}

	private <S> Object retrieveEntityByIdorRecordIdIfExists(final S entity, final ClassDefinition classDefinition) {
		if ( ! (entity instanceof IEntityProxy) && entity.getClass().isAnnotationPresent(Entity.class)) {
			final List<Field> idFields = classDefinition.getGenerateIds();
			final Field rIdField = classDefinition.getRidField();
			try {
				if (rIdField != null) {
					final Object rIdValue = FieldUtils.readField(rIdField, entity, true);
					if (rIdValue != null) {
						final OElement element = getDbSession().load(new ORecordId(rIdValue.toString()));
						return mapDataToEntity(new OResultInternal(element), new EntityClassMapper(entity.getClass()));
					}
				}
				if ( ! idFields.isEmpty()) {
					final Field idField = idFields.get(0);
					final Object idValue = FieldUtils.readField(idField, entity, true);
					if (idValue != null) {
						final Query query = new Query();
						query.from(classDefinition.getEntityName()).where(clause(idField.getName(), Operator.EQ, idValue));
						return command(query.toString(), entity.getClass());
					}
				}
				if (rIdField == null && idFields.isEmpty()) {
					LOGGER.warn(() -> "Both Id and @RId field is not present for classDefinition of " +  classDefinition.getEntityName());
				}
			} catch (final IllegalAccessException ex) {
				throw new EntityProxyMappingException("Unable to read Id or @RId field", ex);
			}
		}
		return entity;
	}

	private <S> IEntityProxy createOrUpdateElement(final S entity, final ConcurrentMap<Object, IEntityProxy> storedObjects,
			final ClassDefinition classDefinition, final ObjectStruct oStruct, final OClass schemaClass) {
		final IEntityProxy proxifiedObject;
		final OElement oElement;
		if (entity instanceof IEntityProxy) {
			proxifiedObject = (IEntityProxy) entity;
			oElement = proxifiedObject.__getElement();
			if (oElement.isVertex()) {
				GraphUtils.updateOStructWithEdgesToRemove((OVertex) oElement, classDefinition, oStruct);
			}
		} else if (schemaClass.isEdgeType()) {
			proxifiedObject = retrieveOrCreateEdge(entity, classDefinition, schemaClass, storedObjects);
			oElement = proxifiedObject.__getElement();
			proxifiedObject.__populateObjectFields(entity);
			storedObjects.put(entity, proxifiedObject);
		} else {
			if (schemaClass.isVertexType()) {
				oElement = getDbSession().newVertex(schemaClass.getName());
			}  else {
				/* Embedded fields are created as elements */
				oElement = getDbSession().newElement(schemaClass.getName());
			}
			proxifiedObject = EntityProxyFactory.create(entity, oElement, getSelf(), sessionManager);
			proxifiedObject.__populateObjectFields(entity);
			storedObjects.put(entity, proxifiedObject);
		}
		mapObjectStructToElement(proxifiedObject, oElement, oStruct, storedObjects);
		if (oElement.isVertex()) {
			GraphUtils.saveVertexAndEdges((OVertex) oElement, oStruct);
		} else if (proxifiedObject.__getBaseClass().isAnnotationPresent(Entity.class)) {
			oElement.save();
		}
		return proxifiedObject;
	}

	private <S> IEntityProxy retrieveOrCreateEdge(final S entity, final ClassDefinition classDefinition, final OClass schemaClass, 
			final ConcurrentMap<Object, IEntityProxy> storedObjects) { 
		try {
			final Object fromEntity = FieldUtils.readField(entity, "out", true);
			final Object toEntity = FieldUtils.readField(entity, "in", true);
			return getEdge(entity, classDefinition, fromEntity, toEntity, schemaClass, storedObjects);
		} catch (final IllegalAccessException e) {
			throw new EntityProxyMappingException("Could not retrieve fields", e);
		}
	}

	private <S> IEntityProxy getEdge(final S entity, final ClassDefinition classDefinition, final Object fromEntity, final Object toEntity,
		final OClass schemaClass, final ConcurrentMap<Object, IEntityProxy> storedObjects) {
		final Optional<OVertex> fromEntityVertex = retrieveVertexByRid(fromEntity);
		final Optional<OVertex> toEntityVertex = retrieveVertexByRid(toEntity);
		if (fromEntityVertex.isPresent() && toEntityVertex.isPresent()) {
			final Optional<OEdge> result = retrieveEdgeIfExists(classDefinition.getEntityName(), fromEntityVertex.get(), toEntityVertex.get());
			if (result.isPresent()) {
				return mapDataToEntity(new OResultInternal(result.get()), new EntityClassMapper(entity.getClass()));
			}
			final OElement element = getDbSession().newEdge(fromEntityVertex.get(), toEntityVertex.get(), schemaClass);
			return EntityProxyFactory.create(entity, element, getSelf(), sessionManager);
		}
		final IEntityProxy fromEntityProxy = saveRelationshipElement(fromEntity, storedObjects);
		final IEntityProxy toEntityProxy = saveRelationshipElement(toEntity, storedObjects);
		final OElement element = getDbSession().newEdge((OVertex) fromEntityProxy.__getElement(), (OVertex) toEntityProxy.__getElement(), schemaClass);
		return EntityProxyFactory.create(entity, element, getSelf(), sessionManager);
	}

	private Optional<OEdge> retrieveEdgeIfExists(final String edgeClassName, final OVertex fromEntityVertex, final OVertex toEntityVertex) {
		final Query query = new Query();
		final Clause outClause = clause("out", Operator.EQ, fromEntityVertex.getIdentity().toString());
		final Clause inClasue = clause("in", Operator.EQ, toEntityVertex.getIdentity().toString());
		query.from(edgeClassName).where(new CompositeClause(andJoiner, outClause, inClasue));
		try (final OResultSet resultSet = getDbSession().command(query.toString())) {
			if (resultSet.hasNext()) {
				return resultSet.next().getEdge();
			}
		}
		return Optional.empty();
	}
	
	private <S> Optional<OVertex> retrieveVertexByRid(final S entity) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(entity);
		@Nullable
		final Field ridField = classDefinition.getRidField();
		try {
			if (ridField != null) {
				@Nullable
				final Object ridValue = FieldUtils.readField(ridField, entity, true);
				if (ridValue instanceof String) {
					final ORecord record2 = getDbSession().load(new ORecordId((String) ridValue));
					if (record2 instanceof OVertex) {
						return Optional.of((OVertex) record2);
					}
				}
			}
		} catch (final IllegalAccessException ex) {
			throw new EntityProxyMappingException("Unable to read @Rid field", ex);
		}
		return Optional.empty();
	}

	@Override
	public IEntityProxy mapDataToEntity(final OResult oResult, final EntityClassMapper classMapper) {
		final OElement oElement;
		if (oResult.isElement()) {
			oElement = oResult.toElement();
		} else {
			final Object rid = oResult.getProperty("@rid");
			if (rid instanceof OIdentifiable) {
				oElement = getDbSession().load(((OIdentifiable) rid).getIdentity());
			} else {
				oElement = getDbSession().load(new ORecordId(rid.toString()));
			}
		}

		final EntityClassMapper entityClassMapper = getEntityClassMapper(oElement, classMapper);
		final Class<?> actualClass = entityClassMapper.getEntityClass();
		final ClassDefinition classdef = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(actualClass);
		final Map<Field, Object> fields = new HashMap<>();

		/* Maps the basic primitive attribute fields to proxy */
		fields.putAll(mapPrimitiveFieldsToProxy(oElement, classdef.getPrimitiveFields(), entityClassMapper, classdef));

		/* Process enum fields */
		fields.putAll(mapEnumFieldsToProxy(oElement, classdef.getEnumFields(), classdef));

		/* Process enum collections */
		fields.putAll(mapEnumCollectionsToProxy(oElement, classdef.getEnumCollectionFields(), classdef));

		/* Process relationship link fields i.e., edge to other vertex */
		fields.putAll(mapEdgeRelationFieldsToEntity(oElement, classdef.getEntityEdges(), entityClassMapper));

		/* Process embedded relationship fields */
		fields.putAll(mapEmbeddedRelationFieldsToEntity(oElement, classdef.getEmbeddedFields(), entityClassMapper, classdef));

		/* Create a proxy object to map record details on it */
		final IEntityProxy proxy;

		proxy = EntityProxyFactory.create(actualClass, oElement.getRecord(), getSelf(), sessionManager);
		/* Map record id to proxy field */
		proxy.__injectRid();

		mapCustomProperties(oElement, classdef, proxy);

		/* Set all the field values */
		fields.entrySet().parallelStream().forEach(entry -> proxy.__setFieldValue(entry.getValue(), entry.getKey()));

		/* Process collection of relationship entities */
		mapLinkListsToProxy(oElement, classdef.getLinkLists(), proxy, entityClassMapper);

		/* Process collection of relationship entities edges*/
		mapLinkListsToProxy(oElement, classdef.getCollectionOfEntityEdges(), proxy, entityClassMapper);
		
		return proxy;
	}
	
	/**
	 * Maps the abstract parent class and it's child entity class to {@link EntityClassMapper} if exists, else return same {@code classMapper} object. 
	 * Both parent and child model classes should be in the same package.
	 *
	 * @param oElement instance of {@link OElement}
	 * @param classMapper instance of {@link EntityClassMapper}
	 * @return either an new instance of {@link EntityClassMapper} if there is parent-child domain class, else {@code classMapper}
	 */
	private EntityClassMapper getEntityClassMapper(final OElement oElement, final EntityClassMapper classMapper) {
		final Class<?> domainClass = classMapper.getEntityClass();
		if (Modifier.isAbstract(domainClass.getModifiers())) {
			final Optional<OClass> schemaClass = ((OElement) oElement.getRecord()).getSchemaType();
			if (schemaClass.isPresent()) {
				final String className = StringUtils.substringBeforeLast(domainClass.getCanonicalName(), ".") + "." + schemaClass.get().getName();
				try {
					final Class<?> actualClass = Class.forName(className);
					return new EntityClassMapper(actualClass, domainClass);
				} catch (final ClassNotFoundException e) {
					throw new EntityProxyMappingException("Error while retrieving "+ className +  " class", e);
				}
			}
		}
		return classMapper;
	}
	
	private Map<Field, Object> mapPrimitiveFieldsToProxy(final OElement oElement, final Map<String, Field> primitiveFields,
			final EntityClassMapper classMapper, final ClassDefinition classDefinition) {
		final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
		final Map<Field, Object> primitiveMap = new HashMap<>();
		final String rootName = classMapper.getRootName();
		primitiveFields.entrySet().forEach(entry -> primitiveMap.put(entry.getValue(), oElementAdapter.getProperty(oElement, rootName + entry.getKey())));
		return primitiveMap;
	}
	
	private <S extends Enum<S>> Map<Field, Object> mapEnumFieldsToProxy(final OElement oElement, final Map<String, Field> enumFields,
			final ClassDefinition classDefinition) {
		final Map<Field, Object> enumMap = new HashMap<>();
		enumFields.entrySet().forEach(entry -> {
			final String fieldName = entry.getKey();
			final Field fieldInProxy = entry.getValue();
			final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
			@Nullable
			final Object value = oElementAdapter.getProperty(oElement, fieldName);
			Object name = null;
			if (value instanceof OVertex) {
				name = ((OVertex) value).getProperty("name");
			}
			if (name != null) {
				@SuppressWarnings("unchecked")
				/* Fixes Java11 compile error: incompatible types: inference variable T has incompatible equality constraints Enum,S */
				final S enumValue = (S) Enum.valueOf(fieldInProxy.getType().asSubclass(Enum.class), name.toString());
				enumMap.put(fieldInProxy, enumValue);
			}
		});
		return enumMap;
	}

	private <S extends Enum<S>> Map<Field, Object> mapEnumCollectionsToProxy(final OElement oElement, final Map<String, Field> enumCollectionFields,
			final ClassDefinition classDefinition) {
		final Map<Field, Object> enumCollectionMap = new HashMap<>();
		enumCollectionFields.entrySet().forEach(entry -> {
			final String fieldName = entry.getKey();
			final Field fieldInProxy = entry.getValue();
			final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
			@Nullable
			final Object value = oElementAdapter.getProperty(oElement, fieldName);
			if (value instanceof ORecordLazyList) {
				((ORecordLazyList) value).convertLinks2Records();
				final List<S> enumList = new ArrayList<>();
				final Class<?> listClass = getListType(fieldInProxy);
				((ORecordLazyList) value).forEach(vertex -> {
					if (vertex instanceof OVertex) {
						final Object name = ((OVertex) vertex).getProperty("name");
						@SuppressWarnings("unchecked")
						/* Fixes Java11 compile error: incompatible types: inference variable T has incompatible equality constraints Enum,S */
						final S enumValue = (S) Enum.valueOf(listClass.asSubclass(Enum.class), name.toString());
						enumList.add(enumValue);
					}
				});
				enumCollectionMap.put(fieldInProxy, enumList);
			}
		});
		return enumCollectionMap;
	}
	
	private Map<Field, Object> mapEdgeRelationFieldsToEntity(final OElement oElement, final Map<String, Field> edgeFields,
			final EntityClassMapper classMapper) {
		final Map<Field, Object> edgeMap = new HashMap<>();
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(classMapper.getEntityClass());
		edgeFields.entrySet().forEach(entry -> {
			final Field field = entry.getValue();
			final EdgeInformation edgeInfo = assertNotNull(classDefinition.getEdgeInformation(field));
			final ORecord oRecord = oElement.getRecord();
			if (field.getType().isAnnotationPresent(RelationshipProperties.class)) {
				final Iterable<OEdge> vertices = ((OVertex) oRecord).getEdges(edgeInfo.getDirection(), edgeInfo.getName());
				vertices.forEach(vertex -> {
					final IEntityProxy data = findByIdInternal(field.getType(), vertex.getIdentity());
					edgeMap.put(field, data);
				});
			} else {
				final Iterable<OVertex> vertices = ((OVertex) oRecord).getVertices(edgeInfo.getDirection(), edgeInfo.getName());
				vertices.forEach(vertex -> {
					final IEntityProxy data = findByIdInternal(field.getType(), vertex.getIdentity());
					edgeMap.put(field, data);
				});
			}
			
		});
		return edgeMap;
	}
	
	private Map<Field, Object> mapEmbeddedRelationFieldsToEntity(final OElement oElement, final Map<String, Field> embeddedFields,
			final EntityClassMapper classMapper, final ClassDefinition classDefinition) {
		final Map<Field, Object> embeddedMap = new HashMap<>();
		embeddedFields.entrySet().forEach(entry -> {
			final Field field = entry.getValue();
			final String fieldName = entry.getKey();
			final OElementAdapter oElementAdapter= OElementAdapter.getOElementAdapter(classDefinition);
			@Nullable
			final Object element = oElementAdapter.getProperty(oElement, classMapper.getRootName() + fieldName);
			if (element instanceof OElement) {
				final IEntityProxy data = mapDataToEntity(new OResultInternal((OElement) element), new EntityClassMapper(field.getType()));
				embeddedMap.put(field, data);
			}
		});
		return embeddedMap;
	}
	
	/**
	 * Maps the custom properties from database to proxy object.
	 *
	 * @param oElement data retrieved from DB
	 * @param classDefinition class definition for entity class
	 * @param proxy a proxy object of entity type
	 */
	private void mapCustomProperties(final OElement oElement, final ClassDefinition classDefinition, final IEntityProxy proxy) {
		@Nullable
		final Field field = classDefinition.getCustomPropertyField();
		if (field != null) {
			final Map<String, List<CustomProperty>> customPropertiesMap = customPropertyService.readCustomProperties(oElement.getIdentity().toString(),
					getDbSession());
			proxy.__setFieldValue(customPropertiesMap, field);
		}
	}
	
	private void mapLinkListsToProxy(final OElement oElement, final Map<String, Field> linkLists, final IEntityProxy proxy,
			final EntityClassMapper classMapper) {
		linkLists.entrySet().forEach(entry -> {
			final String fieldName = entry.getKey();
			final Field fieldInProxy = entry.getValue();
			mapLazyLoadedFields(oElement, proxy, classMapper, fieldName, fieldInProxy);
		});
	}
	
	private void mapLazyLoadedFields(final OElement oElement, final IEntityProxy proxy, final EntityClassMapper classMapper, final String fieldName,
			final Field fieldInProxy) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(classMapper.getEntityClass());
		@Nullable
		final EdgeInformation edgeInformation = classDefinition.getEdgeInformation(fieldInProxy);
		final ODirection direction;
		final String relationshipFieldName;
		if (edgeInformation != null) {
			direction = edgeInformation.getDirection();
			relationshipFieldName = edgeInformation.getName();
		} else {
			direction = ODirection.OUT;
			relationshipFieldName = fieldName;
		}
		try {
			final OElementAdapter oElementAdapter= OElementAdapter.getOElementAdapter(classDefinition);
			if ((oElement instanceof OVertex && ((OVertex) oElement).getEdges(direction, relationshipFieldName).iterator().hasNext())
					|| oElementAdapter.getProperty(oElement, fieldName) != null) {
				LOGGER.debug(() -> "Collection of relation data field: " + fieldName + "Type: " + fieldInProxy.getType());
				proxy.__populateLazyCollectionReference(fieldName);
			}
		} catch (final IllegalArgumentException ex) {
			throw new IllegalStateException("Error while process link lists:: " + fieldName, ex);
		}
	}
	
	/**
	 * Returns the type of {@link List}.
	 *
	 * @param listField the list data field in entity class
	 * @return the type of {@link List}
	 */
	private Class<?> getListType(final Field listField) {
		final ParameterizedType listType = (ParameterizedType) listField.getGenericType();
		return (Class<?>) listType.getActualTypeArguments()[0];
	}

	ODatabaseSession getDbSession() {
		return sessionManager.getThreadDatabase();
	}

	@Nullable
	@Override
	public IEntityProxy getRespectiveEntityForFieldType(final ClassDefinition classDefinition, final OElement oElement, final Field linkField) {
		final String fieldName = linkField.getName();
		final OElementAdapter oElementAdapter = OElementAdapter.getOElementAdapter(classDefinition);
		@Nullable
		final Object propertyValue = oElementAdapter.getProperty(oElement, fieldName);
		if (propertyValue == null) {
			return null;
		} else if (propertyValue instanceof OVertex && RequestContextHolder.getRequestAttributes() != null) {
			/* using getSelf() here so that the @Cacheable annotation is respected */
			return getSelf().getCachedEntityProxyByVertex((OVertex) propertyValue, linkField.getType());
		} else if (propertyValue instanceof ORecordId && RequestContextHolder.getRequestAttributes() != null) {
			/* using getSelf() here so that the @Cacheable annotation is respected */
			return getSelf().getCachedEntityProxyByRecordId((ORecordId) propertyValue, linkField.getType());
		} else if (propertyValue instanceof OVertex || propertyValue instanceof ORecordId) {
			return getEntityProxyByVertexOrRecordID(propertyValue, linkField.getType());
		} else {
			throw new IllegalStateException("Unrecognized Orient data type");
		}
	}

	@Override
	public IEntityProxy getEntityProxyByVertexOrRecordID(final Object propertyValue, final Class<?> type) {
		if (propertyValue instanceof OVertex) {
			final OVertex oVertex = (OVertex) propertyValue;
			LOGGER.debug(() -> "Accessing value for type " + type.getSimpleName() + " vertex identity " + oVertex.getIdentity());
			return mapValueFromDB(oVertex, type);
		} else if (propertyValue instanceof ORecordId) {
			final ORecordId oRecordID = (ORecordId) propertyValue;
			LOGGER.debug(() -> "Accessing value for type " + type.getSimpleName() + " record identity " + oRecordID);
			return findById(type, oRecordID.toString());
		} else {
			throw new IllegalArgumentException("Unsupported type for " + propertyValue + " (" + propertyValue.getClass().getSimpleName()
					+ "). Expected OVertex or ORecordId");
		}
	}

	@Override
	@Cacheable(cacheManager = "springDataCacheManager", cacheNames = OrientRequestCacheManager.VERTEX_CACHE_NAME)
	public IEntityProxy getCachedEntityProxyByVertex(final OVertex oVertex, final Class<?> type) {
		LOGGER.debug(() -> "Accessing the cached value for type " + type.getSimpleName() + " vertex identity " + oVertex.getIdentity());
		return mapValueFromDB(oVertex, type);
	}
	
	@Override
	@Cacheable(cacheManager = "springDataCacheManager", cacheNames = OrientRequestCacheManager.RECORD_CACHE_NAME)
	public IEntityProxy getCachedEntityProxyByRecordId(final ORecordId oRecordId, final Class<?> type) {
		LOGGER.debug(() -> "Accessing the cached value for type " + type.getSimpleName() + " record identity " + oRecordId);
		return findById(type, oRecordId.toString());
	}
	
	private IEntityProxy mapValueFromDB(final OVertex oVertex, final Class<?> type) {
		final EntityClassMapper entityClassMapper = new EntityClassMapper(type);
		return mapDataToEntity(new OResultInternal(oVertex), entityClassMapper);
	}
}
