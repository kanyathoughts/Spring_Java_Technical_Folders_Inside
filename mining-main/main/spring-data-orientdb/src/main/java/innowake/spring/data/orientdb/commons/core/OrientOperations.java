/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.commons.core;

import java.lang.reflect.Field;
import java.util.List;

import com.orientechnologies.orient.core.sql.executor.OResult;
import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.transaction.annotation.Transactional;
import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.OVertex;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.ogm.mapping.EntityClassMapper;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.query.OrientQueryMethod;

/**
 * Interface that specifies a basic set of OrientDB operations. 
 * 
 * @param <T> type of entity
 */
@Transactional(transactionManager = "orientdb-transaction", noRollbackFor = { NoRecordFoundException.class })
public interface OrientOperations<T> {

	/**
	 * Creates corresponding graph instances for a given entity object and persists the data in DB.
	 *
	 * @param entity object of class annotated with {@code @Entity}
	 * @return a proxy instance of the persisted entity
	 */
	IEntityProxy save(final T entity);

	/**
	 * Returns a proxy object mapped with values from DB for a given entity class type by using {@code rid}.
	 * 
	 * @param domainClass class annotated with {@code @Entity}
	 * @param rid record id saved in DB
	 * @return proxy object of type {@code domainClass} with values saved in database
	 * 
	 * @throws NoRecordFoundException if no record with given id is found
	 */
	IEntityProxy findById(final Class<?> domainClass, final String rid);

	/**
	 * Validates if a record exits in database with the given rid.
	 * 
	 * @param rid record id 
	 * @return Returns {@code true} if the element with the record id is present in the database
	 */
	boolean existsById(final String rid);

	/**
	 * Find all the records of entity class.
	 *
	 * @param clazz class annotated with {@code @Entity}
	 * @return all the entity objects of the given class, if no records are found it returns empty list.
	 */
	Iterable<T> findAll(final Class<T> clazz);

	/**
	 * Returns the total number of records of a given entity class type in database.
	 * 
	 * @param clazz class annotated with {@code @Entity}
	 * @return the count of the elements present in the db with the given entity type
	 */
	long count(final Class<T> clazz);

	/**
	 * Deletes the element with the record id given oRID present in the database.
	 * 
	 * @param oRID record id
	 */
	void deleteById(final String oRID);

	/**
	 * Deletes the given entity in the database.
	 * 
	 * @param entity object of class annotated with {@code @Entity}
	 */
	void delete(final T entity);

	/**
	 * Deletes all the entities present in the iterable @param entities from the database.
	 * 
	 * @param entities list of entities
	 */
	void deleteAll(final Iterable<? extends T> entities);

	/**
	 * Deletes all the entities present in the database of the type @param clazz.
	 * 
	 * @param clazz entity class
	 */
	void deleteAll(final Class<T> clazz);

	/**
	 * Find all the records of entity class and sorts based on @param sort.
	 *
	 * @param clazz entity class
	 * @param sort the sorting condition on which sorting is to be done
	 * @return all the entity objects of the given class
	 */
	Iterable<T> findAll(final Class<T> clazz, final Sort sort);

	/**
	 * Find all the records of entity class and returns a page.
	 *
	 * @param clazz entity class
	 * @param pageable the paging configuration
	 * @return page streamable containing the records
	 */
	Page<T> findAll(final Class<T> clazz, final Pageable pageable);

	/**
	 * Deletes the element with the ORID @param id present in the database.
	 * 
	 * @param id instance of {@link ORID}
	 */
	void deleteById(final ORID id);

	/**
	 * Validates if a given {@link ORID} exists in database.
	 * 
	 * @param rid instance of {@link ORID}
	 * @return Returns true if the element with the @param rid is present in the database
	 */
	boolean existsById(final ORID rid);

	/**
	 * Returns a proxy object mapped with values from DB for a given entity class type by using {@code rid}.
	 * 
	 * @param domainClass entity class
	 * @param rid instance of {@link ORID}
	 * @return proxy object of type {@code domainClass} with values saved in database
	 * 
	 * @throws NoRecordFoundException if no record with given id is found
	 */
	IEntityProxy findById(final Class<?> domainClass, final ORID rid);
	
	/**
	 * Returns a proxy object mapped with values from DB for a given entity class type by using {@code rid}.
	 * It is used for retrieving the dependent object values.
	 * 
	 * @param domainClass entity class
	 * @param rid instance of {@link ORID}
	 * @return proxy object of type {@code domainClass} with values saved in database
	 * 
	 * @throws NoRecordFoundException if no record with given id is found
	 */
	IEntityProxy findByIdInternal(final Class<?> domainClass, final ORID rid);

	/**
	 * Executes query and result is mapped to entity objects.
	 *
	 * @param query SQL query string to be executed
	 * @param entityClass type of entity
	 * @param queryMethod query method
	 * @param args value of the arguments passed to query
	 * @return collection of entity objects fetched from executing query
	 */
	List<IEntityProxy> query(final String query, final Class<?> entityClass, final OrientQueryMethod queryMethod, final Object... args);

	/**
	 * Executes query and the result is mapped to a single entity object. 
	 * If the result set consists more than a record, then returns only the first record.
	 *
	 * @param query SQL query string to be executed
	 * @param entityClass type of entity
	 * @param args value of the arguments passed to query
	 * @return an entity object fetched from executing query
	 */
	IEntityProxy command(final String query, final Class<?> entityClass, final Object... args);

	/**
	 * Executes either delete or count query and returns the number elements deleted or total count respectively.
	 *
	 * @param query SQL query string to be executed
	 * @param entityClass type of entity
	 * @param propertyName "@count" for count query and "count" for delete query
	 * @param args value of the arguments passed to query
	 * @return an entity object fetched from executing query
	 */
	Object command(final String query, final Class<?> entityClass, final String propertyName, final Object... args);

	/**
	 * Returns the next sequence number.
	 *
	 * @param sequenceName the name of the sequence.
	 * @return  the next value of the sequence
	 */
	Long getSequenceValue(final String sequenceName);
		
	/**
	 * Returns the data retrieved by querying with the criteria as {@link OrientClause}.
	 *
	 * @param domainClass the entity class
	 * @param clause criteria to build query
	 * @param pageable defines page size and element size
	 * @param queryArgs arguments for the criteria clause
	 * @return the data retrieved by querying with the criteria mentioned as clause
	 */
	Page<T> findAll(final Class<T> domainClass, final OrientClause clause, final Pageable pageable, Object... queryArgs);

	/**
	 * Returns an {@link IEntityProxy} that represents the value of the given {@code linkField} of the given {@code oElement}. This is used
	 * to lazy-load the values of fields of type LINK.
	 * @param classDefinition the class definition for the given {@code oElement}
	 * @param oElement the {@code OElement} to which the LINK property belongs
	 * @param linkField a field on the {@code oElement} with type LINK
	 * @return the field value wrapped in an {@code IEntityProxy}
	 */
	@Nullable
	IEntityProxy getRespectiveEntityForFieldType(final ClassDefinition classDefinition, final OElement oElement, final Field linkField);

	/**
	 * If field type is vertex then maps the entity class to prepare a map of fields
	 * else returns a proxy object mapped with values from DB for a given entity class type by using recordID
	 *
	 * @param propertyValue instance of {@link Object}
	 * @param type entity class
	 * @return maps the entity class or proxy object mapped with values from DB
	 */
	IEntityProxy getEntityProxyByVertexOrRecordID(final Object propertyValue, final Class<?> type);

	/**
	 * Get the instance of oVertex with request attribute and check if the request attribute is not null
	 * then maps the cached entity proxy class to prepare a map of fields.
	 *
	 * @param oVertex instance of {@link OVertex}
	 * @param type entity class
	 * @return map of fields
	 */
	IEntityProxy getCachedEntityProxyByVertex(final OVertex oVertex, final Class<?> type);

	/**
	 * Get the instance of object by RecordID and check if the request attribute is not null
	 * return a cached proxy object mapped with values from DB for a given entity class type by using {@link ORecordId}.
	 *
	 * @param recordID instance of {@link ORecordId}
	 * @param type entity class
	 * @return a cached proxy object mapped with values from DB for a given entity class type by using recordID
	 */
	IEntityProxy getCachedEntityProxyByRecordId(final ORecordId recordID, final Class<?> type);

	/**
	 * Maps the data fetched from DB to entity proxy object.
	 * 
	 * @param oResult the data retrieved from the DB
	 * @param classMapper instance of {@link EntityClassMapper}
	 * @return a proxy object of the entity class which holds its corresponding values from DB
	 */
	IEntityProxy mapDataToEntity(final OResult oResult, final EntityClassMapper classMapper);
}
