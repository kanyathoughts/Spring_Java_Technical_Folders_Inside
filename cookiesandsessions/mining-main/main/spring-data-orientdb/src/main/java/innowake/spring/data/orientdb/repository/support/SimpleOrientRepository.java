/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.support;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.id.ORecordId;

import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.repository.OrientRepository;

/**
 * Default implementation of the {@link PagingAndSortingRepository} interface for OrientDB.
 * @param <T> generic type to handle entity. 
 */
@Repository
@Transactional(transactionManager = "orientdb-transaction", noRollbackFor = { NoRecordFoundException.class })
public class SimpleOrientRepository<T> implements OrientRepository<T> {

	private static final String RID_NULL_ERROR = "Rid should not be null or empty.";
	private static final String ENTITIES_NULL_ERROR = "Entities should never be null";
	private static final String ENTITY_NULL_ERROR = "Entity should not be null";

	private final OrientOperations<T> operations;
	private final Class<T> domainClass;

	/**
	 * Initializes the instance of the default implementation of {@link PagingAndSortingRepository}
	 * 
	 * @param domainClass the type of the entity for which the repository instance would be created
	 * @param operations the instance of the class containing all the crud operations
	 */
	public SimpleOrientRepository(final Class<T> domainClass, final OrientOperations<T> operations) {
		this.operations = operations;
		this.domainClass = domainClass;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <S extends T> S save(@Nullable final S entity) {
		return (S) operations.save(assertNotNull(entity, ENTITY_NULL_ERROR));
	}

	@Override
	public <S extends T> Iterable<S> saveAll(@Nullable final Iterable<S> entities) {
		final Iterable<S> entitiesRef = assertNotNull(entities, ENTITIES_NULL_ERROR);
		final List<S> result = new ArrayList<>();
		entitiesRef.forEach(entity -> result.add(save(entity)));
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Optional<T> findById(@Nullable final String id) {
		if (id == null || id.isEmpty()) {
			throw new IllegalArgumentException(RID_NULL_ERROR);
		}
		return (Optional<T>) Optional.of(operations.findById(domainClass, id));
	}

	@Override
	public boolean existsById(@Nullable final String id) {
		if (id == null || id.isEmpty()) {
			throw new IllegalArgumentException(RID_NULL_ERROR);
		}
		return operations.existsById(id);
	}

	@Override
	public Iterable<T> findAll() {
		return operations.findAll(domainClass);
	}

	@Override
	public Iterable<T> findAllById(@Nullable final Iterable<String> ids) {
		final Iterable<String> idsRef = assertNotNull(ids, RID_NULL_ERROR);
		final List<T> result = new ArrayList<>();
		for (final String id : idsRef) {
			findById(id).ifPresent(result::add);
		}
		return result;
	}

	@Override
	public long count() {
		return operations.count(domainClass);
	}

	@Override
	public void deleteById(@Nullable final String id) {
		if (id == null || id.isEmpty()) {
			throw new IllegalArgumentException(RID_NULL_ERROR);
		}
		operations.deleteById(id);
	}

	@Override
	public void delete(@Nullable final T entity) {
		final T entityRef = assertNotNull(entity, ENTITY_NULL_ERROR);
		operations.delete(entityRef);
	}

	@Override
	public void deleteAll(@Nullable final Iterable<? extends T> entities) {
		final Iterable<? extends T> entitiesRef = assertNotNull(entities, ENTITIES_NULL_ERROR);
		operations.deleteAll(entitiesRef);
	}

	@Override
	public void deleteAll() {
		operations.deleteAll(domainClass);
	}

	@Override
	public Iterable<T> findAll(@Nullable final Sort sort) {
		final Sort sortRef = assertNotNull(sort, "sort instance must not be null");
		return operations.findAll(domainClass, sortRef);
	}

	@Override
	public Page<T> findAll(@Nullable final Pageable pageable) {
		final Pageable pageableRef = assertNotNull(pageable, "pageable instance must not be null");
		return operations.findAll(domainClass, pageableRef);
	}

	@Override
	public boolean existsById(final ORID id) {
		return operations.existsById(id);
	}

	@Override
	public void deleteById(final ORID id) {
		operations.deleteById(id);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Optional<T> findById(final ORID id) {
		return (Optional<T>) Optional.of(operations.findById(domainClass, id));
	}

	@Override
	public Page<T> findAll(final OrientClause clause, final Pageable pageable, final Object... queryArgs) {
		return operations.findAll(domainClass, clause, pageable, queryArgs);
	}

	@Override
	public void deleteAllById(final Iterable<? extends String> ids) {
		ids.forEach(id -> deleteById(new ORecordId(id)));
	}
	
}
