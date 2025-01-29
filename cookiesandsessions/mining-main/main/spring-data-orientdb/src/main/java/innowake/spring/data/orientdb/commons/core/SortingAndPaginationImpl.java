/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.commons.core;

import static innowake.lib.core.lang.Assert.assertFalse;

import java.util.List;
import java.util.stream.Collectors;

import innowake.spring.data.orientdb.repository.query.SortOrder;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.support.PageableExecutionUtils;
import org.springframework.stereotype.Component;

import com.github.raymanrt.orientqb.query.Query;
import com.orientechnologies.orient.core.exception.ORecordNotFoundException;
import com.orientechnologies.orient.core.metadata.sequence.OSequence;
import com.orientechnologies.orient.core.metadata.sequence.OSequenceLibrary;
import com.orientechnologies.orient.core.metadata.sequence.OSequenceLibraryImpl;
import com.orientechnologies.orient.core.metadata.sequence.OSequenceLibraryProxy;
import com.orientechnologies.orient.core.sql.executor.OResult;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.EntityClassMapper;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomPropertyService;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.query.OrientQuery;
import innowake.spring.data.orientdb.repository.query.OrientQueryMethod;
import innowake.spring.data.orientdb.repository.query.Pagination;
import innowake.spring.data.orientdb.repository.query.SortingUtils;

/**
 * A class to handle sorting, pagination, named and native queries on OrientDB database.
 * @param <T> type of entity 
 */
@Component
public class SortingAndPaginationImpl<T> extends OrientOperationsImpl<T> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SortingAndPaginationImpl.class);

	private static final String SORT_VAR = "$sort";

	/**
	 * Parameterized constructor to initialize {@link SessionManager} instances.
	 * 
	 * @param sessionManager instance used to create a orientdb session
	 * @param customPropertyService instance of the {@linkplain CustomPropertyService}
	 */
	@Autowired
	public SortingAndPaginationImpl(final SessionManager sessionManager, final CustomPropertyService customPropertyService) {
		super(sessionManager, customPropertyService);
	}

	@Override
	public Page<T> findAll(final Class<T> clazz, final Pageable pageable) {
		return PageableExecutionUtils.getPage(populateResultSetWithPaginationOrSorting(clazz, null, pageable), pageable, () -> count(clazz));
	}

	@Override
	public Iterable<T> findAll(final Class<T> clazz, final Sort sort) {
		return populateResultSetWithPaginationOrSorting(clazz, sort, null);
	}

	@Override
	public List<IEntityProxy> query(final String query, final Class<?> entityClass, final OrientQueryMethod queryMethod, final Object... args) {
		try (final OResultSet resultSet = getDbSession().query(query, args)) {
			final EntityClassMapper classMapper = new EntityClassMapper(entityClass);
			return resultSet.stream().map(result -> mapDataToEntity(result, classMapper)).collect(Collectors.toList());
		}
	}

	@Override
	public IEntityProxy command(final String query, final Class<?> entityClass, final Object... args) {
		LOGGER.debug(() -> "Command to be executed :: " + query);
		try (final OResultSet resultSet = getDbSession().command(query, args)) {
			final OResult record2 = resultSet.stream().findFirst()
					.orElseThrow(() -> new NoRecordFoundException(String.format("Query :: %s returned empty records", query)));
			return mapDataToEntity(record2, new EntityClassMapper(entityClass));
		}
	}

	@Override
	public Object command(final String query, final Class<?> entityClass, final String propertyName, final Object... args) {
		LOGGER.debug(() -> "Command executed :: " + query);
		try (final OResultSet result = getDbSession().command(query, args)) {
			final OResult record2 = result.stream().findFirst()
					.orElseThrow(() -> new NoRecordFoundException(String.format("Query :: %s returned empty records", query)));
			return record2.getProperty(propertyName);
		}
	}

	@SuppressWarnings("unchecked")
	private List<T> populateResultSetWithPaginationOrSorting(final Class<?> clazz, @Nullable final Sort sort, @Nullable final Pageable pageable) {
		final StringBuilder queryBuilder = new StringBuilder();
		if (pageable != null) {
			final SortOrder sortOrder = SortingUtils.convert(pageable.getSort());
			queryBuilder.append(sortOrder.asSelectClause());
			queryBuilder.append(" from ?");
			queryBuilder.append(sortOrder.asLetClause());
			queryBuilder.append(sortOrder.asOrderByClause());

		} else if (sort != null) {
			final SortOrder sortOrder = SortingUtils.convert(sort);
			queryBuilder.append(sortOrder.asSelectClause());
			queryBuilder.append(" from ?");
			queryBuilder.append(sortOrder.asLetClause());
			queryBuilder.append(sortOrder.asOrderByClause());
		} else {
			queryBuilder.append("select * from ? ");
		}
		if (pageable != null && pageable.isPaged()) {
			final Pagination pagination = new Pagination(pageable.getPageNumber(), pageable.getPageSize());
			queryBuilder.append(pagination);
		}
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(clazz);
		try (final OResultSet resultSet = getDbSession().execute("sql", queryBuilder.toString(), classDefinition.getEntityName())) {
			final EntityClassMapper classMapper = new EntityClassMapper(clazz);
			return resultSet.stream().map(record2 -> (T) mapDataToEntity(record2, classMapper))
					.collect(Collectors.toList());
		}
	}

	@Override
	public synchronized Long getSequenceValue(final String sequenceName) {
		assertFalse(StringUtils.isBlank(sequenceName), "Sequence name should not be null or empty");
		@Nullable
		final OSequenceLibrary sequenceLibrary = getDbSession().getMetadata().getSequenceLibrary();
		if (sequenceLibrary == null) {
			throw new MetadataException("Sequence library is null");
		}
		
		try {
			return Long.valueOf(getSequence(sequenceName, sequenceLibrary, /* refresh library */ false).next());
		} catch (final ORecordNotFoundException e) {
			/* This should only occur if the sequence library got stale due to dropping and recreating the sequences like we do in our test environment */
			LOGGER.debug("Sequence library got stale. Refreshing the library and retrying.", e);
			return Long.valueOf(getSequence(sequenceName, sequenceLibrary, /* refresh library */ true).next());
		}
	}

	private OSequence getSequence(final String sequenceName, final OSequenceLibrary sequenceLibrary, final boolean refreshLibrary) {
		if (refreshLibrary && sequenceLibrary instanceof OSequenceLibraryProxy) {
			final OSequenceLibraryProxy sequenceLibraryProxy = (OSequenceLibraryProxy) sequenceLibrary;
			final OSequenceLibraryImpl sequenceLibraryImpl = sequenceLibraryProxy.getDelegate();
			sequenceLibraryImpl.update();
		}
		
		@Nullable
		final OSequence sequence = sequenceLibrary.getSequence(sequenceName);
		if (sequence == null) {
			throw new MetadataException("Could not find the sequence " + sequenceName);
		}
		return sequence;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Page<T> findAll(final Class<T> domainClass, final OrientClause clauses, final Pageable pageable, final Object... queryArgs) {
		final ClassDefinition classDefinition = classDefinitionMapper.getClassDefinition(domainClass);
		final OrientQuery query = new OrientQuery();
		query.from(classDefinition.getEntityName()).where(clauses.getClause());
		final Query newQuery = new Query();
		final String countString = newQuery.select("count(*)").from(classDefinition.getEntityName()).where(clauses.getClause()).toString();
		final Long count = (Long) command(countString, domainClass, "count(*)", queryArgs);
		if (pageable.isPaged()) {
			((OrientQuery) query.limit(pageable.getPageSize())).offset(pageable.getOffset());
		}
		query.select("*");
		int sortIndex = 0;
		for (final Sort.Order sortOrder : pageable.getSort()) {
			final String propertyName = sortOrder.getProperty();
			query.let("sort" + sortIndex, () -> propertyName);
			if (sortOrder.isAscending()) {
				query.orderBy(SORT_VAR + sortIndex);
			} else {
				query.orderByDesc(SORT_VAR + sortIndex);
			}
			query.select(SORT_VAR + sortIndex);
			sortIndex++;
		}
		final List<IEntityProxy> queryResult = query(query.toString(), domainClass, queryArgs);
		return (Page<T>) PageableExecutionUtils.getPage(queryResult, pageable, count::longValue);
	}
	
	private List<IEntityProxy> query(final String query, final Class<T> domainClass, final Object... queryArgs) {
		try (final OResultSet resultSet = getDbSession().query(query, queryArgs)) {
			return resultSet.stream().map(result -> mapDataToEntity(result, new EntityClassMapper(domainClass)))
					.collect(Collectors.toList());
		}
	}

}
