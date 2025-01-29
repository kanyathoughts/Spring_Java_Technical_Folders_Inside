/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.repository.query.Parameters;
import org.springframework.data.repository.query.RepositoryQuery;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.springdata.annotations.Query;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.repository.query.OrientQueryExecution.CollectionExecution;
import innowake.spring.data.orientdb.repository.query.OrientQueryExecution.CountExecution;
import innowake.spring.data.orientdb.repository.query.OrientQueryExecution.DeleteExecution;
import innowake.spring.data.orientdb.repository.query.OrientQueryExecution.PagedExecution;
import innowake.spring.data.orientdb.repository.query.OrientQueryExecution.SingleEntityExecution;

/**
 * The base class to implement {@link RepositoryQuery} for OrientDB.
 */
public abstract class AbstractOrientQuery implements RepositoryQuery {

	private final OrientQueryMethod method;
	private final OrientOperations<?> operations;
	@Nullable
	private String countAliasString;

	/**
	 * Instantiates a new {@link AbstractOrientQuery}.
	 *
	 * @param method the query method
	 * @param operations instance of {@link OrientOperations}
	 */
	public AbstractOrientQuery(final OrientQueryMethod method, final OrientOperations<?> operations) {
		super();
		this.method = method;
		final Query queryAnnotation = method.getQueryAnnotation();
		if (queryAnnotation != null) {
			countAliasString = StringUtils.trimToNull(queryAnnotation.countQueryAlias());
		}
		this.operations = operations;
	}

	@Override
	public OrientQueryMethod getQueryMethod() {
		return method;
	}

	@Override
	@org.springframework.lang.Nullable
	public Object execute(final Object[] parameters) {
		return getExecution().execute(this, Assert.assertNotNull(parameters));
	}

	/**
	 * Builds the sql query with given parameters as values.
	 *
	 * @param params values in the query
	 * @return sql query string to be executed
	 */
	protected abstract String toSql(final Object[] params);

	/**
	 * Builds count query with the given parameters as values.
	 *
	 * @param params values in the query
	 * @return sql count query to be executed
	 */
	protected abstract String toSqlCount(final Object[] params);
	
	/**
	 * 
	 * Builds the count query alias
	 *
	 * @return sql count query alias
	 */
	protected String getCountAlias() {
		return countAliasString != null ? countAliasString : "count(*)";
	}

	/**
	 * Checks if it's count query.
	 *
	 * @return true, if it's count query
	 */
	protected abstract boolean isCountQuery();

	/**
	 * Checks if it's delete query.
	 *
	 * @return true, if it's delete query
	 */
	protected abstract boolean isDeleteQuery();

	/**
	 * Gets an execution for query based on method's return type.
	 *
	 * @return an execution type for handling queries
	 */
	protected OrientQueryExecution getExecution() {
		final Parameters<?, ?> parameters = method.getParameters();
		final Class<?> domainClass = method.getEntityInformation().getJavaType();

		if (method.isCollectionQuery()) {
			return new CollectionExecution(operations, parameters, domainClass);
		} else if (isCountQuery()) {
			return new CountExecution(operations, parameters, domainClass);
		} else if (method.isPageQuery()) {
			return new PagedExecution(operations, parameters, domainClass);
		} else if (method.isQueryForEntity()) {
			return new SingleEntityExecution(operations, parameters, domainClass);
		} else if (isDeleteQuery()) {
			return new DeleteExecution(operations, parameters, domainClass);
		}

		throw new UnsupportedQueryTypeException("Unsupported query execution");
	}

}
