/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import org.apache.commons.lang3.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Query;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.core.OrientOperationsImpl;

/**
 * Used to execute the query build using @Query annotation.
 */
public class StringBasedOrientQuery extends AbstractOrientQuery {

	private final String queryString;
	@Nullable
	private String countQueryString;
	private boolean isCountQuery;
	private final boolean isDeleteQuery;

	/**
	 * Used to build a declarative query.
	 * 
	 * @param query a declarative query
	 * @param method the query method
	 * @param operations instance of {@link OrientOperationsImpl}
	 */
	public StringBasedOrientQuery(final String query, final OrientQueryMethod method, final OrientOperations<?> operations) {
		super(method, operations);
		queryString = query;
		final Query queryAnnotation = method.getQueryAnnotation();
		if (queryAnnotation != null) {
			isCountQuery = queryAnnotation.count();
			countQueryString = StringUtils.trimToNull(queryAnnotation.countQuery());
		}
		isDeleteQuery = query.toLowerCase().contains("delete");
	}

	@Override
	protected boolean isCountQuery() {
		return isCountQuery;
	}

	@Override
	protected boolean isDeleteQuery() {
		return isDeleteQuery;
	}

	@Override
	protected String toSql(final Object[] values) {
		return queryString;
	}

	@Override
	protected String toSqlCount(final Object[] values) {
		return countQueryString != null ? countQueryString : queryString;
	}

}
