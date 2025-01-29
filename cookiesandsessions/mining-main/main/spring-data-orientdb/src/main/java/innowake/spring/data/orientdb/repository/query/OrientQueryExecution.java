/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.query.Parameter;
import org.springframework.data.repository.query.ParameterAccessor;
import org.springframework.data.repository.query.Parameters;
import org.springframework.data.repository.query.ParametersParameterAccessor;
import org.springframework.data.support.PageableExecutionUtils;

import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Set of classes to contain query execution strategies.
 */
public abstract class OrientQueryExecution {

	/**
	 * Instance of {@link OrientOperations}
	 */
	protected final OrientOperations<?> orientOperations;

	/**
	 * Parameters used to build query string.
	 */
	protected final Parameters<?, ?> parameters;

	/**
	 * The entity class mapped in database.
	 */
	protected final Class<?> entityClass;

	/**
	 * Instantiation of {@link OrientQueryExecution}.
	 * 
	 * @param orientOperations instance of {@link OrientOperations}
	 * @param parameters passed during query execution
	 * @param entityClass mapped in database
	 */
	public OrientQueryExecution(final OrientOperations<?> orientOperations, final Parameters<?, ?> parameters, final Class<?> entityClass) {
		super();
		this.orientOperations = orientOperations;
		this.parameters = parameters;
		this.entityClass = entityClass;
	}

	/**
	 * Executes the given {@link AbstractOrientQuery} with the given values.
	 *
	 * @param query the orient query to be executed
	 * @param values parameters values of the query
	 *
	 * @return result of the executed query
	 */
	public Object execute(final AbstractOrientQuery query, final Object[] values) {
		return doExecute(query, values);
	}

	/**
	 * Method to be implemented by executions.
	 *
	 * @param query the orient query to be executed
	 * @param values parameters values of the query
	 *
	 * @return result of the executed query
	 */
	protected abstract Object doExecute(final AbstractOrientQuery query, final Object[] values);

	/**
	 * Prepares the parameter values, if the parameter is of bindable type.
	 *
	 * @param parameters passed as arguments to the query
	 * @param values of the parameters
	 * @return list of parameter values
	 */
	protected Object[] prepareParameters(final Parameters<?, ?> parameters, final Object[] values) {
		final List<Object> params = new ArrayList<>();
		int index = 0;
		for (final Parameter parameter : parameters) {
			if (parameter.isBindable()) {
				params.add(values[index]);
			}
			++index;
		}
		return params.toArray();
	}

	/**
	 * Executes the query to return a collection of entities.
	 *
	 */
	static class CollectionExecution extends OrientQueryExecution {

		/**
		 * Instantiates a new {@link CollectionExecution}.
		 *
		 * @param orientOperations instance of {@link OrientOperations}
		 * @param parameters arguments passed to the query
		 * @param entityClass mapped to database vertex
		 */
		public CollectionExecution(final OrientOperations<?> orientOperations, final Parameters<?, ?> parameters, final Class<?> entityClass) {
			super(orientOperations, parameters, entityClass);
		}

		@Override
		protected Object doExecute(final AbstractOrientQuery query, final Object[] values) {
			return orientOperations.query(query.toSql(values), entityClass, query.getQueryMethod(), prepareParameters(parameters, values));
		}
	}

	/**
	 * Executes the query to return a single entity.
	 *
	 */
	static class SingleEntityExecution extends OrientQueryExecution {

		/**
		 * Instantiates a new {@link SingleEntityExecution}.
		 *
		 * @param orientOperations instance of {@link OrientOperations}
		 * @param parameters arguments passed to the query
		 * @param entityClass mapped to database vertex
		 */
		public SingleEntityExecution(final OrientOperations<?> orientOperations, final Parameters<?, ?> parameters, final Class<?> entityClass) {
			super(orientOperations, parameters, entityClass);
		}

		@Override
		protected Object doExecute(final AbstractOrientQuery query, final Object[] values) {
			return orientOperations.command(query.toSql(values), entityClass, prepareParameters(parameters, values));
		}
	}

	/**
	 * Executes a count query to return total count of entities.
	 *
	 */
	static class CountExecution extends OrientQueryExecution {

		/**
		 * Instantiates a new {@link CountExecution}.
		 *
		 * @param orientOperations instance of {@link OrientOperations}
		 * @param parameters arguments passed to the query
		 * @param entityClass mapped to database vertex
		 */
		public CountExecution(final OrientOperations<?> orientOperations, final Parameters<?, ?> parameters, final Class<?> entityClass) {
			super(orientOperations, parameters, entityClass);
		}

		@Override
		protected Object doExecute(final AbstractOrientQuery query, final Object[] values) {
			return orientOperations.command(query.toSql(values), entityClass, "count(*)",  prepareParameters(parameters, values));
		}
	}

	/**
	 * Executes a query to return a {@link org.springframework.data.domain.Page} of entities.
	 */
	static class PagedExecution extends OrientQueryExecution {

		/**
		 * Instantiates a new {@link PagedExecution}.
		 *
		 * @param orientOperations instance of {@link OrientOperations}
		 * @param parameters arguments passed to the query
		 * @param entityClass mapped to database vertex
		 */
		public PagedExecution(final OrientOperations<?> orientOperations, final Parameters<?, ?> parameters, final Class<?> entityClass) {
			super(orientOperations, parameters, entityClass);
		}

		@Override
		protected Object doExecute(final AbstractOrientQuery query, final Object[] values) {
			final ParameterAccessor accessor = new ParametersParameterAccessor(parameters, values);
			final Object[] queryParams = prepareParameters(parameters, values);
			final Object countObj = orientOperations.command(query.toSqlCount(values), entityClass, query.getCountAlias(), queryParams);
			final Long count = countObj instanceof Long ? (Long) countObj : ((Integer) countObj).longValue();
			final Pageable pageable = accessor.getPageable();
			final List<IEntityProxy> content = orientOperations.query(query.toSql(values), entityClass, query.getQueryMethod(), queryParams);
			return PageableExecutionUtils.getPage(content, pageable, count::longValue);
		}
	}

	/**
	 * Executes a delete query and returns the total number entities deleted.
	 */
	static class DeleteExecution extends OrientQueryExecution {

		/**
		 * Instantiates a new {@link DeleteExecution}.
		 * 
		 * @param orientOperations instance of {@link OrientOperations}
		 * @param parameters arguments passed to the query
		 * @param entityClass mapped to database vertex
		 */
		public DeleteExecution(final OrientOperations<?> orientOperations, final Parameters<?, ?> parameters, final Class<?> entityClass) {
			super(orientOperations, parameters, entityClass);
		}

		@Override
		protected Object doExecute(final AbstractOrientQuery query, final Object[] values) {
			return orientOperations.command(query.toSql(values), entityClass, "count", prepareParameters(parameters, values));
		}
	}
}
