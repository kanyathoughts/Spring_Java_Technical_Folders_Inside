/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import java.lang.reflect.Method;

import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;
import org.springframework.data.repository.query.RepositoryQuery;

import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;

/**
 * Strategy used to build or execute query defined manually as a String or 
 * having it being derived from the method name.
 */
public final class OrientQueryLookupStrategy {

	private OrientQueryLookupStrategy() {}

	/**
	 * Instantiates a query building strategy to build a query string.
	 *
	 * @param operations instance of {@link OrientOperations}
	 * @param key enum value used to decide the strategy
	 * @return a query strategy
	 */
	public static QueryLookupStrategy create(final OrientOperations<?> operations, @Nullable final Key key) {
		if (key == null) {
			return new CreateIfNotFoundQueryLookupStrategy(operations);
		}
		switch (key) {
			case CREATE:
				return new CreateQueryLookupStrategy(operations);
			case USE_DECLARED_QUERY:
				return new DeclaredQueryLookupStrategy(operations);
			case CREATE_IF_NOT_FOUND:
				return new CreateIfNotFoundQueryLookupStrategy(operations);
			default:
				throw new UnsupportedQueryTypeException(String.format("Unsupported query lookup strategy %s!", key));
		}
	}

	private abstract static class AbstractQueryLookupStrategy implements QueryLookupStrategy {

		private final OrientOperations<?> operations;

		private AbstractQueryLookupStrategy(final OrientOperations<?> orientOperations) {
			operations = orientOperations;
		}

		@Override
		public final RepositoryQuery resolveQuery(final Method method, final RepositoryMetadata metadata, final ProjectionFactory factory,
				final NamedQueries namedQueries) {
			return resolveQuery(new OrientQueryMethod(method, metadata, factory), operations, namedQueries);
		}

		/**
		 * Based on the strategy it either resolves a native query or builds a named query.
		 *
		 * @param method declared in the repository
		 * @param orientOperations instance of {@link OrientOperations}
		 * @param namedQueries instance of {@link NamedQueries}
		 * @return a RepositoryQuery from the given QueryMethod that can be executed afterwards
		 */
		protected abstract RepositoryQuery resolveQuery(final OrientQueryMethod method, final OrientOperations<?> orientOperations,
				final NamedQueries namedQueries);
	}

	private static class CreateQueryLookupStrategy extends AbstractQueryLookupStrategy {

		private CreateQueryLookupStrategy(final OrientOperations<?> operations) {
			super(operations);
		}

		/**
		 * Named queries are constructed by using the method name.
		 */
		@Override
		protected RepositoryQuery resolveQuery(final OrientQueryMethod method, final OrientOperations<?> operations, final NamedQueries namedQueries) {
			try {
				return new PartTreeOrientQuery(method, operations);
			} catch (final IllegalArgumentException e) {
				throw new UnsupportedQueryTypeException(String.format("Could not create query metamodel for method %s!", method.toString()), e);
			}
		}
	}

	private static class DeclaredQueryLookupStrategy extends AbstractQueryLookupStrategy {

		private DeclaredQueryLookupStrategy(final OrientOperations<?> operations) {
			super(operations);
		}

		/**
		 * It executes a native query added via @Query annotation.
		 */
		@Override
		protected RepositoryQuery resolveQuery(final OrientQueryMethod method, final OrientOperations<?> operations, final NamedQueries namedQueries) {
			final String query = method.getAnnotatedQuery();
			if (query != null) {
				return new StringBasedOrientQuery(query, method, operations);
			}

			throw new IllegalStateException(String.format("Did neither find a NamedQuery nor an annotated query for method %s!", method));
		}
	}

	private static class CreateIfNotFoundQueryLookupStrategy extends AbstractQueryLookupStrategy {

		private final DeclaredQueryLookupStrategy strategy;
		private final CreateQueryLookupStrategy createStrategy;

		private CreateIfNotFoundQueryLookupStrategy(final OrientOperations<?> operations) {
			super(operations);
			strategy = new DeclaredQueryLookupStrategy(operations);
			createStrategy = new CreateQueryLookupStrategy(operations);
		}

		@Override
		protected RepositoryQuery resolveQuery(final OrientQueryMethod method, final OrientOperations<?> operations, final NamedQueries namedQueries) {
			try {
				return strategy.resolveQuery(method, operations, namedQueries);
			} catch (final IllegalStateException e) {
				return createStrategy.resolveQuery(method, operations, namedQueries);
			}
		}
	}
}
