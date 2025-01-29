/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;
import javax.sql.DataSource;

import org.apache.logging.log4j.ThreadContext;
import org.springframework.jdbc.BadSqlGrammarException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementCreator;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.RowMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.ConditionalConsumer;
import innowake.mining.shared.lang.CountingSteamBuilder;
import innowake.mining.shared.lang.DiffSet;
import innowake.mining.shared.lang.IterativeAction;

/**
 * Base class for Postgres DAOs.
 */
public abstract class PgDao extends PgUtil {
	
	protected static final Logger LOG = LoggerFactory.getLogger(PgDao.class);
	protected static final int LOG_ARG_LEN_MAX = 1000;
	
	/* make the current statement introspectable by JdbcProfilingAspect */
	public static final String CURRENT_STATEMENT_MDC_KEY = "PgDao.currentStatement";
	protected static final Logger PROFILING_LOG = LoggerFactory.getLogger("prof.db");
	
	private final JdbcTemplate jdbc;
	
	/**
	 * Creates a new DAO for Postgres.
	 * When creating a DAO from a Spring context consider using {@link PgDao#PgDao(JdbcTemplate)}.
	 * @param db Access to the Postgres database.
	 */
	protected PgDao(final DataSource db) {
		this(new JdbcTemplate(db));
	}
	
	/**
	 * Creates a new DAO for Postgres.
	 * @param jdbc Query execution context.
	 *             For profiling support make sure to use an injected instance, but always inject on the Service level not the DAO itself.
	 */
	protected PgDao(final JdbcTemplate jdbc) {
		this.jdbc = jdbc;
	}
	
	/**
	 * Method to process an entire query result.
	 */
	protected interface ResultSetConsumer {
		/**
		 * Processes the entire result.
		 * @param rs Query result.
		 * @throws SQLException If a database related error occurs.
		 */
		public void accept(ResultSet rs) throws SQLException;
	}
	
	/**
	 * Consumer to assign column values of a row in a result-set to an object.
	 * @param <T> Type of the target object.
	 */
	protected interface RowApplier<T> {
		/**
		 * Applies column values of a row to an object. 
		 * @param rs Query result, positioned on a row.
		 * @param obj Object to apply values to.
		 * @throws SQLException In case of an error accessing the result-set.
		 */
		public void apply(ResultSet rs, T obj) throws SQLException;
	}
	
	/**
	 * Function to extract one or more columns of a row in a result-set.
	 * @param <T> Type created from the extracted values.
	 */
	protected interface RowExtractor<T> {
		/**
		 * Extract one or more columns from a row.
		 * @param rs Query result, positioned on a row.
		 * @return Object created from column values.
		 * @throws SQLException In case of an error accessing the result-set.
		 */
		public T get(ResultSet rs) throws SQLException;
	}
	
	/**
	 * Optional reference to an extraction method for an Object from a result-set.
	 * @param <T> Type to extract.
	 */
	protected static class ResultReference<T> {
		@Nullable
		private RowExtractor<T> extractor;
		
		/**
		 * @param extractor Conversion method of one or more result columns into an Object.
		 */
		public void set(final RowExtractor<T> extractor) {
			this.extractor = extractor;
		}
		
		/**
		 * Extract Object from result-set or return {@code null} if no extraction method is available.
		 * @param rs Input row.
		 * @return Output object which may itself be {@code null}.
		 * @throws SQLException In case of an error accessing the result-set.
		 */
		@Nullable
		public T get(final ResultSet rs) throws SQLException {
			return extractor != null ? extractor.get(rs) : null;
		}
		
		/**
		 * If extraction method is defined, extract object and perform an operation on it.
		 * @param rs Input row.
		 * @param action Operation performed if extraction method is available. The object received may be {@code null}.
		 * @throws SQLException In case of an error accessing the result-set.
		 */
		public void ifPresent(final ResultSet rs, final Consumer<T> action) throws SQLException {
			if (extractor != null) {
				action.accept(get(rs));
			}
		}
	}
	
	/**
	 * Function to extract a value from a row in a result-set.
	 * @param <T> Type of the extracted value.
	 */
	protected interface FieldExtractor<T> {
		/**
		 * Extract a specific column from a row.
		 * @param rs Query result, positioned on a row.
		 * @param columnIndex Index of the column to extract.
		 * @return Value converted from the column.
		 * @throws SQLException In case of an error accessing the result-set.
		 */
		public T get(ResultSet rs, int columnIndex) throws SQLException;
	}
	
	/**
	 * Turns a query result into a Map.
	 * @param <K> Type of the key.
	 * @param <V> Type of the value.
	 */
	protected interface RowToMapMapper<K ,V> {
		/**
		 * Called for each record in the result to add it to a Map.
		 * @param rs Query result positioned on a record.
		 * @param m Map to add the current record to.
		 * @throws SQLException In case of an error accessing the result-set.
		 */
		public void map(ResultSet rs, Map<K, V> m) throws SQLException;
	}
	
	/**
	 * Utility for constructing query stings with corresponding parameter values and execute them using the DAOs JDBC template.
	 * <br>Identifiers in the query may be placed in backticks (`) which will be replaced with double quotes (") before execution.
	 */
	protected class QueryBuilder {
		private final StringBuilder builder;
		private List<Object> args;
		private int columns;
		
		/**
		 * Maintains a list of column conversions to be applied to an object.
		 * Column clauses are appended to the underlying query builder immediately.
		 * @param <T> Type of the target object.
		 */
		protected class ColumnMapper<T> {
			private final ArrayList<RowApplier<T>> mappers = new ArrayList<>();
			
			/**
			 * Appends a column to the underlying query builder, specifying how to apply its value to the target object of this mapper.
			 * @param clause Column clause.
			 * @param mapper Function providing a converter for the value at the respective column index in the result-set.
			 * @return This ColumnMapper.
			 */
			public ColumnMapper<T> appendColumn(final String clause, final IntFunction<RowApplier<T>> mapper) {
				return appendColumns(mapper, clause);
			}
			
			/**
			 * Appends one are more columns to the underlying query builder, specifying how to apply the values to the target object of this mapper.
			 * @param mapper Function providing a converter for the values from the respective column index in the result-set.
			 * @param clauses Column clauses.
			 * @return This ColumnMapper.
			 */
			public ColumnMapper<T> appendColumns(final IntFunction<RowApplier<T>> mapper, final String... clauses) {
				mappers.add(mapper.apply(QueryBuilder.this.appendColumns(clauses)));
				return this;
			}
			
			/**
			 * Applies a method for appending columns to the underlying query builder, specifying how to apply the values to the target object of this mapper.
			 * @param mapper Function providing a converter for the values from the respective column index in the result-set.
			 * @param appender Function for adding column clauses that must return the number of columns it actually added.
			 * @return This ColumnMapper.
			 */
			public ColumnMapper<T> appendColumns(final IntFunction<RowApplier<T>> mapper, final Consumer<QueryBuilder> appender) {
				mappers.add(mapper.apply(QueryBuilder.this.appendColumns(appender)));
				return this;
			}
			
			/**
			 * Applies values from the current row of a result-set to an object.
			 * @param rs Query result, positioned on a row.
			 * @param obj Object to apply values to.
			 * @return Original object with values applied.
			 * @throws SQLException In case of an error accessing the result-set.
			 */
			public T map(final ResultSet rs, final T obj) throws SQLException {
				for (final var mapper : mappers) {
					mapper.apply(rs, obj);
				}
				return obj;
			}
		}
		
		/**
		 * Creates a new query builder.
		 * @param builder The string builder backing the query builder.
		 */
		protected QueryBuilder(final StringBuilder builder) {
			this.builder = builder;
			this.args = new ArrayList<>();
		}
		
		/**
		 * Appends a string and optionally arguments to the query.
		 * @param s Query substring.
		 * @param args Optionally, arguments for parameters used in the string.
		 * @return This builder again.
		 */
		public QueryBuilder append(final String s, final Object... args) {
			builder.append(s);
			if (args.length > 0) {
				addArgs(args);
			}
			return this;
		}
		
		/**
		 * Append method keeping track of columns in the result-set.
		 * @param s A column clause. Will be automatically prepended with ", " for all but the first column.
		 * @return The index of this column in the result-set, provided this method has been used consistently.
		 */
		public int appendColumn(@Nullable final String s) {
			if (this.columns > 0) {
				builder.append(", ");
			}
			if (s != null) {
				append(s);
			}
			return ++columns;
		}
		
		/**
		 * Append method keeping track of columns in the result-set.
		 * @param clauses Column clauses. Each will be automatically prepended with ", " except for the first column.
		 * @return The index of the first specified column in the result-set, provided this method has been used consistently.
		 */
		public int appendColumns(final String... clauses) {
			if (clauses.length < 1) {
				throw new IllegalArgumentException("At least one column must be specified");
			}
			final int currentColumn = this.columns;
			for (final String clause : clauses) {
				appendColumn(clause);
			}
			return currentColumn + 1;
		}
		
		/**
		 * Applies a method for appending columns to the underlying query builder.
		 * @param appender Function for adding column clauses that must return the number of columns it actually added.
		 * @return This ColumnMapper.
		 */
		public int appendColumns(final Consumer<QueryBuilder> appender) {
			final int currentColumn = this.columns;
			appender.accept(this);
			if (this.columns <= currentColumn) {
				throw new IllegalStateException("Appender did not add any columns");
			}
			return currentColumn + 1;
		}
		
		/**
		 * Appends one or more columns to the query, providing a function to create an object from their values.
		 * @param <T> Type of the created object.
		 * @param s Column clauses.
		 * @param extractor Function providing a converter for the values from the respective column index in the result-set.
		 * @return Function creating an object from the column values.
		 */
		public <T> RowExtractor<T> appendColumns(final IntFunction<RowExtractor<T>> extractor, final String... s) {
			return extractor.apply(appendColumns(s));
		}
		
		/**
		 * Appends one or more columns to the query, providing a function to create an object from their values.
		 * @param <T> Type of the created object.
		 * @param ref Reference to the extraction method.
		 * @param s Column clauses.
		 * @param extractor Function providing a converter for the values from the respective column index in the result-set.
		 * @return Function creating an object from the column values.
		 */
		public <T> QueryBuilder appendColumns(final ResultReference<T> ref, final IntFunction<RowExtractor<T>> extractor, final String... s) {
			ref.set(extractor.apply(appendColumns(s)));
			return this;
		}
		
		/**
		 * Appends a parameter string to the query depending on the type of ID.
		 * The strings must contain exactly one parameter operator (?).
		 * @param id Entity ID wrapper.
		 * @param uidParameter String to append for a UUID or in case the Entity ID is void.
		 * @param nidParameter String to append for a numeric ID.
		 * @return This builder again.
		 */
		public QueryBuilder appendId(@Nullable final EntityId id, final String uidParameter, final String nidParameter) {
			(id == null ? EntityId.VOID : id).apply(
				uid -> append(uidParameter, uid),
				nid -> append(nidParameter, nid),
				() -> append(uidParameter).addArg(null));
			return this;
		}
		
		/**
		 * Appends a parameter string to the query depending on the type of ID.
		 * That is, {@code uid = ?} in case of a UUID or {@code nid = ?} in case of a numeric ID.
		 * @param id Entity ID wrapper.
		 * @return This builder again.
		 */
		public QueryBuilder appendId(final EntityId id) {
			return appendId(id, "uid = ?", "nid = ?");
		}
		
		/**
		 * Appends a parameter string to the query depending on the type of ID.
		 * That is, {@code <reference>.uid = ?} in case of a UUID or {@code <reference>.nid = ?} in case of a numeric ID.
		 * @param id Entity ID wrapper.
		 * @param reference Reference for the ID field (table name or alias).
		 * @return This builder again.
		 */
		public QueryBuilder appendId(final EntityId id, final String reference) {
			return appendId(id, reference + ".uid = ?", reference + ".nid = ?");
		}

		/**
		 * Appends parameter strings to the query depending on the types of IDs in {@code ids}. Void Entity IDs in {@code ids} are skipped.
		 * <p>If both, uids and nids must be appended, then an {@code OR} clause is added containing both {@code uidParameter} and {@code nidParameter}. The
		 * caller is responsible for setting parentheses if required. Example:</p>
		 * <code>
		 * queryBuilder.appendIds(ids, "any(?)", "uid = (SELECT uid FROM entity WHERE nid = any(?))");
		 * queryBuilder.append("NOT (").appendIds(ids, "any(?)", "uid = (SELECT uid FROM entity WHERE nid = any(?))").append(")");
		 * </code>
		 *
		 * @param ids list of {@link EntityId EntityIds}
		 * @param uidParameter String to append for all UUIDs in {@code ids}. If no 
		 * @param nidParameter String to append for all numeric IDs in {@code ids}
		 * @return This builder again.
		 */
		public QueryBuilder appendIds(final Collection<EntityId> ids, final String uidParameter, final String nidParameter) {
			final Collection<UUID> uIds = EntityId.allUids(ids);
			if (uIds.size() == ids.size()) {
				append(uidParameter, arrayFromCollection(PgType.UUID, uIds));
			} else {
				final Collection<Long> nIds = EntityId.allNids(ids);
				if (nIds.size() == ids.size()) {
					append(nidParameter, arrayFromCollection(PgType.LONG, nIds));
				} else {
					append(uidParameter, arrayFromCollection(PgType.UUID, uIds));
					append(" OR ");
					append(nidParameter, arrayFromCollection(PgType.LONG, nIds));
				}
			}
			return this;
		}
		
		/**
		 * Appends an expression along with an ordering direction.
		 * @see #appendOrder(String, SortDirection, boolean)
		 * @param expression Expression to order by.
		 * @param direction Ordering direction.
		 * @return This builder again.
		 */
		public QueryBuilder appendOrder(final String expression, final SortDirection direction) {
			return appendOrder(expression, direction, false);
		}

		/**
		 * Appends an expression along with an ordering direction and a NULLS LAST clause.
		 * @param expression Expression to order by.
		 * @param direction Ordering direction.
		 * @param nullsLast Whether to append a NULLS LAST clause.
		 * @return This builder again.
		 */
		public QueryBuilder appendOrder(final String expression, final SortDirection direction, final boolean nullsLast) {
			return append(expression).when(direction.isDescending(), q -> q.append(" DESC")).when(nullsLast, q -> q.append(" NULLS LAST"));
		}
		
		/**
		 * Appends an expression in the form [field] [comparator] [value].
		 * @param field Field name or expression to compare.
		 * @param comperator Type of comparison.
		 * @param value Value to compare.
		 * @return This builder again.
		 */
		public QueryBuilder appendComparison(String field, final Comperator comperator, final Object value) {
			return append(field).append(" ").append(comperator.operator()).append(" ?", value);
		}
		
		/**
		 * Appends a query argument for a parameter (?) to the query.
		 * @param value Argument value.
		 * @return This builder again.
		 */
		public QueryBuilder addArg(@Nullable final Object value) {
			this.args.add(value);
			return this;
		}
		
		/**
		 * Appends an array argument for a parameter (?) to the query.
		 * @param type Type of the array elements. 
		 * @param values Array values.
		 * @return This builder again.
		 */
		public QueryBuilder addArg(final PgType type, @Nullable final Collection<?> values) {
			addArg(arrayFromCollection(type, values));
			return this;
		}
		
		/**
		 * Appends query arguments for parameters (?) to the query.
		 * @param values Argument values.
		 * @return This builder again.
		 */
		public QueryBuilder addArgs(final Object... values) {
			this.args.addAll(Arrays.asList(values));
			return this;
		}
		
		/**
		 * Appends multiple array arguments for parameters (?) to the query.
		 * This can be useful for passing results of {@link #pivot(Iterable, Function...)}.
		 * @param values Collections providing the content for the arrays.
		 * @param type SQL types of the Objects in the corresponding Collection.
		 * @return This builder again.
		 */
		public QueryBuilder addArgs(final Collection<?>[] values, final PgType... type) {
			if (values.length != type.length) {
				throw new IllegalArgumentException("Number of values and value types must match");
			}
			for (var n = 0; n < type.length; n++) {
				addArg(arrayFromCollection(type[n], values[n]));
			}
			return this;
		}
		
		/**
		 * Performs a subset of operations on this query builder.
		 * @param action Consumer receiving this builder.
		 * @return This builder again.
		 */
		public QueryBuilder with(final Consumer<QueryBuilder> action) {
			action.accept(this);
			return this;
		}
		
		/**
		 * Performs optional operations on this query builder.
		 * @param condition Condition that needs to be fulfilled.
		 * @param then Consumer conditionally receiving this builder.
		 * @return This builder again.
		 */
		public QueryBuilder when(final boolean condition, final Consumer<QueryBuilder> then) {
			if (condition) {
				then.accept(this);
			}
			return this;
		}
		
		/**
		 * Performs optional operations on this query builder using an argument in case it is not {@code null}.
		 * @param <T> Type of the argument.
		 * @param arg Argument to the operation.
		 * @param action Consumer receiving this builder and the argument.
		 * @return This builder again.
		 */
		public <T> QueryBuilder when(@Nullable final T arg, final BiConsumer<QueryBuilder, T> action) {
			if (arg != null) {
				action.accept(this, arg);
			}
			return this;
		}

		/**
		 * Performs optional operations on this query builder using an argument, when the predicate returns true.
		 * @param <T> Type of the argument.
		 * @param arg Argument to the operation.
		 * @param check Predicate condition to test if the action shall be applied
		 * @param action Consumer receiving this builder and the argument.
		 * @return This builder again.
		 */
		public <T> QueryBuilder when(@Nullable final T arg, final Predicate<T> check, final BiConsumer<QueryBuilder, T> action) {
			if (check.test(arg)) {
				action.accept(this, arg);
			}
			return this;
		}

		/**
		 * Performs optional operations on this query builder depending on whether a Definable has been set.
		 * @param <T> Type of the argument.
		 * @param arg Argument to the operation.
		 * @param action Consumer receiving this builder and the argument.
		 * @return This builder again.
		 */
		public <T> QueryBuilder whenDefined(final Definable<T> arg, final BiConsumer<QueryBuilder, T> action) {
			if (arg.isDefined()) {
				action.accept(this, arg.get());
			}
			return this;
		}
		
		/**
		 * Performs optional operations on this query builder depending on whether a Definable has been set.
		 * @param <T> Type of the argument.
		 * @param arg Argument to the operation.
		 * @param action Consumer receiving this builder.
		 * @return This builder again.
		 */
		public <T> QueryBuilder whenNotDefined(final Definable<T> arg, final Consumer<QueryBuilder> action) {
			if (! arg.isDefined()) {
				action.accept(this);
			}
			return this;
		}
		
		/**
		 * Appends the limit clause with the given {@code limit}.
		 *
		 * @param limit the limit to set, &gt 0
		 * @return This builder again.
		 */
		public QueryBuilder limit(final Integer limit) {
			builder.append(" LIMIT ?");
			addArgs(limit);
			return this;
		}
		
		/**
		 * Returns the built query string. Any back-ticks (`) will be replaced by double-quotes (").
		 */
		@Override
		public String toString() {
			return pgEscape(builder.toString());
		}
		
		
		/**
		 * Retrieves the list of arguments added to the query.
		 * @return List of values.
		 */
		public Object[] getArgs() {
			return args.toArray(EMPTY_OBJECT_ARRAY);
		}
		
		private JdbcContext build() {
			return new JdbcContext(this.toString());
		}
		
		/**
		 * Performs a query and maps the result to an arbitrary object structure.
		 * For querying a single object or value see {@link #first}.
		 * For queries with multiple statements see {@link #buildAll}.
		 * @param <T> Type to return.
		 * @param mapper Processing function called once for the entire result-set. 
		 * @return Mapped result.
		 */
		@Nullable
		public <T> T build(final ResultSetExtractor<T> mapper) {
			return build().execute(args.stream(), mapper);
		}
		
		/**
		 * Executes a query with multiple statements producing multiple results.
		 * Result consumers will need references to external Collections or Stream builders to produce individual objects.
		 * See {@link PreparedStatement#getMoreResults()} for details on the underlying concept.
		 * @param actions A Consumer for each query that produces a result-set, called for each row in the respective result.
		 * @return The number of affected rows for each count result, if any.
		 */
		public int[] buildAll(final ResultSetConsumer ... actions) {
			return build().executeAll(args.stream(), actions);
		}
		
		/**
		 * Performs a query and returns a single value or object.
		 * @param <T> Type to return.
		 * @param mapper Processing function called once with the result-set positioned on the first row.
		 * @return The mapped value or an empty Optional if the result had no rows or the mapper returned {@code null}. 
		 */
		public <T> Optional<T> first(final ResultSetExtractor<T> mapper) {
			return build().first(args.stream(), mapper);
		}
		
		/**
		 * Performs a query and maps the result to a List of objects.
		 * For querying a single object or value see {@link #first}.
		 * @param <T> Type of returned objects.
		 * @param mapper Processing function called once for each row. 
		 * @return List created from the query result.
		 */
		public <T> List<T> toList(final RowMapper<T> mapper) {
			return build().toList(args.stream(), mapper);
		}
		
		/**
		 * Performs a query and maps the result to a Set of objects.
		 * For querying a single object or value see {@link #first}.
		 * @param <T> Type of returned objects.
		 * @param mapper Processing function called once for each row. 
		 * @return Set created from the query result.
		 */
		public <T> Set<T> toSet(final RowMapper<T> mapper) {
			return build().toSet(args.stream(), mapper);
		}
		
		/**
		 * Performs a query and creates a Map from the result.
		 * @param <K> Type of the Map keys.
		 * @param <V> Type of the Map values.
		 * @param mapper Processing function called once for each row.
		 * @return Map created from the query result.
		 */
		public <K, V> Map<K, V> toMap(final RowToMapMapper<K, V> mapper) {
			return build().toMap(args.stream(), mapper);
		}
		
		/**
		 * Wraps a query in a pagination builder which may be invoked to return a paged or unpaged result.
		 * @param <T> Type of objects to create from the query.
		 * @param paging Optional pagination and ordering specification.
		 * @param mapper Function to create a corresponding object for each row in the result.
		 * @return Pagination builder for the query.
		 */
		public <T> Paged.Builder<T> toPageable(@Nullable final Pagination paging, final RowMapper<T> mapper) {
			return toPageable(paging, null, mapper);
		}
		
		/**
		 * Wraps a query in a pagination builder which may be invoked to return a paged or unpaged result.
		 * @param <T> Type of objects to create from the query.
		 * @param paging Optional pagination and ordering specification.
		 * @param sort For performance reasons, if sorting is used it should be applied on the top level where the pagination occurs rather than on the base query.
		 * @param mapper Function to create a corresponding object for each row in the result.
		 * @return Pagination builder for the query.
		 */
		public <T> Paged.Builder<T> toPageable(@Nullable final Pagination paging, @Nullable final OrderStreamBuilder sort, final RowMapper<T> mapper) {
			if (paging != null && ! paging.isSubset()) {
				builder.insert(0, "SELECT *, row_number() OVER () " + Paged.ROW_NUMBER_COLUMN
						+ ", count(*) OVER () " + Paged.ROW_COUNT_COLUMN + " FROM (");
				append(") _paged");
			}
			if (sort != null) {
				with(sort::build);
			}
			if (paging != null) {
				append(" OFFSET ? LIMIT ?");
				addArgs(paging.getOffset(), paging.getSize());
			}
			return new Paged.Builder<>(paging) {
				@Override
				public <U> U query(final Function<Stream<T>, U> streamOperation) {
					return build().stream(args.stream(), (rs, row) -> update(mapper.mapRow(rs, row), rs), streamOperation);
				}
			};
		}
		
		/**
		 * Performs a data modifying query, like INSERT, UPDATE, DELETE.
		 * @return Number of created, modified or removed records.
		 */
		public int update() {
			return build().update(args.stream());
		}
		
		/**
		 * Performs a modifying query in batches. Each execution will be parameterized with the arguments set on the query builder
		 * plus the arguments supplied by each individual argument Stream.
		 * @param batchArgs Streams providing the arguments for each query execution.
		 * @param batchSize Maximum size of one batch.
		 * @return Affected number of rows for each query execution.
		 */
		public int[] updateBatch(final Stream<Stream<Object>> batchArgs, final int batchSize) {
			return build().updateBatch(batchArgs.map(s -> Stream.concat(args.stream(), s)).toList(), batchSize);
		}
		
		/**
		 * Performs a data modifying query, optionally throwing an exception if no record was found to be modified.
		 * @param onZero Supplier of the exception.
		 * @return Number of records modified.
		 */
		public int updateOrThrow(@Nullable final Supplier<PersistenceException> onZero) {
			return build().updateOrThrow(args.stream(), onZero);
		}
	}
	
	protected class JdbcContext {
		private final String query;
		private int argSets;
		
		protected JdbcContext(final String query) {
			this.query = query;
		}
		
		private <T> T perform(final Function<JdbcTemplate, T> operation) {
			if (PROFILING_LOG.isDebugEnabled()) {
				ThreadContext.put(CURRENT_STATEMENT_MDC_KEY, query);
			}
			try {
				return operation.apply(jdbc);
			} catch (final BadSqlGrammarException e) {
				LOG.error(query, e);
				throw e;
			} finally {
				if (PROFILING_LOG.isDebugEnabled()) {
					ThreadContext.remove(CURRENT_STATEMENT_MDC_KEY);
				}
			}
		}
		
		private String logHeader() {
			return "Query @" + Integer.toHexString(hashCode());
		}
		
		private String logArgString(@Nullable final Object arg) {
			final var strArg = new StringBuilder();
			if (arg != null) {
				strArg.append(arg.getClass().getName());
				strArg.append(" [");
				if (arg.getClass().isArray() && ! arg.getClass().getComponentType().isPrimitive()) {
					strArg.append(Arrays.toString(((Object[]) arg)));
				} else {
					strArg.append(arg.toString());
				}
				strArg.append("]");
			} else {
				strArg.append("null");
			}
			final int len = strArg.length();
			return len > LOG_ARG_LEN_MAX ? strArg.substring(0, LOG_ARG_LEN_MAX) + "...[+" + (len - LOG_ARG_LEN_MAX) + "]" : strArg.toString();
			
		}
		
		protected void applyArgs(final PreparedStatement st, final Stream<Object> args) throws SQLException {
			var idx = 1;
			for (final Object arg : (Iterable<Object>) args::iterator) {
				if (LOG.isDebugEnabled()) {
					LOG.debug(logHeader() + " #" + argSets + ":" + idx + " " + logArgString(arg));
				}
				st.setObject(idx++, (arg != null && PgArray.class.isAssignableFrom(arg.getClass())) ? ((PgArray) arg).toJdbcArray(st.getConnection()) : arg);
			}
			argSets++;
		}
		
		protected String applyQuery() {
			LOG.debug(() -> logHeader() + " [" + query + "]");
			return query;
		}
		
		/**
		 * Builds a Statement from the query string and logs it if the log level is debug.
		 * @param con Database connection.
		 * @return Statement prepared from the query string.
		 * @throws SQLException If a database error occurs.
		 */
		protected PreparedStatement applyQuery(final Connection con) throws SQLException {
			return con.prepareStatement(applyQuery());
		}
		
		/**
		 * Builds a Statement from the query string (see {@link #applyQuery(Connection)}
		 * and sets the query parameters (see {@link #applyArgs(PreparedStatement, Stream) }).
		 * @param con Database connection.
		 * @param args Arguments to the query.
		 * @return Statement prepared from the final query string with arguments applied.
		 * @throws SQLException If a database error occurs.
		 */
		protected PreparedStatement applyQueryWithArgs(final Connection con, final Stream<Object> args) throws SQLException {
			final PreparedStatement st = applyQuery(con);
			applyArgs(st, args);
			return st;
		}
		
		/**
		 * Executes a query and maps the result to an arbitrary object structure.
		 * For querying a single object or value see {@link #first}.
		 * For queries with multiple statements see {@link #executeAll}.
		 * @param <T> Type to return.
		 * @param args Arguments to the query.
		 * @param mapper Processing function called once for the entire result-set. 
		 * @return Mapped result.
		 */
		@Nullable
		public <T> T execute(final Stream<Object> args, final ResultSetExtractor<T> mapper) {
			return perform(t -> t.query(this::applyQuery, st -> applyArgs(st, args), mapper));
		}
		
		/**
		 * Executes a query with multiple statements producing multiple results.
		 * Result consumers will need references to external Collections or Stream builders to produce individual objects.
		 * See {@link PreparedStatement#getMoreResults()} for details on the underlying concept.
		 * @param args Arguments to the query.
		 * @param actions A Consumer for each query that produces a result-set, called for each row in the respective result.
		 * @return The number of affected rows for each count result, if any.
		 */
		public int[] executeAll(final Stream<Object> args, final ResultSetConsumer ... actions) {
			final var counts = IntStream.builder();
			perform(t -> t.execute((PreparedStatementCreator) con -> this.applyQueryWithArgs(con, args), st -> {
				consumeResults(st, actions, counts);
				return null;
			}));
			return counts.build().toArray();
		}
		
		private void consumeResults(final PreparedStatement st, final ResultSetConsumer[] actions, final IntStream.Builder counts) throws SQLException {
			int action = 0;
			boolean more = st.execute();
			int count = st.getUpdateCount();
			while (more || count >= 0) {
				if (count < 0) {
					if (action < actions.length) {
						consumeResult(st, actions[action++]);
					} else {
						throw new IllegalStateException("Missing comsumer for result.");
					}
				} else {
					counts.accept(count);
				}
				more = st.getMoreResults();
				count = st.getUpdateCount();
			}
			if (action < actions.length) {
				throw new IllegalStateException("No more results available.");
			}
		}
		
		private void consumeResult(final PreparedStatement st, final ResultSetConsumer action) throws SQLException {
			try (final ResultSet rs = st.getResultSet()) {
				while (rs.next()) {
					action.accept(rs);
				}
			}
		}
		
		/**
		 * Performs a query and returns a single value or object.
		 * @param <T> Type to return.
		 * @param args Arguments to the query.
		 * @param mapper Processing function called once with the result-set positioned on the first row.
		 * @return The mapped value or an empty Optional if the result had no rows or the mapper returned {@code null}. 
		 */
		public <T> Optional<T> first(final Stream<Object> args, final ResultSetExtractor<T> mapper) {
			return Optional.ofNullable(execute(args, rs -> {
				if (! rs.next()) {
					return null;
				}
				return mapper.extractData(rs);
			}));
		}
		
		private <T, U> U stream(final Stream<Object> args, final RowMapper<T> mapper, final Function<Stream<T>, U> streamOperation) {
			/* CAUTION: For a streamed query, the associated ResultSet will remain open and block DB resources until the Stream
			 * is fully consumed or explicitly closed. Therefore, a try-with is crucial to prevent resource leaks.
			 */
			try (final Stream<T> result = perform(t -> t.queryForStream(this::applyQuery, st -> applyArgs(st, args), mapper))) {
				return streamOperation.apply(result);
			}
		}
		
		/**
		 * Performs a query and maps the result to a List of objects.
		 * For querying a single object or value see {@link #first}.
		 * @param <T> Type of returned objects.
		 * @param args Arguments to the query.
		 * @param mapper Processing function called once for each row. 
		 * @return List created from the query result.
		 */
		public <T> List<T> toList(final Stream<Object> args, final RowMapper<T> mapper) {
			return stream(args, mapper, Stream::toList);
		}
		
		/**
		 * Performs a query and maps the result to a Set of objects.
		 * For querying a single object or value see {@link #first}.
		 * @param <T> Type of returned objects.
		 * @param args Arguments to the query.
		 * @param mapper Processing function called once for each row. 
		 * @return Set created from the query result.
		 */
		public <T> Set<T> toSet(final Stream<Object> args, final RowMapper<T> mapper) {
			return stream(args, mapper, s -> s.collect(Collectors.toSet()));
		}
		
		/**
		 * Performs a query and creates a Map from the result.
		 * @param <K> Type of the Map keys.
		 * @param <V> Type of the Map values.
		 * @param args Arguments to the query.
		 * @param mapper Processing function called once for each row.
		 * @return Map created from the query result.
		 */
		public <K, V> Map<K, V> toMap(final Stream<Object> args, final RowToMapMapper<K, V> mapper) {
			final LinkedHashMap<K, V> m = new LinkedHashMap<>();
			execute(args, rs -> {
				while (rs.next()) {
					mapper.map(rs, m);
				}
				return null;
			});
			return m;
		}
		
		/**
		 * Performs a data modifying query, like INSERT, UPDATE.
		 * @param args Arguments to the query.
		 * @return Number of records created or found to be modified.
		 */
		public int update(final Stream<Object> args) {
			return perform(t -> t.update(st -> applyQueryWithArgs(st, args)));
		}
		
		/**
		 * Performs a modifying query in batches.
		 * @param batchArgs Streams providing the arguments for each query execution.
		 * @param batchSize Maximum size of one batch.
		 * @return Affected number of rows for each query execution.
		 */
		public int[] updateBatch(final Collection<Stream<Object>> batchArgs, final int batchSize) {
			return perform(t -> Arrays.stream(t.batchUpdate(applyQuery(), batchArgs, batchSize, this::applyArgs)).flatMapToInt(Arrays::stream)).toArray();
		}
		
		/**
		 * Performs a data modifying query, optionally throwing an exception if no record was found to be modified.
		 * @param args Arguments to the query.
		 * @param onZero Supplier of the exception.
		 * @return Number of records modified.
		 */
		public int updateOrThrow(final Stream<Object> args, @Nullable final Supplier<PersistenceException> onZero) {
			final int count = update(args); 
			if (count == 0 && onZero != null) {
				throw onZero.get();
			}
			return count;
		}
		
		@Override
		public int hashCode() {
			return query.hashCode();
		}
		
		@Override
		public boolean equals(@Nullable final Object obj) {
			return query.equals(obj != null ? obj.toString() : null);
		}

		@Override
		public String toString() {
			return query;
		}
	}
	
	/**
	 * Builder for a sequence of fields with arguments to be used in an INSERT or UPDATE query.
	 */
	protected static class FieldBuilder {
		protected final Stream.Builder<String> fieldNames = Stream.builder();
		protected final Stream.Builder<Consumer<QueryBuilder>> fieldParams = Stream.builder();
		
		private static class ArgumentListAppender<S, T> extends IterativeAction<S, T> {
			public ArgumentListAppender(final S subject, final BiConsumer<S, String> appender, final BiConsumer<S, T> action) {
				super(subject, (i, a) -> {
					if (! i.isFirst()) {
						appender.accept(i.subject(), ", ");
					}
					action.accept(i.subject(), a);
				});
			}
		}
		
		public FieldBuilder() {
			/* for instantiation in anonymous classes */
		}
		
		/**
		 * Adds a field to the builder.
		 * @param name Name of the field in the table.
		 * @param parameter Delegate to add the statement for the filed value.
		 * @return This builder.
		 */
		public FieldBuilder add(final String name, final Consumer<QueryBuilder> parameter) {
			fieldNames.accept(name);
			fieldParams.accept(parameter);
			return this;
		}
		
		/**
		 * Adds a field to the builder unless the provided parameter Supplier returns {@code null}.
		 * @param name Name of the field in the table.
		 * @param parameterSupplier Supplier of a delegate to add the statement for the filed value.
		 * @return This builder.
		 */
		public FieldBuilder add(final String name, final Supplier<Consumer<QueryBuilder>> parameterSupplier) {
			final Consumer<QueryBuilder> parameter = parameterSupplier.get();
			if (parameter != null) {
				add(name, parameter);
			}
			return this;
		}

		/**
		 * Adds a field to the builder.
		 * @param name Name of the field in the table.
		 * @param parameter Parameter specification (SQL with ?) of the field. If this is {@code null} the value will be added to the query string in plain
		 *                  without an argument. This is intended for special cases like predefined values but must never be used for user definable input.
		 * @param value Object constituting the value of the field.
		 * @return This builder.
		 */
		public FieldBuilder add(final String name, final String parameter, @Nullable final Object value) {
			return add(name, q -> q.append(parameter, value));
		}

		/**
		 * Adds a field to the builder if the provided value is not {@code null}.
		 * @param name Name of the field in the table.
		 * @param parameter Parameter specification (SQL with ?) of the field. If this is {@code null} the value will be added to the query string in plain
		 *                  without an argument. This is intended for special cases like predefined values but must never be used for user definable input. 
		 * @param value Object constituting the value of the field.
		 * @return This builder.
		 */
		public FieldBuilder addNonNull(@Nullable final Object value, final String name, final String parameter) {
			if (value != null) {
				return add(name, parameter, value);
			}
			return this;
		}
		
		/**
		 * Adds a field depending on a Definable to the builder.
		 * @param <T> Type of the field value.
		 * @param field Definable providing the value for the field.
		 * @param name Name of the field in the table.
		 * @param parameter Parameter specification (SQL with ?) of the field.
		 * @return This builder.
		 */
		public <T> FieldBuilder add(final Definable<T> field, final String name, final String parameter) {
			if (field.isDefined()) {
				add(name, parameter, field.get());
			}
			return this;
		}

		/**
		 * Adds a field depending on a Definable to the builder, allowing to specify a conversion of the value.
		 * @param <T> Type of the input value.
		 * @param <R> Type of the field value.
		 * @param field Definable providing the value for the field.
		 * @param name Name of the field in the table.
		 * @param parameter Parameter specification (SQL with ?) of the field.
		 * @param conversion Function converting the value to the proper type to use it as query argument.
		 * @return This builder.
		 */
		public <T, R> FieldBuilder add(final Definable<T> field, final String name, final String parameter, final Function<T, R> conversion) {
			if (field.isDefined()) {
				add(name, parameter, conversion.apply(field.get()));
			}
			return this;
		}

		/**
		 * Adds a field depending on a Definable to the builder.
		 * @param <T> Type of the input value.
		 * @param field Definable providing the value for the field.
		 * @param name Name of the field in the table.
		 * @param parameter Delegate to add the statement for the field value.
		 * @return This builder.
		 */
		public <T> FieldBuilder add(final Definable<T> field, final String name, final Function<T, Consumer<QueryBuilder>> parameter) {
			if (field.isDefined()) {
				add(name, q -> q.with(parameter.apply(field.get())));
			}
			return this;
		}
		
		/**
		 * Adds an array field to be assigned from a Collection which may be a {@link DiffSet} allowing to add or remove certain entries from an existing array. 
		 * @param name Name of the array field in the table.
		 * @param type Data type of the array elements.
		 * @param values Collection of array values. Unless this is a DiffSet its entries replace any existing.
		 * @param forceOverwrite Treat an existing array as empty even if values are provided in a DiffSet.
		 * @return This builder.
		 */
		public FieldBuilder add(final String name, final PgType type, @Nullable final Collection<?> values, final boolean forceOverwrite) {
			final var merge = values instanceof DiffSet ? (DiffSet<?>) values : null;
			if (forceOverwrite) {
				add(name, q -> q.append("?").addArg(type, merge == null ? values : merge.getAdditions()));
			} else {
				add(name, q -> q.append("array_set_merge(CASE WHEN ? THEN null ELSE ").append(name).append(" END, ?, ?)", merge == null)
						.addArg(type, merge != null ? merge.getAdditions() : values)
						.addArg(type, merge != null ? merge.getDeletions() : null));
			}
			return this;
		}
		
		/**
		 * Adds an array field to be optionally assigned from a Collection which may be a {@link DiffSet}
		 * allowing to add or remove certain entries from an existing array. 
		 * @param field Definable providing the Collection of array values. Unless this is a DiffSet its entries replace any existing.
		 * @param name Name of the array field in the table.
		 * @param type Data type of the array elements.
		 * @param forceOverwrite Treat an existing array as empty even if values are provided in a DiffSet.
		 * @return This builder.
		 */
		public FieldBuilder add(final Definable<? extends Collection<?>> field, final String name, final PgType type, final boolean forceOverwrite) {
			if (field.isDefined()) {
				add(name, type, field.get(), forceOverwrite);
			}
			return this;
		}

		/**
		 * Delegate this builder.
		 * @param builder Sub-builder.
		 * @return This builder.
		 */
		public FieldBuilder and(final Consumer<FieldBuilder> builder) {
			builder.accept(this);
			return this;
		}

		/**
		 * Adds the defined field names separated by {@code ', '} to a string.
		 * @param string Builder for the string.
		 * @return Number of fields.
		 */
		public int buildColumns(final StringBuilder string) {
			final ArgumentListAppender<StringBuilder, String> columns = new ArgumentListAppender<>(string, StringBuilder::append, StringBuilder::append);
			fieldNames.build().forEach(columns::accept);
			return columns.getCount();
		}

		/**
		 * Adds the defined field parameters separated by {@code ', '} to a query.
		 * @param builder QueryBuilder
		 */
		public void buildFields(final QueryBuilder builder) {
			final ArgumentListAppender<QueryBuilder, Consumer<QueryBuilder>> insertValues = new ArgumentListAppender<>(builder,
					QueryBuilder::append, (q, c) -> c.accept(q));
			fieldParams.build().forEach(insertValues::accept);
		}

		/**
		 * Adds the defined fields to a query in the form {@code ([name], ...) VALUES ([parameter], ...) }.
		 * @param builder Builder of the query.
		 * @return Number of fields added.
		 */
		public int buildInsert(final QueryBuilder builder) {
			return buildUpsert(builder, (Set<String>) null);
		}

		/**
		 * Adds the defined fields to a query in the form {@code ([name], ...) VALUES ([parameter], ...) }.
		 * @param builder Builder of the query.
		 * @param upsertKeyColumns Optional column names, forming to a constraint, a conflict on which shall result in a respective UPDATE operation.
		 * @return Number of fields added.
		 */
		public int buildUpsert(final QueryBuilder builder, final String... upsertKeyColumns) {
			return buildUpsert(builder, new LinkedHashSet<>(Arrays.asList(upsertKeyColumns)));
		}

		/**
		 * Adds the defined fields to a query in the form {@code ([name], ...) VALUES ([parameter], ...) }.
		 * @param builder Builder of the query.
		 * @param upsertKey Optional Set of column names (usually the primary key), forming a constraint, a conflict on which shall result
		 *                  in an UPDATE operation of all other defined columns or NOTHING if the Set is empty or no other columns are defined.
		 * @return Number of fields added.
		 */
		public int buildUpsert(final QueryBuilder builder, @Nullable final Set<String> upsertKey) {
			final CountingSteamBuilder<String> upsertColumns = new CountingSteamBuilder<>();
			builder.append("(");
			final ArgumentListAppender<QueryBuilder, String> insertColumns = new ArgumentListAppender<>(builder, QueryBuilder::append, QueryBuilder::append);
			fieldNames.build().forEach(column -> {
				insertColumns.accept(column);
				if (upsertKey != null && ! upsertKey.contains(column)) {
					upsertColumns.accept(column);
				}
			});

			builder.append(") VALUES (");
			buildFields(builder);
			builder.append(")");

			if (upsertKey != null) {
				builder.append(" ON CONFLICT ");
				if (! upsertKey.isEmpty()) {
					builder.append("(");
					final ArgumentListAppender<QueryBuilder, String> conflictColumns = new ArgumentListAppender<>(builder,
							QueryBuilder::append, QueryBuilder::append);
					upsertKey.forEach(conflictColumns::accept);
					builder.append(") ");
				}
				if (upsertKey.isEmpty() || upsertColumns.isEmpty()) {
					builder.append("DO NOTHING");
				} else {
					builder.append("DO UPDATE SET ");
					final ArgumentListAppender<QueryBuilder, String> upsertAssignments = new ArgumentListAppender<>(builder, QueryBuilder::append, (q, s) -> {
						q.append(s);
						q.append(" = EXCLUDED.");
						q.append(s);
					});
					upsertColumns.build().forEach(upsertAssignments);
				}
			}

			return insertColumns.getCount();
		}

		/**
		 * Adds the defined fields to a query in the form {@code [name] = [parameter], ... }.
		 * @param builder Builder of the query.
		 * @return Number of fields added.
		 */
		public int buildUpdate(final QueryBuilder builder) {
			final Iterator<String> iterNames = fieldNames.build().iterator();
			final Iterator<Consumer<QueryBuilder>> iterParams = fieldParams.build().iterator();
			final ArgumentListAppender<QueryBuilder, Consumer<QueryBuilder>> updateAssignments = new ArgumentListAppender<>(builder,
					QueryBuilder::append, (q, c) -> c.accept(q));
			while (iterNames.hasNext()) {
				updateAssignments.accept(q -> {
					q.append(iterNames.next());
					q.append(" = ");
					iterParams.next().accept(q);
				});
			}
			return updateAssignments.getCount();
		}
	}

	/**
	 * Aggregates a sequence of clauses and applies them to QueryBuilder.
	 */
	protected static class ClauseStreamBuilder implements ConditionalConsumer<QueryBuilder> {
		protected final Stream.Builder<ConditionalConsumer<QueryBuilder>> stream;
		protected final BiConsumer<IterativeAction<QueryBuilder, Consumer<QueryBuilder>>, QueryBuilder> before;
		protected final BiConsumer<IterativeAction<QueryBuilder, Consumer<QueryBuilder>>, QueryBuilder> after;
		protected boolean empty;
		
		/**
		 * Creates a new builder for a sequence of clauses to be iteratively applied to a query.
		 * The {@code before} and {@code after} operations allow to add things like opening and closing brackets, commas or other operators between clauses.
		 * They may query the Iteration object to detect the position in the sequence.
		 * @param before Operation to perform before each clause.
		 * @param after Operation to perform after each clause.
		 */
		protected ClauseStreamBuilder(final BiConsumer<IterativeAction<QueryBuilder, Consumer<QueryBuilder>>, QueryBuilder> before,
				final BiConsumer<IterativeAction<QueryBuilder, Consumer<QueryBuilder>>, QueryBuilder> after) {
			this.stream = Stream.builder();
			this.before = before;
			this.after = after;
			this.empty = true;
		}
		
		@Override
		public void accept(@Nullable final QueryBuilder t) {
			if (t != null) {
				build(t);
			}
		}
		
		@Override
		public boolean isApplicable() {
			return ! isEmpty();
		}
		
		/**
		 * Adds another clause to the sequence of this builder.
		 * @param clause Operation building the clause.
		 */
		public void accept(final ConditionalConsumer<QueryBuilder> clause) {
			stream.accept(clause);
			empty = false;
		}
		
		private int build(final QueryBuilder q, final boolean initialFirst) {
			final IterativeAction<QueryBuilder, Consumer<QueryBuilder>> iteration = new IterativeAction<>(q, (i, clause) -> i.subject()
					.with(iq -> before.accept(i, iq)).with(clause).with(iq -> after.accept(i, iq)), initialFirst);
			stream.build().filter(ConditionalConsumer::isApplicable).forEachOrdered(iteration);
			return iteration.getCount();
		}
		
		/**
		 * Finalizes the sequence of clauses and applies them to a query.
		 * @param q QueryBuilder to apply the clauses to.
		 * @return The number of causes processed.
		 */
		public int build(final QueryBuilder q) {
			return build(q, true);
		}
		
		/**
		 * Finalizes the sequence of clauses and applies them to a query expecting that the sequence has already been started.
		 * @param q QueryBuilder to apply the clauses to.
		 * @return The number of causes processed.
		 */
		public int buildSubsequent(final QueryBuilder q) {
			return build(q, false);
		}
		
		/**
		 * @return {@code true} if no clauses have been added to this builder.
		 */
		public boolean isEmpty() {
			return empty;
		}
	}

	/**
	 * Aggregates a sequence of clauses to be added to a query in the form {@code WHERE ([clause]) AND ([clause]) AND ...}
	 * or {@code WHERE ([clause]) OR ([clause]) OR ...}
	 */
	protected static class FilterStreamBuilder extends ClauseStreamBuilder {
		public FilterStreamBuilder(final boolean anding, final boolean nested) {
			super((i, q) -> {
				if (i.isFirst()) {
					q.append(nested ? "(" : " WHERE (");
				} else {
					q.append(anding ? " AND (" : " OR (");
				}
			}, (i, q) -> q.append(")"));
		}
		
		public FilterStreamBuilder(final boolean anding) {
			this(anding, false);
		}
		
		public FilterStreamBuilder() {
			this(true);
		}
	}
	
	/**
	 * Aggregates a sequence of clauses to be added to a query in the form {@code ORDER BY [clause], [clause], ...}
	 */
	protected static class OrderStreamBuilder extends ClauseStreamBuilder {
		public OrderStreamBuilder() {
			super((i, q) -> q.append(i.isFirst() ? " ORDER BY " : ", "), (i, q) -> {});
		}
	}

	/**
	 * Aggregates a sequence of clauses to be added to a query in the form {@code ORDER BY [clause], [clause], ...}
	 */
	protected static class GroupStreamBuilder extends ClauseStreamBuilder {
		public GroupStreamBuilder() {
			super((i, q) -> q.append(i.isFirst() ? " GROUP BY " : ", "), (i, q) -> {});
		}
	}
	
	/**
	 * Manages a set of columns dynamically added to a query.
	 */
	protected static class DynamicSelection {
		private static final String REF = "_dyn_";
		
		private final Map<String, BiConsumer<QueryBuilder.ColumnMapper<Map<String, Object>>, Integer>> clauses;
		private final Map<String, Integer> columns;
		private int index;
		
		@Nullable
		private QueryBuilder.ColumnMapper<Map<String, Object>> mapper;
		
		public DynamicSelection() {
			this.clauses = new LinkedHashMap<>();
			this.columns = new HashMap<>();
		}
		
		/**
		 * Defines a dynamic column.
		 * @param name Name for the property (used for mapping the result, does not appear on the query string).
		 * @param clause Clause for selecting the value.
		 * @param extractor Function mapping the column from the query result to an object.
		 */
		public void putColumn(final String name, final Consumer<QueryBuilder> clause, final FieldExtractor<Object> extractor) {
			clauses.put(Objects.requireNonNull(name, "Field name must not be null"),
				(m, i) -> m.appendColumns(
					n -> (rs, map) -> map.put(name, extractor.get(rs, n)),
					q -> {
						q.appendColumn("(");
						clause.accept(q);
						q.append(") " + REF + i);
					}
				)
			);
		}
		
		/**
		 * Returns the field reference for a previously added column.
		 * @param name Name of the dynamic property.
		 * @return Field reference for addressing the column on the query.
		 */
		public String getReference(final String name) {
			if (mapper == null) {
				throw new IllegalStateException("Cannot retrieve dynamic field reference before selection has been applied");
			}
			return REF + Objects.requireNonNull(columns.get(name), () -> "Field " + name + " not in selection");
		}
		
		/**
		 * Applies all defined dynamic columns to a query.
		 * @param q QueryBuilder to apply column clauses to.
		 */
		public void apply(final QueryBuilder q) {
			if (mapper != null) {
				throw new IllegalStateException("Selection already applied");
			}
			final var m = q.new ColumnMapper<Map<String, Object>>();
			for (final var e : clauses.entrySet()) {
				e.getValue().accept(m, ++index);
				columns.put(e.getKey(), index);
			}
			this.mapper = m;
		}
		
		/**
		 * Extracts the values for dynamic properties from a result-set.
		 * @param rs Query result.
		 * @return Map of properties and values.
		 * @throws SQLException m In case of an error accessing the result-set.
		 */
		public Map<String, Object> extract(final ResultSet rs) throws SQLException {
			return Objects.requireNonNull(mapper, "Selection has not been applied")
				.map(rs, new LinkedHashMap<>());
		}

		/**
		 * @return {@code true} if no dynamic selection was specified. Otherwise {@code false}.
		 */
		public boolean isEmpty() {
			return clauses.isEmpty() && columns.isEmpty();
		}
	}
	
	/**
	 * Creates a new query builder.
	 * @return Builder for the query string and its attributes.
	 */
	protected QueryBuilder query() {
		return new QueryBuilder(new StringBuilder());
	}
	
	/**
	 * Creates a new query builder.
	 * @param s Starting string of the query, usually SELECT, INSERT, etc.
	 * @return Builder to further extend the query string and its attributes.
	 */
	protected QueryBuilder query(final String s) {
		return new QueryBuilder(new StringBuilder(s));
	}
	
	/**
	 * Creates a new query builder.
	 * @param builder Function to prepare the query.
	 * @return Prepared builder of the query string and its attributes.
	 */
	protected QueryBuilder query(final BuildingConsumer<QueryBuilder> builder) {
		return builder.prepare(query());
	}
	
	/**
	 * Performs queries for a set of entity specifications, in batches if their arguments match.
	 * @param <T> Type of the batch elements.
	 * @param entries All batch elements.
	 * @param batchSize Maximum number of entries to process in a single database operation.
	 *                  If this is smaller than the total number of elements, this function still only needs to be called once to process them all.
	 * @param builder Function creating the query and parameter set for each entry in the batch.
	 *                For batch processing to be efficient most entries should produce the same query.
	 * @return Affected number of rows per execution for each query.
	 */
	protected static <T> int[][] queryBatch(final Collection<T> entries, final int batchSize, final Function<T, QueryBuilder> builder) {
		final HashMap<JdbcContext, Stream.Builder<Stream<Object>>> qBatch = new HashMap<>();
		int n = 0;
		for (final var entry : entries) {
			try {
				final QueryBuilder q = builder.apply(entry);
				qBatch.computeIfAbsent(q.build(), k -> Stream.<Stream<Object>>builder()).accept(q.args.stream());
				n++;
			} catch (final Exception e) {
				throw new IllegalArgumentException("Error processing batch entry #" + n, e);
			}
		}
		
		LOG.debug("Processing " + n + " entries in " + qBatch.size() + " batches");
		
		n = 0;
		final var result = new int[qBatch.size()][];
		for (final var batch : qBatch.entrySet()) {
			result[n++] = batch.getKey().updateBatch(batch.getValue().build().toList(), batchSize);
		}
		return result;
	}
	
}
