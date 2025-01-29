/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Array;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.postgresql.util.PGobject;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.PgDao.QueryBuilder;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.CachingSupplier;
import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.access.Table;

/**
 * Static utility methods for accessing Postgres.
 */
public class PgUtil {
	
	public static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];

	protected PgUtil() { }
	
	/**
	 * Converts back-ticks to double-quotes.
	 * @param query String containing back-ticks.
	 * @return String with back-ticks replaced by double-quotes.
	 */
	public static String pgEscape(final String query) {
		return query.replace('`', '"');
	}
	
	/**
	 * Converts a search pattern for using it with LIKE or ILIKE using {@code %} for {@code *} and {@code _} for {@code ?}.
	 * @param pattern Search string.
	 * @return Converted string.
	 */
	public static String pgPattern(final String pattern) {
		return pattern.replace('*', '%').replace('?', '_');
	}
	
	/**
	 * Replaces any NUL character (\0) in a String with the corresponding control picture (&#x2400;).
	 * @param str Input String.
	 * @return String without NUL characters.
	 */
	public static String replaceNULs(final String str) {
		return str.replace('\0', '\u2400');
	}
	
	/**
	 * Replaces any NUL control picture (&#x2400;) in a String with an actual NUL character (\0).
	 * @param str Input String.
	 * @return String possibly containing NUL characters.
	 */
	public static String restoreNULs(final String str) {
		return str.replace('\u2400', '\0');
	}
	
	/**
	 * Converts an arbitrary type of nullable input value to a probably different type of non-nullable output.
	 * @param <T> Input type.
	 * @param <R> Output type.
	 * @param input Value to convert which may be {@code null}.
	 * @param mapper Function performing the conversion that must produce a result even if the input is {@code null}.
	 * @return The mapped value.
	 */
	public static <T, R> R map(@Nullable final T input, final Function<T, R> mapper) {
		return Objects.requireNonNull(mapper.apply(input), "Mapping function must return a value.");
	}
	
	/**
	 * Converts an arbitrary type of nullable input value to a probably different type of nullable output.
	 * @param <T> Input type.
	 * @param <R> Output type.
	 * @param input Value to convert which may be {@code null}.
	 * @param mapper Function performing the conversion that is only called if the input is not {@code null} while it may return {@code null}.
	 * @return The mapped value, possibly {@code null} if the conversion function returns {@code null} and always if the input is {@code null}.
	 */
	@Nullable
	public static <T, R> R mapNullable(@Nullable final T input, final Function<T, R> mapper) {
		if (input == null) {
			return null;
		}
		return mapper.apply(input);
	}
	
	/**
	 * Converts any JSON field or SQL array to the corresponding basic Java type.
	 * @param object Input as retrieved from a query result using {@code getObject()}.
	 * @return A Map, List, Number or String, or the original object (possibly {@code null}) if it is neither a PgObject nor an SQL array. 
	 */
	@Nullable
	public static Object mapAnyJSONorArray(@Nullable final Object object) {
		if (object instanceof final PGobject pgObj) {
			if (PgJSON.PG_TYPE_JSONB.equals(pgObj.getType())) {
				return PgJSON.fromPGobject(pgObj, Object.class);
			}
		} else if (object instanceof final java.sql.Array array) {
			try {
				return streamArray(array).toList();
			} catch (final SQLException e) {
				throw new IllegalStateException(e);
			}
		}
		return object;
	}
	
	/**
	 * Builder for a {@link Table} with default conversion for JSON and Array types (see {@link PgUtil#mapAnyJSONorArray(Object)}).
	 */
	public static class TableBuilder extends Table.Builder {
		private static final TableBuilder DEFAULT_BUILDER = new TableBuilder();
		
		private TableBuilder() {
			super();
		}
		
		public TableBuilder(final Map<String, Table.FieldConverter> fieldConverters) {
			super(fieldConverters);
		}
		
		@Override
		@Nullable
		protected Object defaultConversion(@Nullable final Object obj) {
			return mapAnyJSONorArray(obj);
		}
		
		public static Table build(final ResultSet rs) throws SQLException {
			return DEFAULT_BUILDER.extract(rs);
		}
	}
	
	/**
	 * Provides a wrapping Function returning {@code null} for {@code null}, to be used with conversion Functions
	 * (like {@link java.sql.Timestamp#from(java.time.Instant)} that cannot handle a {@code null} argument.
	 * @param <T> Input type.
	 * @param <R> Output type.
	 * @param mapper Function that is called only if the input is not {@code null}.
	 * @return A {@code null}-safe conversion Function.
	 */
	public static <T, R> Function<T, R> passNull(final Function<T, R> mapper) {
		return arg -> mapNullable(arg, mapper);
	}
	
	public static <T extends Collection<?>> Optional<T> optionalCollection(@Nullable final T col) {
		if (col == null || col.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(col);
	}
	
	public static <T extends Map<?, ?>> Optional<T> optionalMap(@Nullable final T map) {
		if (map == null || map.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(map);
	}
	
	public static Optional<String> optionalString(@Nullable final String str) {
		if (str == null || str.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(str);
	}
	
	/**
	 * Performs an unchecked cast of an object to a Collection of any type.
	 * @param <T> Type of the Collection.
	 * @param obj Object to cast.
	 * @return Object as Collection.
	 */
	@SuppressWarnings("unchecked")
	@Nullable
	public static <T> Collection<T> collection(@Nullable final Object obj) {
		return (Collection<T>) obj;
	}
	
	/**
	 * Creates a {@link PgArray} from any Collection.
	 * @param type SQL type of the Objects in the Collection.
	 * @param c Collection providing the Array content.
	 * @return {@link PgArray} wrapping the Collection, or {@code null} if the Collection is {@code null}.
	 */
	@Nullable
	public static PgArray arrayFromCollection(final PgType type, @Nullable final Collection<?> c) {
		return c != null ? new PgArray(type.toString(), c.toArray(EMPTY_OBJECT_ARRAY)) : null;
	}
	
	/**
	 * Creates a {@link PgArray} from any Collection, returning {@code null} in case the Collection is empty.
	 * @param type SQL type of the Objects in the Collection.
	 * @param c Collection providing the Array content.
	 * @return {@link PgArray} wrapping the Collection, or {@code null}.
	 */
	@Nullable
	public static PgArray arrayFromCollectionNonEmpty(final PgType type, @Nullable final Collection<?> c) {
		return c == null || c.isEmpty() ? null : arrayFromCollection(type, c);
	}
	
	/**
	 * Creates a {@link PgArray} from any Collection, returning an empty Array in case the Collection is {@code null}.
	 * @param type SQL type of the Objects in the Collection.
	 * @param c Collection providing the Array content.
	 * @return {@link PgArray} wrapping the Collection.
	 */
	public static PgArray arrayFromCollectionNonNull(final PgType type, @Nullable final Collection<?> c) {
		final PgArray a = arrayFromCollection(type, c);
		return a != null ? a : new PgArray(type.toString(), EMPTY_OBJECT_ARRAY);
	}
	
	/**
	 * Creates a stream from a {@link java.sql.Array}.
	 * The array may contain {@code null} elements which are passed to the Stream.
	 * If the array itself is {@code null} an empty Stream will be returned.
	 * @param <T> Element type.
	 * @param array Array from query result.
	 * @return Stream containing the array elements.
	 * @throws SQLException In case of an error accessing the ResultSet.
	 */
	@SuppressWarnings("unchecked")
	public static <T> Stream<T> streamArray(@Nullable final Array array) throws SQLException {
		final Stream.Builder<T> stream = Stream.builder();
		if (array != null) {
			try (final ResultSet rs = array.getResultSet()) {
				while (rs.next()) {
					stream.accept((T) rs.getObject(2));
				}
			}
		}
		return stream.build();
	}
	
	/**
	 * Maps attributes of objects in a Collection to separate collections for each attribute.
	 * This can be used with {@link QueryBuilder#addArgs(Collection[], PgType...)}
	 * for passing table-like arguments to queries which may unpivot them again using {@code unnest()}.
	 * <pre>
	 * <-                            /             Array             \
	 *                        / Collection \ | / Collection \ |     ...
	 *              / Object:   Function(X)  |   Function(X)
	 * ->  Iterable | Object:   Function(X)  |   Function(X)
	 *              \              ...
	 * </pre>
	 * @param <T> Type of input objects.
	 * @param rows Collection of input objects.
	 * @param columns Functions extracting the values for each output Collection from the input object.
	 * @return Array of Collections which, at the same offset, contain the values of one input object.
	 */
	@SafeVarargs
	public static <T> Collection<Object>[] pivot(final Iterable<T> rows, final Function<T, ?>... columns) {
		@SuppressWarnings("unchecked")
		final ArrayList<Object>[] result = new ArrayList[columns.length];
		for (int n = 0; n < result.length; n++) {
			result[n] = new ArrayList<>();
		}
		for (final T value : rows) {
			for (int n = 0; n < result.length; n++) {
				result[n].add(columns[n].apply(value));
			}
		}
		return result;
	}
	
	/**
	 * Turns a set of SQL arrays representing columns of a sub-table into a Stream of arrays constituting rows
	 * which contain the values from each of the original arrays. Arrays may be nested requiring further unpivoting, see {@link #unpivot(Array...)}.
	 * <pre>
	 * ->                      / ResultSet \
	 *                     Array | Array | ...
	 *           / Array:    X   |   X
	 * <- Stream | Array:    X   |   X 
	 *           \ ...
	 * </pre>
	 * Note that when aggregating rows from an outer join the implicit NULL values from when no record exists will be contained in the column arrays.
	 * In that case you may have to do pivoting conditionally, e.g. depending on a count.
	 * @param rs ResultSet to extract the arrays from.
	 * @param columns Indices of the array columns in the ResultSet. 
	 *                Negative indices indicate a column at the respective positive index that contains another array rather than a value,
	 *                see {@link #unpivot(Array[], boolean[])}.
	 * @return Stream of arrays for each set of values in the input arrays.
	 */
	public static Stream<Object[]> unpivot(final ResultSet rs, final int... columns) {
		final Array[] arrays = new Array[columns.length];
		final boolean[] nested = new boolean[columns.length];
		for (int n = 0; n < columns.length; n++) {
			try {
				arrays[n] = rs.getArray(Math.abs(columns[n]));
			} catch (final SQLException e) {
				throw new IllegalArgumentException(e);
			}
			nested[n] = columns[n] < 0;
		}
		return unpivot(arrays, nested);
	}
	
	/**
	 * Turns a set of SQL column arrays into a stream of row arrays. See {@link #unpivot(ResultSet, int...) }
	 * @param rs ResultSet to extract the arrays from.
	 * @param firstColumn Index (1 based) of the first column.
	 * @param columnCount Total number of columns to pivot.
	 * @return Stream of arrays for each set of values in the input arrays.
	 */
	public static Stream<Object[]> unpivotRange(final ResultSet rs, final int firstColumn, final int columnCount) {
		return unpivot(rs, IntStream.range(firstColumn, firstColumn + columnCount).toArray());
	}
	
	/**
	 * Turns a set of SQL column arrays into a stream of row arrays. See {@link #unpivot(ResultSet, int...)}.
	 * @param columns Arrays in which values at the same index constitute the column values for a data row.
	 * @return Stream of arrays for each set of values in the input arrays.
	 */
	public static Stream<Object[]> unpivot(final Array... columns) {
		return unpivot(columns, new boolean[columns.length]);
	}
	
	/**
	 * Turns a set of SQL column arrays into a stream of row arrays. See {@link #unpivot(ResultSet, int...)}.
	 * @param columns Arrays in which values at the same index constitute the column values for a data row.
	 * @param nested Specifies for each input array whether it should be read as an {@link Array} ({@code true})
	 *               rather than being converted to an {@code Object} ({@code false}). 
	 * @return Stream of arrays for each set of values in the input arrays.
	 */
	public static Stream<Object[]> unpivot(final Array[] columns, final boolean[] nested) {
		final Stream.Builder<Object[]> result = Stream.builder();
		final ResultSet[] arrays = new ResultSet[columns.length];
		for (int n = 0; n < columns.length; n++) {
			try {
				arrays[n] = columns[n] == null ? null : columns[n].getResultSet();
			} catch (final SQLException e) {
				throw new IllegalArgumentException(e);
			}
		}
		
		Object[] next;
		do {
			try {
				next = unpivotRow(arrays, nested);
			} catch (final SQLException e) {
				throw new IllegalArgumentException(e);
			}
			if (next != null) {
				result.accept(next);
			}
		} while (next != null);
		
		return result.build();
	}
	
	@Nullable
	private static Object[] unpivotRow(final ResultSet[] arrays, final boolean[] nested) throws SQLException {
		Object[] row = null;
		for (int n = 0; n < arrays.length; n++) {
			if (arrays[n] != null && arrays[n].next()) {
				if (row == null) {
					row = new Object[arrays.length];
				}
				row[n] = nested[n] ? arrays[n].getArray(2) : arrays[n].getObject(2);
			}
		}
		return row;
	}
	
	/**
	 * Creates a Supplier that either wraps a {@link CachingFunction}, if provided, or a {@link CachingSupplier}. 
	 * @param <K> Type of the key for the supplied value.
	 * @param <V> Type of the value.
	 * @param cache Access to a cache storing the value for the key.
	 * @param key Key for finding the value in the cache.
	 * @param loader Supplies the value when it is first requested.
	 * @return A cache wrapping or lazy-loading Supplier.
	 */
	protected static <K, V> Supplier<V> cachable(@Nullable final CachingFunction<K, V> cache, final K key, final Function<K, V> loader) {
		if (cache != null) {
			return () -> cache.apply(key, loader);
		}
		return new CachingSupplier<>(() -> loader.apply(key));
	}
	
	protected static <K, V> Supplier<List<V>> cachableBatch(@Nullable final CachingFunction<K, V> cache,
			final List<K> key, final Function<List<K>, List<V>> loader) {
		if (cache != null) {
			return () -> cache.apply(key, loader);
		}
		return new CachingSupplier<>(() -> loader.apply(key));
	}
	
	/**
	 * Creates a keyed Supplier that either wraps a {@link CachingFunction}, if provided, or a {@link CachingSupplier}. 
	 * @param <K> Type of the key for the supplied value.
	 * @param <V> Type of the value.
	 * @param cache Access to a cache storing the value for the key.
	 * @param key Key for finding the value in the cache.
	 * @param loader Supplies the value when it is first requested.
	 * @return A cache wrapping or lazy-loading Supplier.
	 */
	protected static <K, V> KeyedSupplier<K, V> cachableWithKey(@Nullable final CachingFunction<K, V> cache, final K key, final Function<K, V> loader) {
		return new KeyedSupplier<>(key, cachable(cache, key, loader));
	}
	
	protected static <K, V> KeyedSupplier<List<K>, List<V>> cachableKeyedBatch(@Nullable final CachingFunction<K, V> cache,
			final List<K> key, final Function<List<K>, List<V>> loader) {
		return new KeyedSupplier<>(key, cachableBatch(cache, key, loader));
	}
	
}
