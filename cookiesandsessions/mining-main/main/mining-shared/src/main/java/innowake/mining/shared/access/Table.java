/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.access;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.UnaryOperator;

import innowake.lib.core.api.lang.Nullable;

/**
 * Generic container for unmodifiable tabular data.
 */
public class Table implements List<Table.Row> {

	@Nullable
	private LinkedHashMap<String, Integer> columns;
	@Nullable
	private ArrayList<Object[]> rows;
	
	public interface FieldConverter extends UnaryOperator<Object> { }
	
	/**
	 * Converts a JDBC result to a {@link Table} object, optionally applying field specific conversions. 
	 */
	public static class Builder {
		private final Map<String, FieldConverter> fieldConverters;
		
		public Builder(final Map<String, FieldConverter> fieldConverters) {
			this.fieldConverters = fieldConverters;
		}
		
		public Builder() {
			this(Collections.emptyMap());
		}
		
		@Nullable
		protected Object defaultConversion(@Nullable final Object obj) {
			return obj;
		}
		
		public Table extract(final ResultSet rs) throws SQLException {
			final ResultSetMetaData meta = rs.getMetaData();
			final int cols = meta.getColumnCount();
			
			final Table table = new Table();
			LinkedHashMap<String, Integer> columns = table.columns = new LinkedHashMap<>(cols);
			final FieldConverter[] conv = new FieldConverter[cols];
			for (int col = 0; col < cols; col++) {
				final String field = meta.getColumnName(col + 1);
				columns.put(field, col);
				if (! fieldConverters.isEmpty()) {
					final FieldConverter fieldConv = fieldConverters.get(field);
					if (fieldConv != null) {
						conv[col] = fieldConv;
					}
				}
			}
			
			ArrayList<Object[]> rows = table.rows = new ArrayList<>();
			while (rs.next()) {
				final Object[] row = new Object[cols];
				for (int col = 0; col < cols; col++) {
					final Object o = rs.getObject(col + 1);
					row[col] = conv[col] != null ? conv[col].apply(o) : defaultConversion(o);
				}
				rows.add(row);
			}
			
			return table;
		}
	}
	
	private LinkedHashMap<String, Integer> columnsNotNull() {
		return Objects.requireNonNull(this.columns);
	}
	
	private ArrayList<Object[]> rowsNotNull() {
		return Objects.requireNonNull(this.rows);
	}
	
	public class Row implements Map<String, Object> {
		
		private final EntrySet entries;

		public class EntrySet implements Set<Entry<String, Object>> {
			
			private final Object[] row;
			
			private EntrySet(final Object[] row) {
				this.row = row;
			}
			
			private Object value(final int index) {
				return row[index];
			}
			
			public class ColumnIterator implements Iterator<Entry<String, Object>> {
				private final Iterator<Entry<String, Integer>> cols = columnsNotNull().entrySet().iterator();
				
				@Override
				public boolean hasNext() {
					return cols.hasNext();
				}

				@Override
				public Entry<String, Object> next() {
					final Entry<String, Integer> col = cols.next();  
					return new Entry<String, Object>() {
						@Override
						public String getKey() {
							return col.getKey();
						}
						
						@Override
						public Object getValue() {
							return value(col.getValue());
						}
						
						@Override
						public Object setValue(final @Nullable Object value) {
							throw new IllegalAccessError();
						}
					};
				}
			}
			
			@Override
			public boolean add(final @Nullable Entry<String, Object> e) {
				throw new IllegalAccessError();
			}
			
			@Override
			public boolean addAll(final @Nullable Collection<? extends Entry<String, Object>> c) {
				throw new IllegalAccessError();
			}
			
			@Override
			public void clear() {
				throw new IllegalAccessError();
			}
			
			@Override
			public boolean contains(final @Nullable Object o) {
				throw new IllegalAccessError();
			}
			
			@Override
			public boolean containsAll(final @Nullable Collection<?> c) {
				throw new IllegalAccessError();
			}
			
			@Override
			public boolean isEmpty() {
				return rowsNotNull().isEmpty();
			}
			
			@Override
			public java.util.Iterator<Entry<String, Object>> iterator() {
				return new ColumnIterator();
			}
			
			@Override
			public boolean remove(final @Nullable Object o) {
				throw new IllegalAccessError();
			}
			
			@Override
			public boolean removeAll(final @Nullable Collection<?> c) {
				throw new IllegalAccessError();
			}
			
			@Override
			public boolean retainAll(final @Nullable Collection<?> c) {
				throw new IllegalAccessError();
			}
			
			@Override
			public int size() {
				return columnsNotNull().size();
			}
			
			@Override
			public Object[] toArray() {
				throw new IllegalAccessError();
			}
			
			@Override
			public <T> T[] toArray(@Nullable final T[] a) {
				throw new IllegalAccessError();
			}
			
		}
		
		private Row(final int row) {
			this.entries = new EntrySet(rowsNotNull().get(row));
		}
		
		@Override
		public void clear() {
			throw new IllegalAccessError();
		}
		
		@Override
		public boolean containsKey(final @Nullable Object key) {
			return columnsNotNull().containsKey(key);
		}
		
		@Override
		public boolean containsValue(final @Nullable Object value) {
			throw new IllegalAccessError();
		}
		
		@Override
		public Set<Entry<String, Object>> entrySet() {
			return entries;
		}
		
		/**
		 * Returns the value in the current row for the column to which the specified key is mapped,
		 * or {@code null} if this table contains no column for the key. 
		 * This is the default method specified by {@link Map#get(Object)}.
		 * Consider using the more specific {@link #getNullable(String)} or {@link #getNonNull(String)}.
		 */
		@Override
		@Nullable
		public Object get(final @Nullable Object key) {
			final Integer col = columnsNotNull().get(key);
			if (col != null) {
				return entries.value(col);
			}
			return null;
		}
		
		/**
		 * Returns the value of a table field allowing it to be {@code null}.
		 * @param <T> Type of the value to retrieve.
		 * @param column Name of the table column.
		 * @return Value for the column in the current row, or {@code null} if the column does not exist or the field is set to {@code null}.
		 */
		@Nullable
		@SuppressWarnings("unchecked")
		public <T> T getNullable(final @Nullable String column) {
			return (T) get(column);
		}
		
		/**
		 * Returns the value of a table field which must not be {@code null}.
		 * @param <T> Type of the value to retrieve.
		 * @param column Name of the table column.
		 * @return Value for the column in the current row.
		 * @throws IllegalArgumentException If the field is {@code null} or the column does not exist.
		 */
		@SuppressWarnings("unchecked")
		public <T> T getNonNull(final @Nullable String column) {
			final var val = get(column);
			if (val == null) {
				throw new IllegalArgumentException("Value must not be null for column: " + column);
			}
			return (T) val;
		}
		
		/**
		 * Returns the value of a table field as an Optional.
		 * @param <T> Type of the value to retrieve.
		 * @param column Name of the table column.
		 * @return Optional that holds the value for the column in the current row or is empty if the value is {@code null} or the column does not exist.
		 */
		public <T> Optional<T> getOptional(final @Nullable String column) {
			return Optional.ofNullable(getNullable(column));
		}
		
		@Override
		public boolean isEmpty() {
			return rowsNotNull().isEmpty();
		}
		
		@Override
		public Set<String> keySet() {
			return columns();
		}
		
		@Override
		public Object put(final @Nullable String key, final @Nullable Object value) {
			throw new IllegalAccessError();
		}
		
		@Override
		public void putAll(final @Nullable Map<? extends String, ? extends Object> o) {
			throw new IllegalAccessError();
		}
		
		@Override
		public Object remove(final @Nullable Object key) {
			throw new IllegalAccessError();
		}
		
		@Override
		public int size() {
			return columnsNotNull().size();
		}
		
		@Override
		public Collection<Object> values() {
			return Arrays.asList(entries.row);
		}
		
		/**
		 * @return Array of field values for each column in the current row.
		 */
		public Object[] toArray() {
			return entries.row.clone();
		}
	}
	
	public class TableIterator implements ListIterator<Row> {
		
		private int row;
		
		private TableIterator() {
			this.row = -1;
		}
		
		private TableIterator(final int row) {
			this.row = row;
		}
		
		@Override
		public void add(final @Nullable Row e) {
			throw new IllegalAccessError();
		}
		
		@Override
		public boolean hasNext() {
			return row + 1 < rowsNotNull().size();
		}
		
		@Override
		public boolean hasPrevious() {
			return row > 0;
		}
		
		@Override
		public Row next() {
			if (hasNext()) {
				row = nextIndex();
				return new Row(row);
			}
			throw new NoSuchElementException();
		}
		
		@Override
		public int nextIndex() {
			return row + 1;
		}
		
		@Override
		@Nullable
		public Row previous() {
			if (hasPrevious()) {
				row = previousIndex();
				return new Row(row);
			}
			throw new NoSuchElementException();
		}
		
		@Override
		public int previousIndex() {
			if (row < 0) {
				return -1;
			}
			return row - 1;
		}
		
		@Override
		public void remove() {
			throw new IllegalAccessError();
		}

		@Override
		public void set(final @Nullable Row e) {
			throw new IllegalAccessError();
		}
		
	}
	
	public Set<String> columns() {
		return Collections.unmodifiableSet(columnsNotNull().keySet());
	}
	
	@Override
	public boolean add(final @Nullable Row o) {
		throw new IllegalAccessError();
	}

	@Override
	public void add(final int index, final @Nullable Row o) {
		throw new IllegalAccessError();
	}

	@Override
	public boolean addAll(final @Nullable Collection<? extends Row> c) {
		throw new IllegalAccessError();
	}

	@Override
	public boolean addAll(final int index, final @Nullable Collection<? extends Row> c) {
		throw new IllegalAccessError();
	}

	@Override
	public void clear() {
		throw new IllegalAccessError();
	}

	@Override
	public boolean contains(final @Nullable Object o) {
		throw new IllegalAccessError();
	}

	@Override
	public boolean containsAll(final @Nullable Collection<?> c) {
		throw new IllegalAccessError();
	}

	@Override
	public Row get(final int index) {
		return new Row(index);
	}

	@Override
	public int indexOf(final @Nullable Object o) {
		throw new IllegalAccessError();
	}

	@Override
	public boolean isEmpty() {
		return rowsNotNull().isEmpty();
	}
	
	@Override
	public TableIterator iterator() {
		return (TableIterator) listIterator();
	}
	
	@Override
	public int lastIndexOf(final @Nullable Object o) {
		throw new IllegalAccessError();
	}
	
	@Override
	public ListIterator<Row> listIterator() {
		return new TableIterator();
	}
	
	@Override
	public ListIterator<Row> listIterator(final int index) {
		return new TableIterator(index);
	}
	
	@Override
	public boolean remove(final @Nullable Object o) {
		throw new IllegalAccessError();
	}
	
	@Override
	public Row remove(final int o) {
		throw new IllegalAccessError();
	}
	
	@Override
	public boolean removeAll(final @Nullable Collection<?> c) {
		throw new IllegalAccessError();
	}
	
	@Override
	public boolean retainAll(final @Nullable Collection<?> c) {
		throw new IllegalAccessError();
	}
	
	@Override
	public Row set(final int index, @Nullable final Row o) {
		throw new IllegalAccessError();
	}
	
	@Override
	public int size() {
		return rowsNotNull().size();
	}
	
	@Override
	public List<Row> subList(final int fromIndex, final int toIndex) {
		throw new IllegalAccessError();
	}
	
	@Override
	public Object[] toArray() {
		throw new IllegalAccessError();
	}
	
	@Override
	public <T> T[] toArray(@Nullable T[] a) {
		throw new IllegalAccessError();
	}
	
}
