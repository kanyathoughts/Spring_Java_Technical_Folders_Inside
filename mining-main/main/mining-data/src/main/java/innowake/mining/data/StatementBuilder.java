/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.jdbc.core.SqlTypeValue;
import org.springframework.jdbc.core.StatementCreatorUtils;

import com.google.common.collect.LinkedHashMultimap;
import com.orientechnologies.orient.core.id.ORecordId;

import innowake.lib.core.api.lang.Nullable;

/**
 * Helper class for working with SQL statement attributes 
 */
public class StatementBuilder {
	
	/** Used for attributes that have no value. When set the attribute is added to the {@link PreparedStatement} without setting a value for it. */
	public static final Object NO_ARGUMENT = new Object();

	final LinkedHashMultimap<String, Object> statements = LinkedHashMultimap.create();
	
	Optional<Clause> whereClause = Optional.empty();

	/**
	 * Returns whether this StatementBuilder contains any statements.
	 * @return {@code true} if no statements were added to the builder, else {@code false}
	 */
	public boolean isEmpty() {
		return statements.isEmpty();
	}

	/**
	 * Returns a comma separated String consisting of the the SQL statements added.
	 * <p> 
	 * The order is the insertion order.
	 *
	 * @return comma separated SQL statements
	 */
	public String attributes() {
		return String.join(",", statements.keySet());
	}
	
	/**
	 * Returns list of statement values.
	 *
	 * @return statement values
	 */
	public List<Object> values() {
		return new ArrayList<>(statements.values());
	}
	
	/**
	 * Initializes the given statement with the added values.
	 * <p>
	 * Currently supported value types are {@link String} and {@link Long}.
	 *
	 * @param statement the prepared statement to initialize
	 * @throws SQLException if a database error occurs
	 */
	public void init(final PreparedStatement statement) throws SQLException {
		int index = 1;
		for (final Object value : getValues()) {
			if (value == null) {
				StatementCreatorUtils.setParameterValue(statement, index, SqlTypeValue.TYPE_UNKNOWN, value);
			} else if (value == NO_ARGUMENT) {
				continue;
			} else if (value instanceof String) {
				statement.setString(index, (String) value);
			} else if (value instanceof Long) {
				statement.setLong(index, ((Long) value).longValue());
			} else if (value instanceof Double) {
				statement.setDouble(index, ((Double) value).doubleValue());
			} else if (value instanceof Integer) {
				statement.setInt(index, ((Integer) value).intValue());
			} else if (value instanceof Boolean) {
				statement.setBoolean(index, ((Boolean) value).booleanValue());
			} else if (value instanceof List) {
				statement.setObject(index, ((List<?>) value).toArray(new Object[0]));
			} else if (value instanceof Set) {
				statement.setObject(index, ((Set<?>) value).toArray(new Object[0]));
			} else if (value instanceof Map) {
				statement.setObject(index, value);
			} else if (value instanceof Object[]) {
				statement.setObject(index, value);
			} else if (value instanceof Float) {
				statement.setFloat(index, ((Float) value).floatValue());
			} else if (value instanceof ORecordId) {
				statement.setObject(index, value);
			} else if (value instanceof java.util.Date) {
				statement.setDate(index, new java.sql.Date(((java.util.Date) value).getTime()));
			} else if (value instanceof java.sql.Date) {
				statement.setDate(index, (java.sql.Date) value);
			} else {
				final String message = String.format("Unsupported value type '%s' for initialising the prepared statement.", value.getClass().getSimpleName());
				throw new IllegalStateException(message);
			}
			index++;
		}
	}

	private Collection<Object> getValues() {
		if ( ! hasClause()) {
			return statements.values();
		} else {
			return Stream.concat(statements.values().stream(), Arrays.stream(whereClause.get().args))
					.collect(Collectors.toList());
		}
	}

	/**
	 * Adds an attribute with a value of type {@link Long} if the value is not {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, @Nullable final Long value) {
		add(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with a value of type {@link Integer} if the value is not {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, @Nullable final Integer value) {
		add(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with a value of type {@link Integer}, which can also be {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttributeNullable(final String attribute, @Nullable final Integer value) {
		statements.put(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with a value of type {@link Long}, which can also be {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttributeNullable(final String attribute, @Nullable final Long value) {
		statements.put(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with a value of type {@link String}, which can also be {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttributeNullable(final String attribute, @Nullable final String value) {
		statements.put(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with multiple values for example when using conjunctions. 
	 * <p>
	 * {@code null} values are allowed
	 *
	 * @param attribute the SQL attribute to add
	 * @param args the values to add
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, final Object... args) {
		statements.putAll(attribute, Arrays.asList(args));
		return this;
	}
	
	/**
	 * Adds an attribute with a value of type {@link String} array if the value is not {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, final String[] value) {
		add(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with a value of type {@link String} if the value is not {@code null}.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, @Nullable final String value) {
		add(attribute, value);
		return this;
	}
	
	/**
	 * Adds an attribute with an optional value if the value is present.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, final Optional<?> value) {
		value.ifPresent(o -> add(attribute, o));
		return this;
	}

	/**
	 * Adds an attribute with a value of type {@link Map} if the value is not not null or empty.
	 *
	 * @param attribute the SQL attribute to add
	 * @param value the value of the attribute
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute, @Nullable final Map<?, ?> value) {
		if (value != null && ! value.isEmpty()) {
			statements.put(attribute, value);
		}
		return this;
	}
	
	/**
	 * Adds an attribute without a value.
	 *
	 * @param attribute the SQL attribute to add
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder addAttribute(final String attribute) {
		statements.put(attribute, NO_ARGUMENT);
		return this;
	}

	/**
	 * Adds a {@code WHERE} clause to the statement.
	 * 
	 * @param clause the {@code WHERE} clause with placeholders
	 * @param values the values assigned in the prepared statement
	 * @return the {@link StatementBuilder} for chaining
	 */
	public StatementBuilder where(final String clause, final Object... values) {
		whereClause = Optional.of(new Clause(clause, values));
		return this;
	}

	/**
	 * @return {@code true} if the statement has a {@code WHERE} clause, {@code false} otherwise
	 */
	public boolean hasClause() {
		return whereClause.isPresent();
	}

	/**
	 * clear content from the {@code statements} map.
	 */ 
	public void resetAttributes() {
		statements.clear();
	}
	/**
	 * Returns the {@code WHERE} clause if available.
	 * <p>
	 * Callers must first call {@link #hasClause()} and check if there's an actual clause available.
	 *
	 * @return the {@code WHERE} clause of the statement.
	 */
	public String getClause() {
		return whereClause.orElseThrow(() -> new IllegalStateException("No WHERE clause in statement present")).condition;
	}

	private void add(final String statement, @Nullable final Object value) {
		if (value != null) {
			statements.put(statement, value);
		}
	}

	private static class Clause {
		private final String condition;
		private final Object[] args;
		
		private Clause(final String condition, final Object[] args) {
			this.condition = condition;
			this.args = args;
		}
	}
}
