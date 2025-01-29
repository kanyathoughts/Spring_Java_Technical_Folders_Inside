/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.base.CaseFormat;
import org.postgresql.util.PGobject;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import innowake.lib.core.api.lang.Nullable;

/**
 * Utility class for handling JSON fields in Postgres queries.
 */
public class PgJSON {
	
	public static final String PG_TYPE_JSONB = PgType.JSONB.toString();
	
	private static final ObjectMapper json = new ObjectMapper()
			.registerModule(new Jdk8Module())
			.registerModule(new JavaTimeModule());

	private PgJSON() { }

	public static <T> T fromJSON(final String value, final Class<T> cls) {
		try {
			return json.readValue(value, cls);
		} catch (final JsonProcessingException e) {
			throw new IllegalArgumentException(e);
		}
	}

	public static <T> T fromJSON(final String value, final TypeReference<T> valueTypeRef) {
		try {
			return json.readValue(value, valueTypeRef);
		} catch (final JsonProcessingException e) {
			throw new IllegalArgumentException(e);
		}
	}

	protected static String toJSON(final Object value) {
		try {
			return json.writeValueAsString(value);
		} catch (final JsonProcessingException e) {
			throw new IllegalArgumentException(e);
		}
	}
	
	/**
	 * Converts a Postgres JSON field (as returned by {@link java.sql.ResultSet#getObject}) to a specific object. 
	 * @param obj JSON data wrapped in a {@link PGobject}.
	 * @param cls Class to convert to.
	 * @return Object deserialized from JSON data or {@code null} if the source field is {@code null}.
	 */
	@Nullable
	public static <T> T fromPGobjectOrNull(@Nullable final Object obj, final Class<T> cls) {
		if (obj != null) {
			final PGobject pgObj = (PGobject) obj;
			if (! PG_TYPE_JSONB.equals(pgObj.getType())) {
				throw new IllegalArgumentException("Not a " + PG_TYPE_JSONB + " field");
			}
			if (pgObj.getValue() != null) {
				return fromJSON(pgObj.getValue(), cls);
			}
		}
		return null;
	}
	
	/**
	 * Converts a Postgres JSON field (as returned by {@link java.sql.ResultSet#getObject}) to a specific object. 
	 * @param obj JSON data wrapped in a {@link PGobject}.
	 * @param cls Class to convert to.
	 * @return Objects deserialized from JSON data.
	 */
	public static <T> T fromPGobject(@Nullable final Object obj, final Class<T> cls) {
		final T result = fromPGobjectOrNull(obj, cls);
		if (result == null) {
			throw new IllegalArgumentException("JSON mapping resulted in null where a value is required");
		}
		return result;
	}
	
	/**
	 * Adds the elements from a Postgres JSON array field (as returned by {@link java.sql.ResultSet#getObject}) to a Collection.
	 * The field may be {@code null} in which case the unmodified input Collection is returned.
	 * Note that JSON objects may contain combinations of arbitrary objects and this function does not perform any type checking.
	 * @param obj JSON data wrapped in a {@link PGobject}.
	 * @param collection Collection to place the field's elements in.
	 * @return The provided Collection.
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Collection<U>, U> T fromPGobject(@Nullable final Object obj, final T collection) {
		final Collection<U> result = fromPGobjectOrNull(obj, Collection.class);
		if (result != null) {
			collection.addAll(result);
		}
		return collection;
	}
	
	/**
	 * Converts a Postgres JSON field (as returned by {@link java.sql.ResultSet#getObject}) to a {@link java.util.Map} object. 
	 * @param obj JSON data wrapped in a {@link PGobject}.
	 * @return Map of the JSON data.
	 */
	@SuppressWarnings("unchecked")
	public static Map<String, Object> fromPGobject(@Nullable final Object obj) {
		final Map<String, Object> map = fromPGobjectOrNull(obj, Map.class);
		return map == null ? Collections.emptyMap() : Collections.unmodifiableMap(map);
	}
	
	private static <T> Stream<T> fromPGobjectsStream(@Nullable final java.sql.Array array, final Class<T> cls) throws SQLException {
		if (array == null) {
			return Stream.empty();
		}
		final ResultSet arrayResult = array.getResultSet();
		final Stream.Builder<T> objs = Stream.builder();
		while (arrayResult.next()) {
			objs.accept(fromPGobjectOrNull(arrayResult.getObject(2), cls));
		}
		return objs.build();
	}
	
	/**
	 * Converts a Postgres JSON array field (retrieved using {@link java.sql.ResultSet#getArray}) to {@link java.util.Map} objects.
	 * @param array SQL array of {@link PGobject}s wrapping JSON data.
	 * @param cls Class to convert each element to.
	 * @return List of objects deserialized from JSON data. At least an empty List, if input is {@code null}.
	 * @throws SQLException In case of an error.
	 */
	public static <T> List<T> fromPGobjects(@Nullable final java.sql.Array array, final Class<T> cls) throws SQLException {
		return fromPGobjectsStream(array, cls).collect(Collectors.toList());
	}
	
	/**
	 * Converts a Postgres JSON array field (retrieved using {@link java.sql.ResultSet#getArray}) to {@link java.util.Map} objects.
	 * @param array SQL array of {@link PGobject}s wrapping JSON data.
	 * @return List of Maps of the JSON data. At least an empty List, if input is {@code null}.
	 * @throws SQLException In case of an error.
	 */
	@SuppressWarnings("unchecked")
	public static List<Map<String, Object>> fromPGobjects(@Nullable final java.sql.Array array) throws SQLException {
		return fromPGobjectsStream(array, Map.class).map(m -> (Map<String, Object>) m).collect(Collectors.toList());
	}
	
	/**
	 * Converts a raw JSON string to a {@link PGobject} to be used as a query argument.
	 * @param value Valid JSON string or {@code null}.
	 * @return {@link PGobject} wrapping the JSON data.
	 */
	public static PGobject toPGobjectFromString(@Nullable final String value) {
		final PGobject o = new PGobject();
		o.setType(PG_TYPE_JSONB);
		try {
			o.setValue(value);
		} catch (final SQLException e) {
			throw new IllegalArgumentException(e);
		}
		return o;
	}
	
	/**
	 * Converts a Java object to a Postgres JSON object.
	 * @param value Map to be converted to JSON.
	 * @return {@link PGobject} wrapping the JSON data.
	 */
	public static PGobject toPGobject(@Nullable final Object value) {
		return toPGobjectFromString(value != null ? toJSON(value) : null);
	}

	/**
	 * Converts a Java object to a Postgres JSON object with keys of lower camel case converted to snake case.
	 *
	 * @param value Map to be converted to JSON
	 * @return {@link PGobject} wrapping the JSON data
	 */
	public static PGobject toPGobjectConvertToSnakeCase(@Nullable final Object value) {
		final String jsonContent = value != null ? toJSON(value) : null;
		final String snakeCaseContent = CaseFormat.LOWER_CAMEL.converterTo(CaseFormat.LOWER_UNDERSCORE).convert(jsonContent);
		return toPGobjectFromString(snakeCaseContent);
	}
	
	/**
	 * Converts list of raw JSON strings to an array to be used as a query argument.
	 * @param values List of valid JSON strings. {@code null} is equivalent to an empty list.
	 * @return {@link PgArray} of {@link PGobject}s wrapping the JSON data.
	 */
	public static PgArray toPGobjectsFromStrings(@Nullable final List<String> values) {
		final PGobject[] objs;
		if (values == null) {
			objs = new PGobject[0];
		} else {
			objs = new PGobject[values.size()];
			for (int n = 0; n < values.size(); n++) {
				objs[n] = toPGobjectFromString(values.get(n));
			}
		}
		return new PgArray(PG_TYPE_JSONB, objs);
	}
	
	/**
	 * Converts a list of Java objects to an array of Postgres JSON objects.
	 * @param values List to be converted to JSON. {@code null} is equivalent to an empty list.
	 * @return {@link PgArray} of {@link PGobject}s wrapping the JSON data.
	 */
	public static PgArray toPGobjects(@Nullable final Collection<?> values) {
		return toPGobjectsFromStrings(values == null ? null : values.stream()
				.map(PgJSON::toJSON).collect(Collectors.toList()));
	}
	
}
