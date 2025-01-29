/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Utility to convert filter values of type Object into the required target types.
 */
public class FilterUtil {

	private FilterUtil() {
		/* no instances allowed */
	}
	
	/**
	 * Converts the specified {@code value} into an {@link EntityId}. If {@code value} is not already an instance of {@link EntityId}, this method utilizes 
	 * {@link EntityId#of(Long)} or {@link EntityId#of(String)} to convert {@code value} into an {@link EntityId}.
	 *
	 * @param value the value to convert
	 * @param field name of the field for which the value is converted for error handling
	 * @return the {@link EntityId} value
	 * @throws IllegalArgumentException if {@code value} is not a valid {@link EntityId}
	 */
	public static EntityId toEntityId(final Object value, final String field) {
		try {
			if (value instanceof EntityId) {
				return (EntityId) value;
			}
			if (value instanceof Long) {
				return EntityId.of((Long) value);
			}

			return EntityId.of(value.toString());
		} catch (final Exception e) {
			throw new IllegalArgumentException(field + ": " + value + " is not supported. " + field + " must be an EntityId, Long or UUID string.");
		}
	}

	/**
	 * Converts the specified {@code value} into a list. If {@code value} is not already an instance of {@link Collection}, this method utilizes 
	 * {@link List#of()} to create a list containing the provided {@code value}.
	 *
	 * @param value the value to convert
	 * @return the {@link List}
	 */
	@SuppressWarnings("unchecked")
	public static Collection<Object> toCollection(final Object value) {
		if (value instanceof Collection) {
			return (Collection<Object>) value;
		}

		return List.of(value);
	}

	/**
	 * Converts the specified {@code value} into a list of strings. If {@code value} is not already an instance of {@link Collection}, this method utilizes 
	 * {@link List#of()} to create a list containing the provided {@code value}.
	 * <p>This method uses {@link Object#toString()} to convert all elements into strings.</p>
	 *
	 * @param value the value to convert
	 * @return the {@link List} of strings
	 */
	public static List<String> toStrings(final Object value) {
		return toCollection(value).stream().map(Object::toString).collect(Collectors.toList());
	}

	/**
	 * Converts the specified {@code value} into a list of {@link EntityId EntityIds}. If {@code value} is not already an instance of {@link Collection},
	 * this method utilizes {@link List#of()} to create a list containing the provided {@code value}.
	 * <p>This method uses {@link EntityId#of(Long)} or {@link EntityId#of(String)} to convert list entries, that are not already an instance of 
	 * {@link EntityId}.</p>
	 *
	 * @param value the value to convert
	 * @param field name of the field for which the value is converted for error handling
	 * @return the {@link EntityId} value
	 * @throws IllegalArgumentException if {@code value} is not a valid {@link EntityId}
	 */
	public static List<EntityId> toEntityIds(final Object value, final String field) {
		final var collection = toCollection(value);
		final List<EntityId> result = new ArrayList<>(collection.size());
		collection.forEach(entry -> {
			if (entry instanceof String && entry.toString().indexOf(',') != -1) {
				for (final var s : entry.toString().split(",")) {
					result.add(toEntityId(s, field));
				}
			} else {
				result.add(toEntityId(entry, field));
			}
		});

		return result;
	}

	/**
	 * Converts the specified {@code value} into a {@link Number}. If {@code value} is not already an instance of {@link Number}, this method utilizes 
	 * {@link Integer#valueOf(String)} to convert {@code value} into an {@link Integer}.
	 *
	 * @param value the value to convert
	 * @param field name of the field for which the value is converted for error handling
	 * @return the {@link Number} value
	 * @throws IllegalArgumentException if {@code value} is not a number (string)
	 */
	public static Number toNumber(final Object value, final String field) {
		if (value instanceof Number) {
			return (Number) value;
		}

		try {
			return Integer.valueOf(value.toString());
		} catch (final Exception e) {
			throw new IllegalArgumentException(field + ": " + value + " is not supported. " + field + " must be number or number string.");
		}
	}

	/**
	 * Converts the specified {@code value} into a {@link Boolean}. If {@code value} is not already an instance of {@link Boolean}, this method utilizes 
	 * {@link Boolean#valueOf(String)} to convert {@code value} into an {@link Boolean}.
	 *
	 * @param value the value to convert
	 * @return the {@link Boolean} value
	 */
	public static Boolean toBoolean(final Object value) {
		return value instanceof Boolean ? (Boolean) value : Boolean.valueOf(value.toString());
	}

	/**
	 * Converts the specified {@code value} into an {@link UUID}. If {@code value} is not already an instance of {@link UUID}, this method utilizes 
	 * {@link UUID#fromString(String)} to convert {@code value} into an {@link UUID}.
	 *
	 * @param value the value to convert
	 * @param field name of the field for which the value is converted for error handling
	 * @return the {@link UUID} value
	 * @throws IllegalArgumentException if {@code value} is not a valid {@link EntityId}
	 */
	public static UUID toUuid(final Object value, final String field) {
		try {
			if (value instanceof UUID) {
				return (UUID) value;
			}

			return UUID.fromString(value.toString());
		} catch (final Exception e) {
			throw new IllegalArgumentException(field + ": " + value + " is not supported. " + field + " must be an UUID or UUID string.");
		}
	}
	
	/**
	 * Converts the specified {@code value} into a list of {@link UUID UUIDs}. If {@code value} is not already an instance of {@link Collection},
	 * this method utilizes {@link List#of()} to create a list containing the provided {@code value}.
	 * <p>This method uses {@link UUID#fromString(String)} to convert list entries, that are not already an instance of {@link UUID}.</p>
	 *
	 * @param value the value to convert
	 * @param field name of the field for which the value is converted for error handling
	 * @return the {@link EntityId} value
	 * @throws IllegalArgumentException if {@code value} is not a valid {@link EntityId}
	 */
	public static List<UUID> toUuids(final Object value, final String field) {
		final var collection = toCollection(value);
		final List<UUID> result = new ArrayList<>(collection.size());
		collection.forEach(entry -> {
			if (entry instanceof String && entry.toString().indexOf(',') != -1) {
				for (final var s : entry.toString().split(",")) {
					result.add(toUuid(s, field));
				}
			} else {
				result.add(toUuid(entry, field));
			}
		});

		return result;
	}

	/**
	 * Converts the specified {@code value} into an {@link Instant}. If {@code value} is not already an instance of {@link Instant}, this method utilizes 
	 * {@link Instant#parse(CharSequence)} to convert {@code value} into an {@link Instant}.
	 *
	 * @param value the value to convert
	 * @return the {@link UUID} value
	 * @throws IllegalArgumentException if {@code value} is not a valid {@link EntityId}
	 */
	public static Instant toInstant(final Object value) {
		return value instanceof Instant ? (Instant) value : Instant.parse(value.toString());
	}
}
