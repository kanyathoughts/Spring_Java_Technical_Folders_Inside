/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.persistence;

import innowake.lib.core.api.lang.Nullable;

import java.io.Serializable;
import java.util.Optional;

/**
 * Provides information on the outcome of an import operation into persistent storage.
 * 
 * @param <TKey> type used for primary keys
 */
public class ImportResult<TKey> implements Serializable {

	public enum Status {
		CREATED(true),
		UPDATED(true),
		AMBIGUOUS_MATCH(false),
		DB_ERROR(false),
		ERROR(false);

		private final boolean success;

		private Status(final boolean success) {
			this.success = success;
		}

		public boolean isSuccess() {
			return success;
		}
	}

	@Nullable
	private final TKey key;
	private final Status status;
	@Nullable
	private final String message;
	@Nullable
	private final Throwable cause;

	private ImportResult(@Nullable final TKey key, final Status status, @Nullable final String message, @Nullable final Throwable cause) {
		this.key = key;
		this.status = status;
		this.message = message;
		this.cause = cause;
	}

	/**
	 * Creates an ImportResult to be returned when a new entity was created successfully.
	 *
	 * @param key the key of the created entity
	 * @param <TKey> the type of the key
	 * @return the ImportResult
	 */
	public static <TKey> ImportResult<TKey> forSuccessfulCreation(final TKey key) {
		return new ImportResult<>(key, Status.CREATED, null, null);
	}

	/**
	 * Creates an ImportResult to be returned when an existing entity was updated successfully.
	 *
	 * @param key the key of the updated entity
	 * @param <TKey> the type of the key
	 * @return the ImportResult
	 */
	public static <TKey> ImportResult<TKey> forSuccessfulUpdate(final TKey key) {
		return new ImportResult<>(key, Status.UPDATED, null, null);
	}

	/**
	 * Creates an ImportResult to be returned when the import failed because the module
	 * that needed to be created or updated could not be identified.
	 *
	 * @param message a message explaining while matching the target module has failed
	 * @param <TKey> the type of the key
	 * @return the ImportResult
	 */
	public static <TKey> ImportResult<TKey> forAmbiguousMatch(final String message) {
		return new ImportResult<>(null, Status.AMBIGUOUS_MATCH, message, null);
	}

	/**
	 * Creates an ImportResult to be returned when the import failed because of an unhandled database error.
	 *
	 * @param cause the cause of the import error
	 * @param <TKey> the type of the key
	 * @return the ImportResult
	 */
	public static <TKey> ImportResult<TKey> forDbError(final Throwable cause) {
		return new ImportResult<>(null, Status.DB_ERROR, null, cause);
	}

	/**
	 * Creates an ImportResult to be returned when the import failed because of an unhandled database error.
	 *
	 * @param message an error message
	 * @param cause the cause of the import error
	 * @param <TKey> the type of the key
	 * @return the ImportResult
	 */
	public static <TKey> ImportResult<TKey> forDbError(final String message, final Throwable cause) {
		return new ImportResult<>(null, Status.DB_ERROR, message, cause);
	}

	/**
	 * Creates an ImportResult to be returned when the import failed because of an error.
	 *
	 * @param message an error message
	 * @param cause the cause of the import error, if available
	 * @param <TKey> the type of the key
	 * @return the ImportResult
	 */
	public static <TKey> ImportResult<TKey> forError(final String message, @Nullable final Throwable cause) {
		return new ImportResult<>(null, Status.ERROR, message, null);
	}

	/**
	 * Returns the id of the entity that was created or updated.
	 *
	 * @return the id
	 */
	public Optional<TKey> getKey() {
		return Optional.ofNullable(key);
	}

	public Status getStatus() {
		return status;
	}

	public boolean isSuccess() {
		return status.isSuccess();
	}

	public Optional<String> getMessage() {
		return Optional.ofNullable(message);
	}

	public Optional<Throwable> getCause() {
		return Optional.ofNullable(cause);
	}

	@Override
	public String toString() {
		return "ImportResult{" +
				"key=" + key +
				", status=" + status +
				", message='" + message + '\'' +
				", cause=" + cause +
				'}';
	}
}
