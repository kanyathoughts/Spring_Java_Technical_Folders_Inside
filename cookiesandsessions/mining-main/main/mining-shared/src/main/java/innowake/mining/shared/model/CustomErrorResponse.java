/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model;

import java.time.LocalDateTime;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import innowake.lib.core.api.lang.Nullable;

/**
 * Error response class used in ExceptionHandler.
 */
public class CustomErrorResponse {

	@JsonDeserialize(using = FlexibleLocalDateTimeDeserializer.class)
	private LocalDateTime timestamp = LocalDateTime.now();
	
	private int status;
	
	@Nullable
	private String error;
	
	@Nullable
	private String message;
	
	@Nullable
	private String path;

	/**
	 * Gets the time stamp.
	 *
	 * @return the time stamp
	 */
	public LocalDateTime getTimestamp() {
		return timestamp;
	}
	
	/**
	 * Sets the time stamp.
	 *
	 * @param timestamp the time stamp
	 */
	public void setTimestamp(final LocalDateTime timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status
	 */
	public void setStatus(final int status) {
		this.status = status;
	}

	/**
	 * Gets the error.
	 *
	 * @return the error
	 */
	@Nullable
	public String getError() {
		return error;
	}

	/**
	 * Sets the error.
	 *
	 * @param error the error
	 */
	public void setError(final String error) {
		this.error = error;
	}

	/**
	 * Gets the message.
	 *
	 * @return the message
	 */
	@Nullable
	public String getMessage() {
		return message;
	}

	/**
	 * Sets the message.
	 *
	 * @param message the message
	 */
	public void setMessage(@Nullable final String message) {
		this.message = message;
	}

	/**
	 * Gets the path.
	 *
	 * @return the path
	 */
	@Nullable
	public String getPath() {
		return path;
	}

	/**
	 * Sets the path.
	 *
	 * @param path the path
	 */
	public void setPath(final String path) {
		this.path = path;
	}
}
