/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.error;

import org.springframework.http.HttpStatus;

/**
 * Exception thrown when the error message should be explicitly posted to the UI. Only use if the error message should be visible to the user.
 */
public class UserFacingException extends RuntimeException {

	private final HttpStatus status;

	/**
	 * Constructs a UserFacingException with the specified message.
	 *
	 * @param message the detailed message
	 */
	public UserFacingException(String message) {
		super(message);
		status = HttpStatus.INTERNAL_SERVER_ERROR;
	}
	public UserFacingException(HttpStatus status, String message) {
		super(message);
		this.status = status;
	}

	public HttpStatus getStatus() {
		return status;
	}

}
