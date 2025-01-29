/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.error;

import org.springframework.security.core.AuthenticationException;

/**
 * Mining specific authentication exception.
 */
public class MiningAuthenticationException extends AuthenticationException {

	/**
	 * Constructs an {@code MiningAuthenticationException} with the specified message.
	 *
	 * @param message the detail message
	 */
	public MiningAuthenticationException(final String message) {
		super(message);
	}

}
