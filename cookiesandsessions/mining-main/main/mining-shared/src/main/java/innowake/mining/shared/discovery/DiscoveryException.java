/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery;

/**
 * This exception is used, whenever an error happens during the discovery process.
 */
public class DiscoveryException extends Exception {

	/**
	 * Public constructor.
	 *
	 * @param message the message; not {@code null}
	 */
	public DiscoveryException(final String message) {
		super(message);
	}

	/**
	 * Public constructor.
	 *
	 * @param message the message; not {@code null}
	 * @param cause the cause or {@code null}
	 */
	public DiscoveryException(final String message, final Throwable cause) {
		super(message, cause);
	}

}
