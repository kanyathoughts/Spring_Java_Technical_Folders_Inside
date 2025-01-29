/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.hashing;

import java.math.BigInteger;

/**
 * Custom Class for Base62 encoder
 */
class Base62Encoder {

	private static final char[] ALPHANUMERIC = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toCharArray();
	
	private Base62Encoder() { }

	/**
	 * Encoding the data based on custom base62 encoder algorithm
	 *
	 * @param data the input to encode
	 * @return the encoding result
	 */
	public static String encode(final byte[] data) {
		BigInteger value = new BigInteger(1, data);
		final StringBuilder result = new StringBuilder();

		while (value.compareTo(BigInteger.ZERO) > 0) {
			final int reminder = value.mod(BigInteger.valueOf(62)).intValue();
			result.insert(0, ALPHANUMERIC[reminder]);
			value = value.divide(BigInteger.valueOf(62));
		}
		return result.toString();
	}
}
