/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.lang.ref.SoftReference;

import com.fasterxml.jackson.annotation.JsonCreator;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;

import com.fasterxml.jackson.annotation.JsonValue;

import innowake.lib.core.api.lang.Nullable;

/**
 * Wraps binary data for serialization to and from hexadecimal strings.
 */
public class BinaryValue {
	
	public static final byte[] EMPTY_ARRAY = new byte[0];
	
	public static final BinaryValue EMPTY = new BinaryValue(EMPTY_ARRAY) {
		@Override
		public String toString() {
			return "";
		}
	};
	
	private final byte[] data;
	@Nullable
	private SoftReference<String> stringRef;
	
	private static byte[] parseHex(final String data) {
		try {
			return Hex.decodeHex(data);
		} catch (DecoderException e) {
			throw new IllegalArgumentException(e);
		}
	}
	
	public BinaryValue(final byte[] data) {
		this.data = data;
	}

	@JsonCreator
	public BinaryValue(final String data) {
		this(parseHex(data)); 
	}
	
	public byte[] get() {
		return data;
	}
	
	public boolean isEmpty() {
		return data.length == 0;
	}
	
	@JsonValue
	@Override
	public String toString() {
		String str = stringRef != null ? stringRef.get() : null;
		if (str == null) {
			str = Hex.encodeHexString(data);
			stringRef = new SoftReference<>(str);
		}
		return str;
	}
}
