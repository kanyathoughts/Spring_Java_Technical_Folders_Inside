/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.lang.ref.SoftReference;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import innowake.lib.core.api.lang.Nullable;

/**
 * Contains binary data that may be interpreted as a Unicode String.
 */
public class BinaryString {
	
	private final byte[] content;
	@Nullable
	private SoftReference<String> stringRef;
	
	public static final BinaryString EMPTY = new BinaryString(BinaryValue.EMPTY_ARRAY) {
		@Override
		public String toString() {
			return "";
		}
	};
	
	public BinaryString(final byte[] content) {
		this.content = content;
	}

	@JsonCreator
	public BinaryString(final String content) {
		this(content.getBytes(StandardCharsets.UTF_8));
	}
	
	public byte[] get() {
		return content;
	}
	
	public boolean isEmpty() {
		return content.length == 0;
	}
	
	@Override
	public boolean equals(@Nullable final Object other) {
		if (super.equals(other)) {
			return true;
		}
		if (other instanceof BinaryString) {
			return Arrays.equals(this.content, ((BinaryString) other).content);
		}
		if (other instanceof byte[]) {
			return Arrays.equals(this.content, ((byte[]) other));
		}
		if (other instanceof String) {
			return this.toString().equals(other); 
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return Arrays.hashCode(content);
	}
	
	@JsonValue
	@Override
	public String toString() {
		String str = stringRef != null ? stringRef.get() : null;
		if (str == null) {
			str = new String(content, StandardCharsets.UTF_8);
			stringRef = new SoftReference<>(str);
		}
		return str;
	}
	
}
