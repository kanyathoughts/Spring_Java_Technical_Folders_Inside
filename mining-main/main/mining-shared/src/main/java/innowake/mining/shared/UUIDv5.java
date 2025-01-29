/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.DigestException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;

/**
 * Implementation of the UUID Version 5 (hashed unique identifiers) generation as specified in RFC 4122.
 * Instances of this class reuse buffers for performance and are therefore not thread-safe.
 */
public class UUIDv5 {

	public enum Namespace {
		/* RFC 4122 */
		DNS  ("6ba7b810-9dad-11d1-80b4-00c04fd430c8"),
		URL  ("6ba7b811-9dad-11d1-80b4-00c04fd430c8"),
		OID  ("6ba7b812-9dad-11d1-80b4-00c04fd430c8"),
		X500 ("6ba7b814-9dad-11d1-80b4-00c04fd430c8");
		
		private final String id;
		
		private Namespace(final String id) {
			this.id = id;
		}
		
		public UUID uuid() {
			return UUID.fromString(id);
		}
	}
	
	private static final int UUID_SIZE = 16;
	
	private final ByteBuffer buffer;
	private final MessageDigest sha1;
	private final byte[] namespace;
	private final Charset charset;
	
	private static ByteBuffer toBytes(final UUID src, final ByteBuffer dst) {
		dst.putLong(0, src.getMostSignificantBits());
		dst.putLong(8, src.getLeastSignificantBits());
		return dst;
	}
	
	private static UUID fromBytes(final ByteBuffer src) {
		return new UUID(src.getLong(0), src.getLong(8));
	}
	
	public UUIDv5(final UUID namespace) {
		this(namespace, StandardCharsets.UTF_8);
	}
	
	public UUIDv5(final UUID namespace, final Charset charset) {
		try {
			sha1 = MessageDigest.getInstance("SHA-1");
		} catch (final NoSuchAlgorithmException e) {
			throw new IllegalStateException(e);
		}
		buffer = ByteBuffer.allocate(sha1.getDigestLength());
		this.namespace = toBytes(namespace, ByteBuffer.allocate(UUID_SIZE)).array();
		this.charset = charset;
	}
	
	public UUID generate(final byte[] data) {
		sha1.update(namespace);
		sha1.update(data);
		try {
			sha1.digest(buffer.array(), 0, sha1.getDigestLength());
		} catch (final DigestException e) {
			throw new IllegalStateException(e);
		}
		/* version */
		buffer.put(6, (byte)(buffer.get(6) & 0x0F | 0x50));
		/* variant */
		buffer.put(8, (byte)(buffer.get(8) & 0x3F | 0x80));
		return fromBytes(buffer);
	}
	
	public UUID generate(final String key) {
		return generate(key.getBytes(charset));
	}
	
	public UUID generate(final String entity, final String... id) {
		for (String s : id) {
			if (s == null) {
				throw new IllegalArgumentException("Entity ID contains null-String.");
			}
		}
		return generate(entity + ":" + String.join(";", id));
	}

}
