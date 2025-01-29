/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.io;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.function.DoublePredicate;
import java.util.function.IntPredicate;
import java.util.function.LongPredicate;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import innowake.lib.core.api.lang.Nullable;

/**
 * A {@link ZipInputStream} with security checks limiting size and number of entries.
 */
public class SecuredZipInputStream extends ZipInputStream {

	public static final int DEFAULT_MAX_ENTRIES = 1_000_000;
	public static final long DEFAULT_MAX_SIZE = 1_000_000_000L;
	public static final double DEFAULT_MAX_RATIO = 100;
	public static final boolean DEFAULT_REQUIRE_BOUNDS = false;
	
	public static class SecurityException extends IOException {
		private SecurityException(final String message) {
			super(message);
		}
	}

	private int maxEntries = DEFAULT_MAX_ENTRIES;
	private long maxSize = DEFAULT_MAX_SIZE;
	private double maxRatio = DEFAULT_MAX_RATIO;
	private boolean requireBounds = DEFAULT_REQUIRE_BOUNDS;

	private final IntPredicate exceedsMaxEntries = entries -> this.maxEntries > 0 && entries > this.maxEntries;
	private final LongPredicate exceedsMaxSize = size -> this.maxSize > 0 && size > this.maxSize;
	private final DoublePredicate exceedsMaxRatio = ratio -> this.maxRatio > 0 && ratio > this.maxRatio;

	@Nullable
	private ZipEntry currentEntry;

	private int totalEntryCount;
	private long currentByteCount;
	private long totalByteCount;

	/**
	 * @see ZipInputStream#ZipInputStream(InputStream, Charset)
	 */
	@SuppressWarnings("javadoc")
	public SecuredZipInputStream(final InputStream in, final Charset charset) {
		super(in, charset);
	}

	/**
	 * @see ZipInputStream#ZipInputStream(InputStream)
	 */
	@SuppressWarnings("javadoc")
	public SecuredZipInputStream(final InputStream in) {
		super(in);
	}

	@Override
	@Nullable
	public ZipEntry getNextEntry() throws IOException {
		ZipEntry currentEntry = super.getNextEntry(); 
		this.currentEntry = currentEntry; 
		currentByteCount = 0;
		
		if (currentEntry != null) {
			totalEntryCount++;
			
			if (exceedsMaxEntries.test(totalEntryCount)) {
				throw new SecurityException("Entry limit exceeded: " + totalEntryCount + ">" + maxEntries);
			}
			
			if (currentEntry.getSize() < 0 || currentEntry.getCompressedSize() < 0) {
				if (requireBounds) {
					throw new SecurityException("Archive entry does not indicate its size.");
				}
			} else {
				double ratio = currentEntry.getCompressedSize() == 0
						? (currentEntry.getSize() != 0 ? Double.POSITIVE_INFINITY : 1)
						: (double) currentEntry.getSize() / currentEntry.getCompressedSize(); 
				if (exceedsMaxRatio.test(ratio)) {
					throw new SecurityException("Archive entry exceeds compression ratio limit (" + ratio + ">" + maxRatio + ").");
				}
			}
		}
		
		return currentEntry;
	}

	@Override
	public int read(@Nullable final byte[] b, final int off, final int len) throws IOException {
		final ZipEntry currentEntry = this.currentEntry;
		if (currentEntry == null) {
			throw new IllegalStateException("No current archive entry.");
		}
		
		final int rd = super.read(b, off, len);
		if (rd > 0) {
			currentByteCount += rd;
			totalByteCount += rd;
		}
		if (currentEntry.getSize() >= 0 && currentByteCount > currentEntry.getSize()) {
			throw new SecurityException("Indicated size of archive entry exceeded (" + currentByteCount + ">" + currentEntry.getSize() + ").");
		}
		if (exceedsMaxSize.test(currentByteCount)) {
			throw new SecurityException("File size limit exceeded (" + currentByteCount + ">" + maxSize + ").");
		}
		if (exceedsMaxSize.test(totalByteCount)) {
			throw new SecurityException("Archive size limit exceeded (" + totalByteCount + ">" + maxSize + ").");
		}
		
		return rd;
	}

	@Override
	public void closeEntry() throws IOException {
		super.closeEntry();
		currentEntry = null;
	}

	/**
	 * Get the archive entry the stream is currently positioned on.
	 *
	 * @return The current entry or {@code null} if none. 
	 */
	@Nullable
	public ZipEntry getCurrentEntry() {
		return currentEntry;
	}

	/**
	 * Set the limit for entries in the archive.
	 * Defaults to {@value #DEFAULT_MAX_ENTRIES}.
	 *
	 * @param maxEntries The maximum number of entries to be read.
	 * @return {@code this}
	 */
	public SecuredZipInputStream limitEntries(final int maxEntries) {
		this.maxEntries = maxEntries;
		return this;
	}

	/**
	 * Set the limit for the total size of the archive.
	 * Defaults to {@value #DEFAULT_MAX_SIZE} bytes.
	 *
	 * @param maxSize The maximum number of uncompressed bytes that may be read.
	 * @return {@code this}
	 */
	public SecuredZipInputStream limitSize(final long maxSize) {
		this.maxSize = maxSize;
		return this;
	}

	/**
	 * Set the limit for the compression ratio per entry.
	 * Defaults to {@value #DEFAULT_MAX_RATIO}.
	 *
	 * @param maxRatio The maximum original to compressed size ratio permitted.
	 * @return {@code this}
	 */
	public SecuredZipInputStream limitRatio(final double maxRatio) {
		this.maxRatio = maxRatio;
		return this;
	}

	/**
	 * Define if archive entries which do not specify their size are permitted.
	 * Defaults to {@value #DEFAULT_REQUIRE_BOUNDS}.
	 *
	 * @param requireBounds If an entry must specify its size.
	 * @return {@code this}
	 */
	public SecuredZipInputStream setRequireBounds(final boolean requireBounds) {
		this.requireBounds = requireBounds;
		return this;
	}

}
