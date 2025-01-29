/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.access;

import java.util.NoSuchElementException;
import java.util.Objects;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;

/**
 * Specification for retrieving a subset of a query result.
 */
public class Pagination {
	
	private static final int DEFAULT_PAGE_SIZE = (Integer.MAX_VALUE - 1) / 2;  /* for legacy compatibility */
	
	/** Limits a query result to a single record. To be used with {@link Paged.Builder#first()}. */
	public static final Pagination FIRST = new Pagination(1);
	/** Specifies a page with a single record. To be used with {@link Paged.Builder#single()}. */
	public static final Pagination SINGLE = new Pagination(0l, 1);
	
	private final int size;
	private final long offset;
	private final boolean subset;
	
	/**
	 * Create a result limiting specification.
	 * @param limit Maximum number of elements to retrieve.
	 */
	public Pagination(final int limit) {
		this(0l, limit, true);
	}
	
	/**
	 * Create a pagination specification.
	 * @param size Maximum number of elements on the page.
	 * @param offset Number of elements to skip before the start of this page.
	 */
	public Pagination(final long offset, final int size) {
		this(offset, size, false);
	}
	
	private Pagination(final long offset, final int size, final boolean subset) {
		if (offset < 0) {
			throw new IllegalArgumentException("Pagination offset cannot be negative.");
		}
		if (size < 0) {
			throw new IllegalArgumentException("Pagination size cannot be negative.");
		}
		this.size = size == 0 ? DEFAULT_PAGE_SIZE : size;
		this.offset = offset;
		this.subset = subset;
	}
	
	/**
	 * Creates a pagination specification for an aligned page.
	 * @param page Number of the page.
	 * @param size Size of each page.
	 * @return Corresponding specification.
	 */
	public static Pagination at(final long page, final int size) {
		return new Pagination(page * size, size);
	}
	
	/**
	 * Creates a specification for a subset of elements not intended for paging.
	 * @param offset Number of elements to skip before the first to retrieve.
	 * @param limit Maximum number of elements to retrieve.
	 * @return Corresponding specification.
	 */
	public static Pagination subset(final long offset, final int limit) {
		return new Pagination(offset, limit, true);
	}
	
	/**
	 * Gets the page size.
	 * @return Maximum number of elements on the page.
	 */
	public Integer getSize() {
		return size;
	}
	
	/**
	 * Gets the page offset.
	 * @return Number of elements before the start of this page.
	 */
	public Long getOffset() {
		return offset;
	}
	
	/**
	 * Gets the zero-based page number of this pagination specification.
	 * @return Number of pages preceding the current one.
	 */
	public long getPage() {
		return offset / size + (isAligned() ? 0 : 1);
	}
	
	/**
	 * Gets the pagination specification for the specified page with the current size.
	 * @param n Number of the possibly unaligned page.
	 * @return Corresponding specification.
	 */
	public Pagination getPage(final long n) {
		final boolean aligned = isAligned();
		if (n == 0 && ! aligned) {
			return new Pagination((int) offset);
		}
		return new Pagination(size * (n - (aligned ? 0 : 1)) + offset % size, size);
	}
	
	/**
	 * Gets the pagination specification for the page at the specified multiple of the current size.
	 * @param n Zero-based number of the page.
	 * @return Corresponding specification.
	 */
	public Pagination getAlignedPage(final long n) {
		return new Pagination(size * n, size);
	}
	
	/**
	 * Gets the pagination specification for the next page.
	 * @return Specification for the page following the current one.
	 */
	public Pagination nextPage() {
		return new Pagination(offset + size, size);
	}
	
	/**
	 * Gets the pagination specification for the previous page.
	 * @return Specification for the page preceding the current one.
	 */
	public Pagination prevPage() {
		if (offset == 0) {
			throw new NoSuchElementException();
		}
		if (offset < size) {
			return new Pagination((int) offset);
		}
		return new Pagination(offset - size, size);
	}
	
	/**
	 * Tells whether the page offset is an even multiple of the page size.
	 * @return If the current page is aligned with equally sized pages.
	 */
	public boolean isAligned() {
		return offset % size == 0;
	}
	
	/**
	 * Determines whether this defines merely a record range but no actual pagination is intended.
	 * @return If a page ({@code false}) or just a subset of rows ({@code true}) is specified.
	 */
	public boolean isSubset() {
		return subset;
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof Pagination)) {
			return false;
		}
		final Pagination that = (Pagination) o;
		return size == that.size && offset == that.offset && subset == that.subset;
	}

	@Override
	public int hashCode() {
		return Objects.hash(size, offset, subset);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
			.append("offset", offset)
			.append("size", size)
			.append("page", getPage())
			.append("aligned", isAligned())
			.toString();
	}
	
}
