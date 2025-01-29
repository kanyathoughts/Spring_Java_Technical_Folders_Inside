/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.access;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * Wrapper for the result of a paged query.
 * @param <T> Entity type queried.
 */
public class Paged<T> {
	
	public static final String ROW_NUMBER_COLUMN = "_row";
	public static final String ROW_COUNT_COLUMN = "_cnt";

	/**
	 * Builder for an optionally paged result of a query.
	 * A query may be unpaged (in which case only {@link #all()} or {@link #first()} may be called),
	 * a mere subset (with the total number of elements not determined) or fully paged (required for {@link #single()}).
	 * @param <T> Type of objects produced by the query.
	 */
	public abstract static class Builder<T> {
		@Nullable
		private final Paged<T> page;
		private final boolean hasPageCols;
		
		private int rowCol = 0;
		private int cntCol = 0;
		
		/**
		 * Creates a new pagination builder. 
		 * @param paging Optional pagination specification. This being {@code null} means pagination is not used.
		 * In that case the builder can only produce unpaged results.
		 */
		protected Builder(@Nullable final Pagination paging) {
			if (paging != null) {
				this.page = new Paged<>(paging.getSize(), paging.getOffset());
				this.hasPageCols = ! paging.isSubset();
			} else {
				this.page = null;
				this.hasPageCols = false;
			}
		}
		
		/**
		 * Produces the elements that constitute the contents of the page.
		 * The implementation must call {@link #update(Object, ResultSet)} accordingly to allow for pagination information to be extracted from the result.
		 * @param <U> Result type of the query. Usually either a {@code Collection}, an {@code Optional} or a single element.
		 * @param streamOperation Final operation to be performed on the result stream.
		 * @return The query result.
		 */
		protected abstract <U> U query(Function<Stream<T>, U> streamOperation);
		
		/**
		 * Updates the page as content elements are created from the query.
		 * Does nothing if pagination is not in use (see {@link #Builder(Pagination)}.
		 * @param element Content element.
		 * @param rs Result set positioned on the record the current element was created from. 
		 * @return Content element.
		 */
		@Nullable
		protected T update(@Nullable final T element, final ResultSet rs) {
			final Paged<?> currentPage = this.page;
			if (currentPage != null) {
				if (hasPageCols) {
					try {
						readPageCols(currentPage, rs);
					} catch (final SQLException e) {
						throw new IllegalStateException("Error reading pagination columns", e);
					}
				}
				currentPage.size++;
			}
			return element;
		}
		
		private void readPageCols(final Paged<?> page, final ResultSet rs) throws SQLException {
			if (rowCol == 0) {
				rowCol = rs.findColumn(ROW_NUMBER_COLUMN);
			}
			if (cntCol == 0) {
				cntCol = rs.findColumn(ROW_COUNT_COLUMN);
			}
			page.lastElement = rs.getLong(rowCol);
			if (page.firstElement == null) {
				page.firstElement = page.lastElement;
			}
			if (page.totalElements == null) {
				page.totalElements = rs.getLong(cntCol);
			}
		}
		
		/**
		 * Returns the first element in the content stream, without pagination.
		 * @return First element or none if the result is empty.
		 */
		public Optional<T> first() {
			return query(Stream::findFirst);
		}
		
		/**
		 * Returns the first element on the page, throwing an exception if there are more.
		 * This requires pagination to be active, for the total records aggregate to be available.
		 * @return First element or none if the result is empty.
		 * @throws IllegalStateException If more than one record is found.
		 */
		public Optional<T> single() {
			final Paged<T> currentPage = this.page;
			if (currentPage == null) {
				throw new IllegalArgumentException("Attempt to fetch single result from unpaged query.");
			}
			if (! hasPageCols) {
				throw new IllegalArgumentException("Total number of elements not known from query result.");
			}
			
			final Optional<T> result = first();
			if (result.isPresent() && currentPage.size < 1) {
				throw new IllegalStateException("Element found but page was not updated.");
			}
			if (currentPage.totalElements != null && currentPage.totalElements > 1) {
				throw new IllegalStateException("Expected a single element but " + currentPage.totalElements + " were found.");
			}
			return result;
		}
		
		/**
		 * Retrieves the result from the query backing this pagination builder without pagination information.
		 * This is intended to be used only if no pagination specification was provided.
		 * @return List of result elements.
		 */
		public List<T> all() {
			return query(s -> s.collect(Collectors.toList()));
		}
		
		/**
		 * Retrieves the query result inside a wrapper holding pagination information.
		 * Requires that update {{@link #update(Object, ResultSet)} is called for every element in the result. 
		 * @return Page object.
		 */
		public Paged<T> page() {
			final Paged<T> currentPage = this.page;
			if (currentPage == null) {
				throw new IllegalArgumentException("Attempt to create paged result from unpaged query.");
			}
			final List<T> entries = all();
			if (currentPage.size != entries.size()) {
				throw new IllegalStateException("Result size does not match page update count.");
			}
			currentPage.content = Collections.unmodifiableList(entries);
			return currentPage;
		}
	}
	
	private final Integer limit;
	private final Long offset;
	private int size;
	
	@Nullable
	private Long firstElement;
	
	@Nullable
	private Long lastElement;
	
	@Nullable
	private Long totalElements;
	
	@Nullable
	private List<T> content;

	/**
	 * Returns an empty Paged.
	 * @return an empty Paged
	 * @param <T> type of the non-existing contents
	 */
	public static <T> Paged<T> empty() {
		final Paged<T> ret = new Paged<>(0, 0L);
		ret.content = Collections.emptyList();
		return ret;
	}

	/**
	 * Create a Paged object holding fixed content;
	 * @param content the content for the Paged
	 * @return a Paged containing the content
	 * @param <T> type of the content
	 */
	public static <T> Paged<T> ofContent(final List<T> content) {
		final Paged<T> ret = new Paged<>(0, 0L);
		ret.content = content;
		ret.size = content.size();
		ret.totalElements = Long.valueOf(content.size());
		ret.firstElement = 1L;
		ret.lastElement = Long.valueOf(content.size());
		return ret;
	}

	/**
	 * Create a Paged object holding dynamic content;
	 * @param content the content for the Paged
	 * @param pagination the pagination information
	 * @return a Paged containing the content
	 * @param <T> type of the content
	 */
	public static <T> Paged<T> ofContent(final List<T> content, final Pagination pagination) {
		final Paged<T> ret = new Paged<>(pagination.getSize(), pagination.getOffset());
		final var contentList = content.stream().skip(pagination.getOffset()).limit(pagination.getSize())
				.collect(Collectors.toList());
		ret.content = contentList;
		ret.size = contentList.size();
		ret.totalElements = (long) content.size();
		ret.firstElement = pagination.getOffset() + 1L;
		ret.lastElement = contentList.isEmpty() ? 0 : pagination.getOffset() + ret.size;
		return ret;
	}

	/**
	 * Creates a new {@link Paged} object for the specified values.
	 * 
	 * @param content the content of the page.
	 * @param limit Number of elements requested.
	 * @param offset Number of elements before this page.
	 * @param totalElements Total number of available elements.
	 * @param firstElement Absolute index of the first element.
	 * @param lastElement Absolute index of the last element
	 */
	@JsonCreator
	public Paged(
			@JsonProperty("content") final List<T> content,
			@JsonProperty("limit") final Integer limit,
			@JsonProperty("offset") final Long offset,
			@JsonProperty("totalElements") @Nullable final Long totalElements,
			@JsonProperty("firstElement") @Nullable final Long firstElement,
			@JsonProperty("lastElement") @Nullable final Long lastElement) {
		this.limit = limit;
		this.offset = offset;
		this.size = content.size();
		this.content = content;
		this.totalElements = totalElements;
		this.firstElement = firstElement;
		this.lastElement = lastElement;
	}

	private Paged(final Integer limit, final Long offset) {
		this.limit = limit;
		this.offset = offset;
		this.size = 0;
	}
	
	/**
	 * Returns the content of the page.
	 * @return List of elements on the current page.
	 */
	public List<T> getContent() {
		return Objects.requireNonNull(content);
	}
	
	/**
	 * Performs an operation on every element on the page.
	 * @param action Operation to perform.
	 */
	public void forEach(final Consumer<? super T> action) {
		if (content != null) {
			content.forEach(action);
		}
	}
	
	/**
	 * Gets the maximum size of the page.
	 * @return Number of elements requested for the current page.
	 */
	public Integer getLimit() {
		return limit;
	}
	
	/**
	 * Gets the offset of the page relative to the whole query result.
	 * @return Number of records in the result before the current page.
	 */
	public Long getOffset() {
		return offset;
	}
	
	/**
	 * Gets the actual size of the page.
	 * @return Number of elements on the current page.
	 */
	public int getSize() {
		return size;
	}
	
	/**
	 * Returns the row number (over the entire result) of the first element on the page.
	 * The number of the first row in the result is 1.
	 * @return Row number.
	 */
	@Nullable
	public Long getFirstElement() {
		return firstElement;
	}
	
	/**
	 * Returns the row number (over the entire result) of the last element on the page.
	 * The number of the first row in the result is 1.
	 * @return Row number.
	 */
	@Nullable
	public Long getLastElement() {
		return lastElement;
	}
	
	/**
	 * Returns the total number of elements in the entire query result.
	 * @return Total number of elements found by the query.
	 */
	@Nullable
	public Long getTotalElements() {
		return totalElements;
	}
	
	@Override
	public String toString() {
		return "Paged [limit=" + limit + ", offset=" + offset + ", count=" + size 
				+ ", firstElement=" + firstElement + ", lastElement=" + lastElement
				+ ", totalElements=" + totalElements + "]";
	}
	
	/**
	 * Returns a new {@link Paged} with each element of the current one passed through the given converter {@link Function}.
	 * @param converter {@link Function} that turns each element into the target type.
	 * @return A new {@link Paged} with the mapped content of the current one.
	 */
	public <U> Paged<U> map(final Function<? super T, ? extends U> converter) {
		return new Paged<>(this.content != null ? this.content.stream().map(converter::apply).collect(Collectors.toList()) : Collections.emptyList(),
				this.limit, this.offset, totalElements, firstElement, lastElement);
	}
	
}
