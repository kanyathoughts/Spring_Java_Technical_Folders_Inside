/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

/**
 * Class to describe the pagination configuration to be achieved.
 */
public class Pagination {

	/** This is needed or else we get ArrayIndexOutOfBounds Exception in OrientDB. */
	public static final int ORIENT_RECORDS_LIMIT = (Integer.MAX_VALUE - 1) / 2;

	private final Integer index;
	private final Integer size;

	/**
	 * Constructor to initialize the pagination POJO.
	 * 
	 * @param pageNumber the number of pages required
	 * @param pageSize the number of elements required in each page
	 */
	public Pagination(final int pageNumber, final int pageSize) {
		if (pageNumber < 0) {
			throw new IllegalArgumentException("Page number must not be negative");
		}
		if (pageSize < 1) {
			throw new IllegalArgumentException("Page size must be greater than zero");
		} else if (pageSize > ORIENT_RECORDS_LIMIT) {
			throw new IllegalArgumentException("Page size must be less than 1073741823");
		}

		index = Integer.valueOf(pageNumber);
		size = Integer.valueOf(pageSize);
	}

	@Override
	public String toString() {
		final StringBuilder skipClauseForPagination = new StringBuilder(" SKIP ");
		skipClauseForPagination.append(index.intValue() * size.intValue())
								.append(" LIMIT ")
								.append(size);
		return skipClauseForPagination.toString();
	}

}
