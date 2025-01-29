/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import java.util.Optional;

import com.github.raymanrt.orientqb.query.Query;

/**
 * Generates orientDB query template.
 */
public class OrientQuery extends Query {

	private Optional<String> offset = Optional.empty();

	/**
	 * Initializes the offset sub clause of query.
	 *
	 * @param offsetValue the offset value
	 * @return {@link OrientQuery} instance
	 */
	public Query offset(final long offsetValue) {
		offset = Optional.of(Long.toString(offsetValue));
		return this;
	}

	/**
	 * Returns the {@link String} representation of offset query clause.
	 *
	 * @return the {@link String} representation of offset query clause
	 */
	protected String generateOffset() {
		if (offset.isPresent()) {
			return " OFFSET " + offset.get();
		}
		return "";
	}

	@Override
	public String toString() {
		return super.toString() + generateOffset();
	}
}
