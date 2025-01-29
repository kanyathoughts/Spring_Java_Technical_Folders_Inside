/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query.clauses;

import static java.lang.String.format;

import java.util.List;

/**
 * Clause for {@code CONTAINSANY} handling
 */
public class ClauseWithContainsAnyOperator extends ClauseWithInOperator {
	
    /**
     * Constructs a {@code CONTAINSANY} clause
     * 
     * @param field field to be queried.
     * @param values values to be queried for.
     */
	public ClauseWithContainsAnyOperator(final String field, final List<Object> values) {
		super(field, values);
	}

	@Override
	public String toString() {
		return format("%s CONTAINSANY [%s]", firstOperandString, valueString);
	}
	
}
