/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query.clauses;

import com.github.raymanrt.orientqb.query.Clause;

/**
 * Custom clause with custom SQL content.
 */
public class CustomClause extends Clause {

	private final String clause;

	/**
	 * Construct new custom clause with given content. Content must be valid and safe SQL.
	 *
	 * @param clause the SQL for the clause
	 */
	public CustomClause(final String clause) {
		this.clause = clause;
	}

	@Override
	public String toString() {
		return clause;
	}
}
