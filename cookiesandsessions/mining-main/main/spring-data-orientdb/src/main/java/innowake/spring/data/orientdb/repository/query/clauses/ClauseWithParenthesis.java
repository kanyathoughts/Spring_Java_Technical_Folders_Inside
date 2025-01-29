/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query.clauses;

import com.github.raymanrt.orientqb.query.Clause;

/**
 * Wraps a clause with parenthesis.
 * Example : (productName = "product 1")
 */
public class ClauseWithParenthesis extends Clause {

	private final Clause clause;

	/**
	 * Instantiates a clause with parenthesis.
	 * 
	 * @param clause the clause to be wrapped
	 */
	public ClauseWithParenthesis(final Clause clause) {
		this.clause = clause;
	}

	@Override
	public String toString() {
		return "(" + clause.toString() + ")";
	}

}
