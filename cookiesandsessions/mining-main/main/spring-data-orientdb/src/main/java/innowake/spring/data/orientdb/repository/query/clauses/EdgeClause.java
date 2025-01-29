/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query.clauses;

import static java.lang.String.format;

import com.github.raymanrt.orientqb.query.Clause;

import innowake.mining.shared.springdata.EdgeDirection;

/**
 * Build a criteria for a vertex linked via an edge.
 * Example : OUT('reviewed').productName
 */
public class EdgeClause extends Clause {

	private final String edgeName;
	private final EdgeDirection direction;
	private final Clause clause;

	/**
	 * Instantiates a clause for querying entity linked by an edge.
	 * 
	 * @param edgeName name of the edge class
	 * @param direction the direction of the edge
	 * @param clause field name in the related vertex
	 */
	public EdgeClause(final String edgeName, final EdgeDirection direction, final Clause clause) {
		this.edgeName = edgeName;
		this.direction = direction;
		this.clause = clause;
	}

	@Override
	public String toString() {
		final String firstOperandString = direction.toString();
		final String format = "%s('%s').%s";
		return format(format, firstOperandString, edgeName, clause.toString());
	}

}
