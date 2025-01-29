/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;


import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

/**
 * Class to maintain the sorting clauses defined to sort on the basis of.
 */
public class SortOrder {

	private static final String SORT_VAR = "$sort";

	private final List<SortClause> sortClauses;

	/**
	 * Creates SortOrder with a empty list of {@link SortClause}.
	 */
	public SortOrder() {
		sortClauses = new ArrayList<>();
	}

	/**
	 * Creates SortOrder with a new {@link SortClause} containing properties ordered by {@link Direction#ASC}.
	 *
	 * @param properties list of properties ordered by {@link Direction#ASC}
	 */
	public SortOrder(final String... properties) {
		this(Direction.ASC, properties);
	}

	/**
	 * Creates SortOrder with a new {@link SortClause} containing properties ordered by direction.
	 *
	 * @param direction  the specified {@link Direction} can be either ASC or DESC
	 * @param properties list of properties ordered by direction
	 */
	public SortOrder(final Direction direction, final String... properties) {
		this();
		add(direction, properties);
	}

	/**
	 * Adds a new {@link SortClause} containing properties ordered by {@link Direction#ASC}.
	 *
	 * @param properties list of properties ordered by {@link Direction#ASC}
	 * @return SortOrder instance after adding the sorting clause to list
	 */
	public SortOrder add(String... properties) {
		return add(Direction.ASC, properties);
	}

	/**
	 * Adds a new {@link SortClause} containing properties ordered by direction.
	 *
	 * @param direction  the specified {@link Direction} can be either ASC or DESC
	 * @param properties list of properties ordered by direction
	 * @return SortOrder instance after adding the sorting clause to list
	 */
	public SortOrder add(final Direction direction, final String... properties) {
		sortClauses.add(new SortClause(direction, properties));
		return this;
	}

	/**
	 * Adds a new {@link SortClause} containing properties ordered by {@link Direction#ASC}.
	 *
	 * @param properties list of properties ordered by {@link Direction#ASC}
	 * @return SortOrder instance after adding the sorting clause to list
	 */
	public SortOrder asc(final String... properties) {
		return add(Direction.ASC, properties);
	}

	/**
	 * Adds a new {@link SortClause} containing properties ordered by {@link Direction#DESC}.
	 *
	 * @param properties list of properties ordered by {@link Direction#DESC}
	 * @return SortOrder instance after adding the sorting clause to list
	 */
	public SortOrder desc(final String... properties) {
		return add(Direction.DESC, properties);
	}

	/**
	 * Return the list of existing {@link SortClause}.
	 * 
	 * @return all the sorting clauses added
	 */
	public List<SortClause> sortClauses() {
		return sortClauses;
	}

	/**
	 * Validates if a sort clause is present.
	 * 
	 * @return true if sort clauses are not empty
	 */
	public boolean hasSortClauses() {
		return ! sortClauses.isEmpty();
	}

	/**
	 * Gets the SQL for the LET clause of the query.
	 * @return the LET clause for the sort order
	 */
	public String asLetClause() {
		if ( ! sortClauses.isEmpty()) {
			final List<String> letClauses = new ArrayList<>();
			int index = 0;
			for (final SortClause clause : sortClauses) {
				for (final String property : clause.getProperties()) {
					letClauses.add(SORT_VAR + index++ + "=" + property);
				}
			}
			return " LET " + String.join(",", letClauses);
		}
		return StringUtils.EMPTY;
	}

	/**
	 * Gets the SQL for the ORDER BY clause of the query.
	 * @return the ORDER BY clause for the sort order
	 */
	public String asOrderByClause() {
		if ( ! sortClauses.isEmpty()) {
			final List<String> orderByClauses = new ArrayList<>();
			for (final SortClause clause : sortClauses) {
				for (int index = 0; index < clause.getProperties().length; index++) {
					orderByClauses.add(SORT_VAR + index + " " + clause.getDirection());
				}
			}
			return " ORDER BY " + String.join(",", orderByClauses);
		}
		return StringUtils.EMPTY;
	}

	/**
	 * Gets the SQL for the SELECT clause of the query.
	 * @return the SELECT clause
	 */
	public String asSelectClause() {
		if ( ! sortClauses.isEmpty()) {
			final List<String> projections = new ArrayList<>();
			for (final SortClause clause : sortClauses) {
				for (int index = 0; index < clause.getProperties().length; index++) {
					projections.add(SORT_VAR + index);
				}
			}
			return "SELECT *," + String.join(",", projections);
		}
		return "SELECT *";
	}

	/**
	 * Contains the ENUM values to distinguish ascending or descending direction.
	 */
	public enum Direction {
		/**
		 * Ascending.
		 */
		ASC, 
		/**
		 * Descending.
		 */
		DESC
	}

}
