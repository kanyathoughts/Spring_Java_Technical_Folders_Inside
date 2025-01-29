/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.clause;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.ArrayList;
import java.util.Arrays;
import org.junit.jupiter.api.Test;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.api.query.clause.OrientOperator;

/**
 * Tests for {@link OrientClause}
 */
class OrientClauseTest {

	/**
	 * Tests that no brackets are set when nested and non nested OR clauses are used.
	 */
	@Test
	void testNestedOr() {
		final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
		final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
		final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");

		final OrientClause innerOrClause = OrientClause.or(ac2, ac3);
		final OrientClause outerAndClause = OrientClause.and(ac1, innerOrClause);
		
		assertEquals("ac1 = 'A' AND ( ac2 = 'B' OR ac3 = 'C' )", outerAndClause.getClause().toString());
	}

	/**
	 * Tests that no brackets are set when multiple nested and non nested OR clauses are used.
	 */
	@Test
	void testNestedOrs() {
		final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
		final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
		final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");
		final OrientClause ac4 = OrientClause.clause("ac4", OrientOperator.EQ, "D");

		final OrientClause innerOrClause1 = OrientClause.or(ac1, ac2);
		final OrientClause innerOrClause2 = OrientClause.or(ac3, ac4);
		final OrientClause outerOrClause1 = OrientClause.or(innerOrClause1, innerOrClause2);
		
		assertEquals("( ac1 = 'A' OR ac2 = 'B' ) OR ( ac3 = 'C' OR ac4 = 'D' )", outerOrClause1.getClause().toString());

		final OrientClause outerOrClause2 = OrientClause.or(ac1, ac2, ac3, ac4);

		assertEquals("ac1 = 'A' OR ac2 = 'B' OR ac3 = 'C' OR ac4 = 'D'", outerOrClause2.getClause().toString());
	}

	/**
	 * Tests that no brackets are set when nested and non nested AND clauses are used.
	 */
	@Test
	void testNestedAnd() {
		final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
		final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
		final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");

		final OrientClause innerAndClause = OrientClause.and(ac2, ac3);
		final OrientClause outerAndClause = OrientClause.and(ac1, innerAndClause);
		
		assertEquals("ac1 = 'A' AND ac2 = 'B' AND ac3 = 'C'", outerAndClause.getClause().toString());
	}

	/**
	 * Tests that no brackets are set when multiple nested and non nested AND clauses are used.
	 */
	@Test
	void testNestedAnds() {
		final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
		final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
		final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");
		final OrientClause ac4 = OrientClause.clause("ac4", OrientOperator.EQ, "D");

		final OrientClause innerAndClause1 = OrientClause.and(ac1, ac2);
		final OrientClause innerAndClause2 = OrientClause.and(ac3, ac4);
		final OrientClause outerAndClause1 = OrientClause.and(innerAndClause1, innerAndClause2);
		
		assertEquals("ac1 = 'A' AND ac2 = 'B' AND ac3 = 'C' AND ac4 = 'D'", outerAndClause1.getClause().toString());

		final OrientClause outerAndClause2 = OrientClause.and(ac1, ac2, ac3, ac4);
		assertEquals("ac1 = 'A' AND ac2 = 'B' AND ac3 = 'C' AND ac4 = 'D'", outerAndClause2.getClause().toString());
	}

	/**
	 * Tests that brackets are set only around OR clauses with nested AND and OR clauses and an outer AND clause.
	 */
	@Test
	void testNestedAndsAndOrsWithOuterAnd() {
		final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
		final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
		final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");
		final OrientClause ac4 = OrientClause.clause("ac4", OrientOperator.EQ, "D");
		final OrientClause ac5 = OrientClause.clause("ac5", OrientOperator.EQ, "E");
		final OrientClause ac6 = OrientClause.clause("ac6", OrientOperator.EQ, "F");
		final OrientClause ac7 = OrientClause.clause("ac7", OrientOperator.EQ, "G");
		final OrientClause ac8 = OrientClause.clause("ac8", OrientOperator.EQ, "H");

		final OrientClause innerAndClause1 = OrientClause.and(ac1, ac2);
		final OrientClause innerOrClause1 = OrientClause.or(ac3, ac4);
		
		final OrientClause outerAndClause1 = OrientClause.and(innerAndClause1, innerOrClause1);
		
		final OrientClause innerAndClause2 = OrientClause.and(ac5, ac6);
		final OrientClause innerOrClause2 = OrientClause.or(ac7, ac8);

		final OrientClause outerOrClause1 = OrientClause.or(innerAndClause2, innerOrClause2);
		
		
		final OrientClause outerAndClause2 = OrientClause.and(outerAndClause1, outerOrClause1);
		/*
		 *                                                             outerAndClause2 = 
		 *                                                             OrientClause.and
		 *                             outerAndClause1 =                                             outerOrClause1 =
		 *                              OrientClause.and                                             OrientClause.or
		 *                innerAndClause1 =               innerOrClause1 =            innerAndClause2 =            innerOrClause2 = 
		 *            OrientClause.and(ac1, ac2)    OrientClause.or(ac3, ac4)    OrientClause.and(ac5, ac6)    OrientClause.or(ac7, ac8)
		 */
		assertEquals("ac1 = 'A' AND ac2 = 'B' AND ( ac3 = 'C' OR ac4 = 'D' ) AND ( ac5 = 'E' AND ac6 = 'F' OR ( ac7 = 'G' OR ac8 = 'H' ) )",
						outerAndClause2.getClause().toString());
		
		final OrientClause outerOrClause2 = OrientClause.or(outerAndClause1, outerOrClause1);
		/*
		 *                                                             outerOrClause2 = 
		 *                                                             OrientClause.or
		 *                             outerAndClause1 =                                             outerOrClause1 =
		 *                              OrientClause.and                                             OrientClause.or
		 *                innerAndClause1 =               innerOrClause1 =            innerAndClause2 =            innerOrClause2 = 
		 *            OrientClause.and(ac1, ac2)    OrientClause.or(ac3, ac4)    OrientClause.and(ac5, ac6)    OrientClause.or(ac7, ac8)
		 */
		assertEquals("ac1 = 'A' AND ac2 = 'B' AND ( ac3 = 'C' OR ac4 = 'D' ) OR ( ac5 = 'E' AND ac6 = 'F' OR ( ac7 = 'G' OR ac8 = 'H' ) )",
						outerOrClause2.getClause().toString());
	}

	/**
	 * Tests that brackets are set only around OR clauses with nested AND and OR clauses and an outer OR clause.
	 */
	@Test
	void testNestedAndsAndOrsDeepWithOuterOr() {
		final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
		final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
		final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");
		final OrientClause ac4 = OrientClause.clause("ac4", OrientOperator.EQ, "D");
		final OrientClause ac5 = OrientClause.clause("ac5", OrientOperator.EQ, "E");
		final OrientClause ac6 = OrientClause.clause("ac6", OrientOperator.EQ, "F");
		final OrientClause ac7 = OrientClause.clause("ac7", OrientOperator.EQ, "G");
		final OrientClause ac8 = OrientClause.clause("ac8", OrientOperator.EQ, "H");

		final OrientClause innerOrClause11 = OrientClause.or(ac1, ac2);
		final OrientClause innerOrClause1 = OrientClause.or(ac3, ac4);
		
		final OrientClause outerAndClause1 = OrientClause.and(innerOrClause11, innerOrClause1);
		
		final OrientClause innerAndClause2 = OrientClause.and(ac5, ac6);
		final OrientClause innerOrClause2 = OrientClause.or(ac7, ac8);

		final OrientClause outerOrClause1 = OrientClause.or(innerAndClause2, innerOrClause2);
		
		
		final OrientClause outerAndClause2 = OrientClause.and(outerAndClause1, outerOrClause1);
		/*
		 *                                                             outerAndClause2 = 
		 *                                                             OrientClause.and
		 *                             outerAndClause1 =                                             outerOrClause1 =
		 *                              OrientClause.and                                             OrientClause.or
		 *                innerOrClause11 =               innerOrClause1 =            innerAndClause2 =            innerOrClause2 = 
		 *            OrientClause.or(ac1, ac2)     OrientClause.or(ac3, ac4)    OrientClause.and(ac5, ac6)    OrientClause.or(ac7, ac8)
		 */
		assertEquals("( ac1 = 'A' OR ac2 = 'B' ) AND ( ac3 = 'C' OR ac4 = 'D' ) AND ( ac5 = 'E' AND ac6 = 'F' OR ( ac7 = 'G' OR ac8 = 'H' ) )",
						outerAndClause2.getClause().toString());
		
		final OrientClause outerOrClause2 = OrientClause.or(outerAndClause1, outerOrClause1);
		/*
		 *                                                             outerOrClause2 = 
		 *                                                             OrientClause.or
		 *                             outerAndClause1 =                                             outerOrClause1 =
		 *                              OrientClause.and                                             OrientClause.or
		 *                innerOrClause11 =               innerOrClause1 =            innerAndClause2 =            innerOrClause2 = 
		 *            OrientClause.or(ac1, ac2)     OrientClause.or(ac3, ac4)    OrientClause.and(ac5, ac6)    OrientClause.or(ac7, ac8)
		 */
		assertEquals("( ac1 = 'A' OR ac2 = 'B' ) AND ( ac3 = 'C' OR ac4 = 'D' ) OR ( ac5 = 'E' AND ac6 = 'F' OR ( ac7 = 'G' OR ac8 = 'H' ) )",
						outerOrClause2.getClause().toString());
	}

	/**
	 * Tests for the query in ModulesGraphQlController.executeEdgeQuery(ModuleV2, DataFetchingEnvironment, OrientRepository<T>, EdgeDirection, String)
	 * but with swapped ANDs and ORs to test that brackets are set only around OR clauses.
	 */
	@Test
	void testSwappedAndOr() {
		/* SELECT count(*) FROM References WHERE
		 * ( out.id = 1535 OR ( in.projectLink.id = 0 AND in.projectLink.id IN [1] AND in.projectLink.clientLink.id IN [] ) )
		 * OR ( in.objectTypeLink.typeLink.name LIKE 'DBD' AND properties.DB_ACCESS_TYPE LIKE 'READ' ) */
		
		OrientClause filterClause = OrientClause.clause("out.id", OrientOperator.EQ, 1535);
		filterClause = OrientClause.or(filterClause, OrientClause.and(
				OrientClause.clause("in.projectLink.id", OrientOperator.EQ, 0),
				OrientClause.clause("in.projectLink.id", OrientOperator.IN, Arrays.asList(1)),
				OrientClause.clause("in.projectLink.clientId", OrientOperator.IN, new ArrayList<Long>())));
		filterClause = OrientClause.or(filterClause, OrientClause.and(OrientClause.clause("in.objectTypeLink.typeLink.name", OrientOperator.LIKE, "DBD"),
																	  OrientClause.clause("properties.DB_ACCESS_TYPE", OrientOperator.LIKE, "READ")));
		/*           ( out.id = 1535 OR ( in.projectLink.id = 0 AND in.projectLink.id IN [1] AND in.projectLink.clientLink.id IN [] ) ) */
		assertEquals("( out.id = 1535 OR in.projectLink.id = 0 AND in.projectLink.id IN [1] AND in.projectLink.clientId IN [] )"
		/*          OR ( in.objectTypeLink.typeLink.name LIKE 'DBD' AND properties.DB_ACCESS_TYPE LIKE 'READ' ) */
				+ " OR in.objectTypeLink.typeLink.name LIKE 'DBD' AND properties.DB_ACCESS_TYPE LIKE 'READ'",
				filterClause.getClause().toString());
	}

	/**
	 * Tests that no brackets are set when an outer OR and only inner AND clauses are used
	 */
	@Test
	void testNestedAndsAndOrsDeep1() {
	    final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
	    final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
	    final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");
	    final OrientClause ac4 = OrientClause.clause("ac4", OrientOperator.EQ, "D");
	    
	    final OrientClause innerOrClause11 = OrientClause.and(ac2, ac3);
	    final OrientClause innerOrClause1 = OrientClause.and(innerOrClause11, ac4);
	    final OrientClause outerAndClause1 = OrientClause.or(ac1, innerOrClause1);
	
	    assertEquals("ac1 = 'A' OR ac2 = 'B' AND ac3 = 'C' AND ac4 = 'D'", outerAndClause1.getClause().toString());
	}

	/**
	 * Tests that brackets are set correctly around NOT clause
	 */
	@Test
    void testNot() {
        final OrientClause ac1 = OrientClause.clause("ac1", OrientOperator.EQ, "A");
        final OrientClause ac2 = OrientClause.clause("ac2", OrientOperator.EQ, "B");
        final OrientClause ac3 = OrientClause.clause("ac3", OrientOperator.EQ, "C");
        final OrientClause ac4 = OrientClause.clause("ac4", OrientOperator.EQ, "D");

        final OrientClause innerOrClause11 = OrientClause.and(ac2, ac3);
        final OrientClause innerOrClause1 = OrientClause.and(innerOrClause11, ac4);
        final OrientClause innerOrClause2 = OrientClause.not(innerOrClause1);

        final OrientClause outerAndClause1 = OrientClause.or(ac1, innerOrClause2);
        
        assertEquals("ac1 = 'A' OR NOT (ac2 = 'B' AND ac3 = 'C' AND ac4 = 'D')", outerAndClause1.getClause().toString());
    }

}
