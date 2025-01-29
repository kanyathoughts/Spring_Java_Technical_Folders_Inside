/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query;

import static com.github.raymanrt.orientqb.query.Clause.clause;
import static com.github.raymanrt.orientqb.query.Clause.not;
import static innowake.lib.core.lang.Assert.assertEqual;

import org.junit.Test;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.Operator;
import com.github.raymanrt.orientqb.query.Query;
import com.gitub.raymanrt.orientqb.delete.Delete;

import innowake.mining.shared.springdata.EdgeDirection;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.api.query.clause.OrientOperator;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.repository.query.clauses.ClauseWithParenthesis;
import innowake.spring.data.orientdb.repository.query.clauses.EdgeClause;
import innowake.spring.data.orientdb.repository.query.clauses.EmbeddedMapClause;
import innowake.spring.data.orientdb.repository.query.clauses.EmbeddedSetClause;

/**
 * Test cases to validate the query creation using OSQL library.
 */
public class OSQLQueryUnitTests {

	/**
	 * Test case for simple select query.
	 */
	@Test
	public void testSelectQuery() {
		final Query query = new Query();
		query.from("Customer");
		assertEqual("SELECT FROM Customer", query.toString());
	}
	
	/**
	 * Test case for simple select query with where clause.
	 */
	@Test
	public void testSimpleWhereClauseQuery() {
		final Query query = new Query();
		final Object value = "User 1";
		final Clause clause = clause("customerName", Operator.EQ, value);
		query.from("Customer").where(clause);
		assertEqual("SELECT FROM Customer WHERE customerName = 'User 1'", query.toString());
	}
	
	/**
	 * Test case for select query with not like criteria.
	 */
	@Test
	public void testSimpleNotLikeClauseQuery() {
		final Query query = new Query();
		final Object value = "User 1";
		final Clause clause = not(clause("customerName", Operator.LIKE, value));
		query.from("Customer").where(clause);
		assertEqual("SELECT FROM Customer WHERE NOT customerName LIKE 'User 1'", query.toString());
	}
	
	/**
	 * Test case for select query with not like criteria.
	 */
	@Test
	public void testWhereLinkQuery() {
		final Query query = new Query();
		final Object value = "Bangalore";
		final Clause clause = clause("address.city", Operator.EQ, value);
		query.from("Customer").where(clause);
		assertEqual("SELECT FROM Customer WHERE address.city = 'Bangalore'", query.toString());
	}
	
	/**
	 * Test case to build a query with link list type.
	 */
	@Test
	public void testComplexClauseQuery() {
		final Query query = new Query();
		final Object value = "product 1";
		final Clause clause1 = new ClauseWithParenthesis(clause("productName", Operator.EQ, value));
		final Clause clause2 = clause("products", Operator.CONTAINS, clause1);
		query.from("Customer").where(clause2);
		assertEqual("SELECT FROM Customer WHERE products CONTAINS (productName = 'product 1')", query.toString());
	}
	
	/**
	 * Test case to build a query with embedded set type.
	 */
	@Test
	public void testEmbeddedSetInClauseQuery() {
		final Query query = new Query();
		final Object value = "product 1";
		final Clause clause = new EmbeddedSetClause(value, Operator.IN, "embeddedSet");
		query.from("Customer").where(clause);
		assertEqual("SELECT FROM Customer WHERE 'product 1' IN embeddedSet", query.toString());
	}
	
	/**
	 * Test case to build a query with embedded map type.
	 */
	@Test
	public void testMapClauseQuery() {
		final Query query = new Query();
		final Clause clause = new EmbeddedMapClause("embeddedMap", "key 1", Integer.valueOf(10));
		query.from("Customer").where(clause);
		assertEqual("SELECT FROM Customer WHERE embeddedMap['key 1'] = 10", query.toString());
	}
	
	/**
	 * Test case to build a query with edge type.
	 */
	@Test
	public void testEdgeClauseQuery() {
		final Query query = new Query();
		final Object value = "product 3";
		final Clause clause1 = clause("productName", Operator.CONTAINS, value);
		final Clause clause = new EdgeClause("reviewed", EdgeDirection.OUT, clause1);
		query.from("Customer").where(clause);
		assertEqual("SELECT FROM Customer WHERE OUT('reviewed').productName CONTAINS 'product 3'", query.toString());
	}
	
	/**
	 * Test case to build a delete query.
	 */
	@Test
	public void testDeleteQuery() {
		final Delete query = new Delete();
		final Object value = "product 3";
		final Clause clause = clause("productName", Operator.EQ, value);
		query.from("VERTEX Customer").where(clause);
		assertEqual("DELETE VERTEX Customer WHERE productName = 'product 3'", query.toString());
	}
	
	/**
	 * Test case to build a count query.
	 */
	@Test
	public void testCountQuery() {
		final Query query = new Query();
		final Object value = "product 3";
		final Clause clause = clause("productName", Operator.EQ, value);
		query.select("COUNT(*)").from("Customer").where(clause);
		assertEqual("SELECT COUNT(*) FROM Customer WHERE productName = 'product 3'", query.toString());
	}
	
	/**
	 * Test case to build a query with pagination and sorting.
	 */
	@Test
	public void testPaginationQuery() {
		final OrientQuery query = new OrientQuery();
		final Object value = "product 3";
		final Clause clause = clause("productName", Operator.EQ, value);
		final Pageable pageable = PageRequest.of(0, 2);
		((OrientQuery) query.from("Customer").where(clause).limit(pageable.getPageSize())).offset(pageable.getOffset());
		assertEqual("SELECT FROM Customer WHERE productName = 'product 3' LIMIT 2 OFFSET 0", query.toString());
	}
	
	/**
	 * Test case to validate the type of value assigned to clause is valid.
	 */
	@Test
	public void testClauseTypeSafetyCheckValidValue() {
		final OrientClause clause = OrientClause.clause(Product.class, "productName", OrientOperator.EQ, "Some product");
		assertEqual("productName = 'Some product'", clause.getClause().toString());
	}

	/**
	 * Test case to validate the type of value assigned to clause is not valid.
	 */
	@Test(expected = UnsupportedQueryTypeException.class)
	public void testClauseTypeSafetyCheckInValidValue() {
		OrientClause.clause(Product.class, "productName", OrientOperator.EQ, Long.valueOf(10));
	}
}
