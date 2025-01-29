/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.plugin.test;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.orientechnologies.orient.core.sql.executor.OResultSet;

/**
 * Tests whether connection to database can be established.
 */
public class ConnectionTest extends AbstractTest {
	
	@Test
	public void testConnection() {
		final OResultSet rs = executeSQL("select count(*) as count from AstNode");
		assertTrue(rs.hasNext());
	}
}