/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage;

import innowake.mining.server.datalineage.fieldtracing.SqlUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

/**
 * Tests {@linkplain SqlUtil} methods
 */
class SqlUtilTest {
	
	@Test
	void testSelectInto() {
		final SqlUtil.SqlInfo info = SqlUtil.getSqlInfo("SELECT"
				+ " ALPHA_SHORT"
				+ " INTO"
				+ " :ALPHA-SHORT-HOST"
				+ " FROM IW_SQL_TEST WHERE"
				+ " ALPHA_SHORT = :ALPHA-HOST");
		final Map<String, List<String>> map = info.getColumnMap();
		final List<String> columns = map.get("ALPHA-SHORT-HOST");
		Assertions.assertNotNull(columns, "The host variable ALPHA-SHORT-HOST should map to a column");
		Assertions.assertEquals(1, columns.size());
		Assertions.assertEquals("ALPHA_SHORT", columns.get(0));
		
		Assertions.assertEquals(1, info.getReadFrom().size());
		Assertions.assertTrue(info.getReadFrom().stream().anyMatch(var -> "ALPHA-HOST".equals(var)));
		Assertions.assertEquals(1, info.getWrittenTo().size());
		Assertions.assertTrue(info.getWrittenTo().stream().anyMatch(var -> "ALPHA-SHORT-HOST".equals(var)));
	}
	
	@Test
	void testSelectMultipleColumnsIntoOneVar() {
		final SqlUtil.SqlInfo info = SqlUtil.getSqlInfo("SELECT"
				+ " ALPHA_SHORT, ALPHA_LONG"
				+ " INTO"
				+ " :ALPHA-SHORT-HOST"
				+ " FROM IW_SQL_TEST WHERE"
				+ " ALPHA_SHORT = 'TEST1'");
		
		final Map<String, List<String>> map = info.getColumnMap();
		final List<String> columns = map.get("ALPHA-SHORT-HOST");
		Assertions.assertNotNull(columns, "The host variable ALPHA-SHORT-HOST should map to columns");
		Assertions.assertEquals(2, columns.size());
		Assertions.assertEquals("ALPHA_SHORT", columns.get(0));
		Assertions.assertEquals("ALPHA_LONG", columns.get(1));
		
		Assertions.assertEquals(0, info.getReadFrom().size());
		Assertions.assertEquals(1, info.getWrittenTo().size());
		Assertions.assertTrue(info.getWrittenTo().stream().anyMatch(var -> "ALPHA-SHORT-HOST".equals(var)));
	}
	
	@Test
	void testSelectOneColumnIntoMultipleVars() {
		final SqlUtil.SqlInfo info = SqlUtil.getSqlInfo("SELECT"
				+ " ALPHA_SHORT"
				+ " INTO"
				+ " :ALPHA-SHORT-HOST-1, :ALPHA-SHORT-HOST-2"
				+ " FROM IW_SQL_TEST WHERE"
				+ " ALPHA_SHORT = 'TEST1'");
		
		final Map<String, List<String>> map = info.getColumnMap();
		final List<String> columns1 = map.get("ALPHA-SHORT-HOST-1");
		Assertions.assertNotNull(columns1, "The host variable ALPHA-SHORT-HOST-1 should map to a column");
		Assertions.assertEquals(1, columns1.size());
		Assertions.assertEquals("ALPHA_SHORT", columns1.get(0));

		Assertions.assertNull(map.get("ALPHA-SHORT-HOST-2"), "The parser shouldn't bind second host variable to the DB column.");
		
		Assertions.assertEquals(0, info.getReadFrom().size());
		Assertions.assertEquals(2, info.getWrittenTo().size());
		Assertions.assertTrue(info.getWrittenTo().stream().anyMatch(var -> "ALPHA-SHORT-HOST-1".equals(var)));
	}
	
	@Test
	void testInsert() {
		final SqlUtil.SqlInfo info = SqlUtil.getSqlInfo("INSERT INTO IW_SQL_TEST (ALPHA, BETA)"
				+ " VALUES (:ALPHA-HOST, :BETA-HOST)");
		
		final Map<String, List<String>> map = info.getColumnMap();
		final List<String> columns1 = map.get("ALPHA-HOST");
		Assertions.assertNotNull(columns1, "The host variable ALPHA-HOST should map to a column");
		Assertions.assertEquals(1, columns1.size());
		Assertions.assertEquals("ALPHA", columns1.get(0));
		
		final List<String> columns2 = map.get("BETA-HOST");
		Assertions.assertNotNull(columns2, "The host variable BETA-HOST should map to a column");
		Assertions.assertEquals(1, columns2.size());
		Assertions.assertEquals("BETA", columns2.get(0));
		
		Assertions.assertEquals(2, info.getReadFrom().size());
		Assertions.assertEquals(0, info.getWrittenTo().size());
		Assertions.assertTrue(info.getReadFrom().stream().anyMatch(var -> "ALPHA-HOST".equals(var)));
		Assertions.assertTrue(info.getReadFrom().stream().anyMatch(var -> "BETA-HOST".equals(var)));
	}
	
	@Test
	void testUpdate() {
		final SqlUtil.SqlInfo info = SqlUtil.getSqlInfo("UPDATE ALPHA"
				+ " SET ALPHA = :ALPHA-HOST"
				+ " WHERE DEPTNO = :ALPHA-HOST");
		
		final Map<String, List<String>> map = info.getColumnMap();
		final List<String> columns1 = map.get("ALPHA-HOST");
		Assertions.assertNotNull(columns1, "The host variable ALPHA-HOST should map to a column");
		Assertions.assertEquals(1, columns1.size());
		Assertions.assertEquals("ALPHA", columns1.get(0));
		
		Assertions.assertEquals(1, info.getReadFrom().size());
		Assertions.assertEquals(0, info.getWrittenTo().size());
		Assertions.assertTrue(info.getReadFrom().stream().anyMatch(var -> "ALPHA-HOST".equals(var)));
	}
	
	/**
	 * getColumnMap should return an empty map and throw no Exception.
	 */
	@Test
	void testDelete() {
		final SqlUtil.SqlInfo info = SqlUtil.getSqlInfo("DELETE FROM DEL.TABLE1"
				+ " WHERE SOME_VALUE = :ALPHA-SHORT");
		final Map<String, List<String>> map = info.getColumnMap();
		Assertions.assertTrue(map.isEmpty(), "Map for DELETE should be empty");
		Assertions.assertEquals(1, info.getReadFrom().size());
		Assertions.assertTrue(info.getReadFrom().stream().anyMatch(var -> "ALPHA-SHORT".equals(var)));
	}

}
