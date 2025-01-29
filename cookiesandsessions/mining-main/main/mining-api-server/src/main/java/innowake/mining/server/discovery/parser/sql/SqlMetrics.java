/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.sql;

import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.ndt.parsing.parser.sql.metrics.api.SqlComplexity;

/**
 * Wrapper class to keep SQL metrics information.
 */
public class SqlMetrics {
	private final SqlStatementType sqlStatementType;
	private final String sqlString;
	private final SqlComplexity sqlComplexity;
	
	/**
	 * Set SQL metrics information.
	 * @param sqlStatementType is type of SQL statement: SELECT, UPDATE, MERGE etc.
	 * @param sqlString is prepared SQL string
	 * @param sqlComplexity contains all complexity indicators
	 */
	public SqlMetrics(SqlStatementType sqlStatementType, String sqlString, SqlComplexity sqlComplexity) {
		this.sqlStatementType = sqlStatementType;
		this.sqlString = sqlString;
		this.sqlComplexity = sqlComplexity;
	}

	public SqlStatementType getSqlStatementType() {
		return sqlStatementType;
	}
	
	public String getSqlString() {
		return sqlString;
	}
	
	public int getLength() {
		return sqlString.length();
	}
	
	public int getNumberOfTables() {
		return sqlComplexity.getNumberOfTables();
	}
	
	public int getNumberOfDistinctTables() {
		return sqlComplexity.getNumberOfDistinctTables();
	}
	
	public int getCustomComplexity() {
		return sqlComplexity.getCustomScore();
	}
	
	public double getHalsteadComplexity() {
		return sqlComplexity.getHalsteadComplexity();
	}
	
	public double getHalsteadDifficulty() {
		return sqlComplexity.getHalsteadDifficulty();
	}
}
