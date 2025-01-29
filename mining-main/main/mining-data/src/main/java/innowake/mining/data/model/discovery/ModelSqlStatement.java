/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.StatementType;

/**
 * Represent an SQL statement in the model artifact.
 */
public class ModelSqlStatement extends ModelStatement {
	
	protected int length;
	protected int numberOfTables;
	protected int numberOfDistinctTables;
	protected int customComplexity;
	protected double halsteadComplexity;
	protected double halsteadDifficulty;
		
	public ModelSqlStatement() {
		reset();
	}
	
	public ModelSqlStatement(final ModelSqlStatement statement) {
		this.setFromStatement(statement);
	}
	
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.omitNullValues()
				.add("statement", statementType)
				.add("length", length)
				.add("numberOfTables", numberOfTables)
				.add("numberOfDistinctTables", numberOfDistinctTables)
				.add("customComplexity", customComplexity)
				.add("halsteadComplexity", halsteadComplexity)
				.add("halsteadDifficulty", halsteadDifficulty)
				.add("string", string)
				.toString();
	}
	
	@Override
	public int hashCode() {
		return super.hashCode() + Objects.hash(Integer.valueOf(length), Integer.valueOf(numberOfTables), Integer.valueOf(numberOfDistinctTables),
				Integer.valueOf(customComplexity), Double.valueOf(halsteadComplexity), Double.valueOf(halsteadDifficulty));
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass() || ! super.equals(obj)) {
			return false;
		}
		
		final ModelSqlStatement other = (ModelSqlStatement) obj;
		return customComplexity == other.customComplexity &&
				length == other.length && 
				numberOfDistinctTables == other.numberOfDistinctTables &&
				numberOfTables == other.numberOfTables &&
				Double.doubleToLongBits(halsteadComplexity) == Double.doubleToLongBits(other.halsteadComplexity) &&
				Double.doubleToLongBits(halsteadDifficulty) == Double.doubleToLongBits(other.halsteadDifficulty);
	}
	
	
	/**
	 * @return length of the SQL string 
	 */
	public int getLength() {
		return length;
	}

	/**
	 * @return number of tables in the SQL statement
	 */
	public int getNumberOfTables() {
		return numberOfTables;
	}

	/**
	 * @return number of distinct tables in the SQL statement
	 */
	public int getNumberOfDistinctTables() {
		return numberOfDistinctTables;
	}

	/**
	 * @return complexity score of the SQL statement based on custom criteria
	 */
	public int getCustomComplexity() {
		return customComplexity;
	}

	/**
	 * @return complexity score of the SQL statement based on Halstead complexity
	 */
	public double getHalsteadComplexity() {
		return halsteadComplexity;
	}

	/**
	 * @return difficulty score of the SQL statement based on Halstead difficulty
	 */
	public double getHalsteadDifficulty() {
		return halsteadDifficulty;
	}
	
	/*--- Setter Methods ---*/
	
	/**
	 * @param length is the length of the SQL string
	 *
	 * @return reference to this ModelSqlStatement
	 */
	public ModelSqlStatement setLength(final int length) {
		this.length = length;
		isModified = true;
		return this;
	}

	/**
	 * @param numberOfTables is the number of tables in the SQL statement
	 *
	 * @return reference to this ModelSqlStatement
	 */
	public ModelSqlStatement setNumberOfTables(final int numberOfTables) {
		this.numberOfTables = numberOfTables;
		isModified = true;
		return this;
	}

	/**
	 * @param numberOfDistinctTables is the number of distinct tables in the SQL statement
	 *
	 * @return reference to this ModelSqlStatement
	 */
	public ModelSqlStatement setNumberOfDistinctTables(final int numberOfDistinctTables) {
		this.numberOfDistinctTables = numberOfDistinctTables;
		isModified = true;
		return this;
	}

	/**
	 * @param customComplexity is a complexity score of the SQL statement based on custom criteria
	 *
	 * @return reference to this ModelSqlStatement
	 */
	public ModelSqlStatement setCustomComplexity(final int customComplexity) {
		this.customComplexity = customComplexity;
		isModified = true;
		return this;
	}

	/**
	 * @param halsteadComplexity is a complexity score of the SQL statement based on Halstead complexity
	 *
	 * @return reference to this ModelSqlStatement
	 */
	public ModelSqlStatement setHalsteadComplexity(final double halsteadComplexity) {
		this.halsteadComplexity = halsteadComplexity;
		isModified = true;
		return this;
	}

	/**
	 * @param halsteadDifficulty is a difficulty score of the SQL statement based on Halstead difficulty
	 *
	 * @return reference to this ModelSqlStatement
	 */
	public ModelSqlStatement setHalsteadDifficulty(final double halsteadDifficulty) {
		this.halsteadDifficulty = halsteadDifficulty;
		isModified = true;
		return this;
	}

	/**
	 * set the statement type enum.
	 *
	 * @param statementType is enum for what "type" the statement is
	 * @return reference to this ModelSqlStatement
	 */
	@Override
	public ModelSqlStatement setStatementType(final StatementType statementType) {
		super.setStatementType(statementType);
		return this;
	}
	
	/**
	 * 
	 * Set the actual statement.
	 *
	 * @param string the statement.
	 * @return reference to this ModelSqlStatement
	 */
	@Override
	public ModelSqlStatement setString(final String string) {
		super.setString(string);
		return this;
	}
	
	/**
	 * Reset the object to its initial state.
	 */
	@Override
	protected void reset() {
		super.reset();
		length = 0;
		numberOfTables = 0;
		numberOfDistinctTables = 0;
		customComplexity = 0;
		halsteadComplexity = 0;
		halsteadDifficulty = 0;
	}
		
	/**
	 * 
	 * Set this ModelSql object from another ModelSql object.
	 *
	 * @param statement the object we set this object with
	 * @return a reference to this ModelSqlStatement object
	 */
	public ModelSqlStatement setFromStatement(final ModelSqlStatement statement ) {
		statement.validate();
		super.setFromStatement(statement);
		this.length = statement.length;
		this.numberOfTables = statement.numberOfTables;
		this.numberOfDistinctTables = statement.numberOfDistinctTables;
		this.customComplexity = statement.customComplexity;
		this.halsteadComplexity = statement.halsteadComplexity;
		this.halsteadDifficulty = statement.halsteadDifficulty;
		return this;
	}
		
	/**
	 * {@inheritDoc}
	 */
	@Override
	public ModelSqlStatement validate() {
		super.validate();
		SqlStatementType.valueOf(assertNotNull(statementType).name());
		return this;
	}

	@Override
	public StatementPojoPrototype convertToPojoPrototype() {
		final Map<String, Object> properties = new HashMap<>();
		properties.put(StatementPojo.PROPERTY_KEY_SQL_LENGTH, length);
		properties.put(StatementPojo.PROPERTY_KEY_TABLES, numberOfTables);
		properties.put(StatementPojo.PROPERTY_KEY_DISTINCT_TABLES, numberOfDistinctTables);
		properties.put(StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY, customComplexity);
		properties.put(StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY, halsteadComplexity);
		properties.put(StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY, halsteadDifficulty);

		return super.convertToPojoPrototype()
					.setProperties(properties);
	}
}
