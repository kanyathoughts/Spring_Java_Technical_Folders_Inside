/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import java.io.Serializable;
import java.util.Objects;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.StatementType;

/**
 * Represent a statement in the model artifact
 */
public class ModelStatement implements Serializable {
	
	/**
	 * The name of the statement or group of statement.
	 */
	@Nullable protected StatementType statementType;
	
	/**
	 * The statement string or arguments.
	 */
	@Nullable protected String string;
	protected boolean isModified;

	public ModelStatement() {
		reset();
	}
	
	public ModelStatement(final ModelStatement statement) {
		this.setFromStatement(statement);
	}
	
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.omitNullValues()
				.add("statement", statementType)
				.add("string", "" + string)
				.toString();
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		
		final ModelStatement other = (ModelStatement) obj;
		return statementType == other.statementType && Objects.equals(string, other.string);
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(statementType, string);
	}
	
	/**
	 * 
	 * Return statement type for this statement. 
	 *
	 * @return statement type enum.
	 */
	public StatementType getStatementType() {
		validate();
		return Assert.assertNotNull(statementType, "Statement Type must not be NULL");
	}
	
	/**
	 * 
	 * Return the actual statement part of this statement object.
	 *
	 * @return the actual statement.
	 */
	public String getString() {
		validate();
		return Assert.assertNotNull(string, "String must not be NULL");
	}
	
	/*--- Setter Methods ---*/
	
	/**
	 * Reset the object to its initial state
	 */
	protected void reset() {
		statementType = null;
		string = null;
		isModified = true;
	}

	/**
	 * @return {@code true} if this {@link ModelStatement} was modified. Otherwise {@code false}. 
	 */
	public boolean isModified() {
		return isModified;
	}

	/**
	 * Sets the modified state of this {@link ModelStatement} to the given {@code modified} value.
	 *
	 * @param modified the modified value
	 * @return this {@link ModelStatement} instance for method chaining
	 */
	public ModelStatement setModified(final boolean modified) {
		isModified = modified;
		return this;
	}
	
	/**
	 * Sets the statement type enum.
	 *
	 * @param statementType is enum for what "type" the statement is.
	 * @return reference to this ModelStatement.
	 */
	public ModelStatement setStatementType(final StatementType statementType) {
		this.statementType = statementType;
		isModified = true;
		return this;
	}
	
	/**
	 * Sets the actual statement.
	 *
	 * @param string the statement.
	 * @return reference to this ModelStatement.
	 */
	public ModelStatement setString(final String string) {
		this.string = string;
		isModified = true;
		return this;
	}
	
	/**
	 * Sets this ModelStatement object from another ModelStatement object.
	 *
	 * @param statement the object we set this object with.
	 * @return a reference to this ModelStatement object.
	 */
	public ModelStatement setFromStatement(final ModelStatement statement ) {
		statement.validate();
		this.statementType = statement.statementType;
		this.string = statement.string;
		isModified = true;
		return this;
	}
	
	/**
	 * Used in conjunction with builder pattern, this validate method can
	 * be used to ensure the object has all the necessary members set and
	 * in addition will assign appropriate default values as needed.
	 * <br>
	 * Should only be used when all builder methods have been called and object
	 * is appropriately "built".
	 *
	 * @return a reference to this object
	 */
	public ModelStatement validate() {
		
		if ( statementType == null ) {
			throw new IllegalArgumentException("The 'statement type' must be set.");
		}

		if ( string == null ) {
			throw new IllegalArgumentException("The 'string' must be set.");
		}
		
		return this;
	}

	/**
	 * @return a new {@link StatementPojoPrototype} instance containing all non {@code null} values of this {@link ModelStatement}.
	 */
	public StatementPojoPrototype convertToPojoPrototype() {
		return new StatementPojoPrototype()
				.setType(getStatementType())
				.setText(getString());
	}
}
