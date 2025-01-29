/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query.clauses;

import static java.lang.String.format;

import com.github.raymanrt.orientqb.query.Assignable;
import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.Operator;
import com.github.raymanrt.orientqb.util.Commons;

/**
 * Clause for embedded set property type.
 * Example : 'Value' IN embeddedSet
 */
public class EmbeddedSetClause extends Clause {

	private final Object firstOperand;
	private final Operator operator;
	private final Object value;

	/**
	 * Instantiates a clause for type embedded set.
	 * 
	 * @param value the value to be matched in the embedded set
	 * @param operator type of operation
	 * @param field name of the embedded set field in entity class
	 */
	public EmbeddedSetClause(final Object value, final Operator operator, final String field) {
		this.firstOperand = field;
		this.operator = operator;
		this.value = value;
	}

	@Override
	public String toString() {
		String valueString = Commons.cast(value);
		if (value instanceof Assignable) {
			final Assignable assignable = (Assignable) value;
			valueString = assignable.getAssignment();
		}

		String firstOperandString = firstOperand.toString();
		if (firstOperand instanceof Assignable) {
			final Assignable assignable = (Assignable) firstOperand;
			firstOperandString = assignable.getAssignment();
		}

		return format(operator.getFormat(), valueString, firstOperandString);
	}

}
