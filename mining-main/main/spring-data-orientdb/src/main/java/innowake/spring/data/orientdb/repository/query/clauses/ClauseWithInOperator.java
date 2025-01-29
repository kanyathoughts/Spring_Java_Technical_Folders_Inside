/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query.clauses;

import static java.lang.String.format;

import java.util.List;
import java.util.stream.Collectors;

import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.Operator;

/**
 * Clause for {@link Operator (IN)} handling list of parameters.
 */
public class ClauseWithInOperator extends Clause {
	
	protected final List<Object> values;
	protected final String valueString;
	protected final String firstOperandString;
	
    /**
     * Constructor with fields.
     * 
     * @param field field to be queried.
     * @param values values to be queried for.
     */
	public ClauseWithInOperator(final String field, final List<Object> values) {
		this.values = values;
		this.valueString = createValueString();
		this.firstOperandString = field;
	}

	private String createValueString() {
		if (this.values.isEmpty()) {
			return "";
		}
		return String.class.isAssignableFrom(this.values.get(0).getClass()) 
			? String.join(",", this.values.stream().map(v -> "'" + v.toString() + "'").collect(Collectors.toList()))
			: String.join(",", this.values.stream().map(Object::toString).collect(Collectors.toList()));
	}

	@Override
	public String toString() {
		return format("%s IN [%s]", firstOperandString, valueString);
	}
	
}
