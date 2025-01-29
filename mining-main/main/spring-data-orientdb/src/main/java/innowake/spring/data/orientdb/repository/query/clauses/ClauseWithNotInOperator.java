/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query.clauses;

import static java.lang.String.format;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.github.raymanrt.orientqb.query.Clause;

/**
 * Clause for Not IN operator handling list of parameters.
 */
public class ClauseWithNotInOperator extends Clause {
	
	private final List<Object> values;
	private final String valueString;
	private final String firstOperandString;
	
    /**
     * Constructor with fields.
     * 
     * @param field field to be queried
     * @param values values to be queried for
     */
	public ClauseWithNotInOperator(final String field, final List<Object> values) {
		this.values = values;
		this.valueString = createValueString();
		this.firstOperandString = field;
	}

	private String createValueString() {
		if (this.values.isEmpty()) {
			return StringUtils.EMPTY;
		}
		return String.class.isAssignableFrom(this.values.get(0).getClass()) 
			? String.join(",", this.values.stream().map(v -> "'" + v.toString() + "'").collect(Collectors.toList()))
			: String.join(",", this.values.stream().map(Object::toString).collect(Collectors.toList()));
	}

	@Override
	public String toString() {
		return format("%s NOT IN [%s]", firstOperandString, valueString);
	}
	
}
