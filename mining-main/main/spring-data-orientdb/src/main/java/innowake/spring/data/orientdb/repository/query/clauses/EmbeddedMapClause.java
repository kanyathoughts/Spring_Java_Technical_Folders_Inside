/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query.clauses;

import static java.lang.String.format;

import com.github.raymanrt.orientqb.query.Assignable;
import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.util.Commons;

/**
 * Build a criteria for a embedded map field.
 * Example : embeddedMap['Key'] = 'Value'
 */
public class EmbeddedMapClause extends Clause {

	private final Object firstOperand;
	private final String key;
	private final Object value;

	/**
	 * Instantiates a clause for type embedded map.
	 * 
	 * @param field name of the embedded map field in entity class
	 * @param key the key value of the map
	 * @param value the value saved with the given key
	 */
	public EmbeddedMapClause(final String field, final String key, final Object value) {
		this.firstOperand = field;
		this.key = key;
		this.value = value;
	}

	@Override
	public String toString() {
		String valueString = Commons.cast(value);
		if (value instanceof Assignable) {
			final Assignable assignable = (Assignable) value;
			valueString = assignable.getAssignment();
		}

		return format("%s['%s'] = %s", firstOperand, key, valueString);
	}

}
