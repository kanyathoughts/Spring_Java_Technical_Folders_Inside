/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.api.query.clause;

import static com.github.raymanrt.orientqb.util.Commons.whereToStringFunction;
import static com.github.raymanrt.orientqb.util.Joiner.andJoiner;
import static com.google.common.collect.Lists.newArrayList;
import static com.google.common.collect.Lists.transform;

import java.util.List;
import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.clause.CompositeClause;
import com.google.common.base.Function;
import com.google.common.base.Joiner;

/**
 * Adjusted {@link CompositeClause} for Orient DB which doesn't add brackets around nested {@ode AND} clauses.
 */
public class OrientCompositeClause extends Clause {
	
	private static final Function<Clause, String> toStringFunction = input -> {
		if (input instanceof OrientCompositeClause) {
			/* we don't need brackets if the nested clause input is of type OrientCompositeClause with an AND joiner */
        	final String string = input.toString();
        	return ((OrientCompositeClause) input).joiner == andJoiner ? string : "(" + string + ")";
        }

		/* fall back to default function which the CompositeClause uses too */
        return whereToStringFunction.apply(input);
	};

	private final Joiner joiner;
    private final Clause[] clauses;

    /**
     * Constructor
     * 
     * @param joiner The {@link Joiner} to use for the clauses
     * @param clauses The nested {@link Clause Clauses}
     */
    public OrientCompositeClause(final Joiner joiner, final Clause... clauses) {
        this.joiner = joiner;
        this.clauses = clauses;
    }

    @Override
    public String toString() {
        final List<String> clausesList = transform(newArrayList(clauses), toStringFunction);
        return joiner.join(clausesList);
    }
}