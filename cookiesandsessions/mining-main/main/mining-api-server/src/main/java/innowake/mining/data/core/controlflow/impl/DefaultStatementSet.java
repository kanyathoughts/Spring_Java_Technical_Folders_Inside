/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * The default case of a StatementSet for control flow calculation using default Statements.
 * @param <S> the Statement type
 */
public class DefaultStatementSet<S extends DefaultStatement> implements StatementSet<S> {

	final Set<S> statementSet = new HashSet<>();

	@Override
	public Iterator<S> iterator() {
		return statementSet.iterator();
	}

	@Override
	public void add(S statement) {
		statementSet.add(statement);
	}

	@Override
	public boolean isEmpty() {
		return statementSet.isEmpty();
	}
	
}
