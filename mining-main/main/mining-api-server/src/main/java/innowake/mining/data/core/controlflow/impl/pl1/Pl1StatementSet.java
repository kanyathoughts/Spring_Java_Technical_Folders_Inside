/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.StatementSet;

/**
 * Statement set for Pl1 statements.
 */
public class Pl1StatementSet implements StatementSet<DefaultStatement> {

	private final Set<DefaultStatement> statements = new HashSet<>();

	@Override
	public Iterator<DefaultStatement> iterator() {
		return statements.iterator();
	}

	@Override
	public void add(final DefaultStatement statement) {
		/* adding statements to set which are not present */
		statements.add(statement);
	}

	@Override
	public boolean isEmpty() {
		return statements.isEmpty();
	}

}
