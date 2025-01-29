/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.StatementSet;

/**
 * Statement Set for Java
 */
public class JavaStatementSet implements StatementSet<DefaultStatement> {
	
	private final Set<DefaultStatement> statements = new HashSet<>();

	@Override
	public Iterator<DefaultStatement> iterator() {
		return statements.iterator();
	}

	@Override
	public void add(final DefaultStatement statement) {
		statements.add(statement);
	}

	@Override
	public boolean isEmpty() {
		return statements.isEmpty();
	}
}
