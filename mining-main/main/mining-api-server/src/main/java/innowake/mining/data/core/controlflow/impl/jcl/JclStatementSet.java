/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.StatementSet;

/**
 * Statement set for JCL statements.
 */
public class JclStatementSet implements StatementSet<DefaultStatement> {

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
