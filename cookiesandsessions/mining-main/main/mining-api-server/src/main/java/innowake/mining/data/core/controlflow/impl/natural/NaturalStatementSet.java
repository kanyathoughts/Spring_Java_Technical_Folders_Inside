/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.Iterator;
import java.util.Map;

import org.apache.commons.collections4.map.HashedMap;

import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Statement set for Natural statements.
 */
public class NaturalStatementSet implements StatementSet<NaturalStatement>{
	
	final Map<AstNodePojo, NaturalStatement> statements = new HashedMap<>();

	@Override
	public Iterator<NaturalStatement> iterator() {
		return statements.values().iterator();
	}

	@Override
	public void add(final NaturalStatement statement) {
		final NaturalStatement alreadyPresentStatement = statements.computeIfAbsent(statement.getAstNode(), k -> statement);
		if (alreadyPresentStatement != statement) {
			alreadyPresentStatement.mergeLastInputStatementNodes(statement.getLastInputStatementNodes());
		}
	}

	@Override
	public boolean isEmpty() {
		return statements.isEmpty();
	}
	
}
