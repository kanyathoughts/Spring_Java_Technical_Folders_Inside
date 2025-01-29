/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.collections4.set.UnmodifiableSet;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.Logging;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
. * Contains the statements where following the control flow ended.
 */
public class ControlFlowResultImpl implements ControlFlowResult {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CFG);
	private final Set<AstNodePojo> returnStatements = new HashSet<>();
	private final Set<AstNodePojo> haltStatements = new HashSet<>();
	private final Set<Tuple2<AstNodePojo, String> > errorStatements = new HashSet<>();
	
	@Override
	public void addReturnStatement(final AstNodePojo returnStatement) {
		LOG.trace(() -> String.format(
				"The node at location %s of type %s is being added to return statement during CFG calculation.",
				returnStatement.getLocation(), returnStatement.getType()));
		returnStatements.add(returnStatement);
	}

	@Override
	public void addHaltStatement(final AstNodePojo haltStatement) {
		LOG.trace(() -> String.format(
				"The node at location %s of type %s is being added to halt statement during CFG calculation.",
				haltStatement.getLocation(), haltStatement.getType()));
		haltStatements.add(haltStatement);
	}

	@Override
	public Set<AstNodePojo> getReturnStatements() {
		return UnmodifiableSet.unmodifiableSet(returnStatements);
	}
	
	@Override
	public Set<AstNodePojo> getHaltStatements() {
		return UnmodifiableSet.unmodifiableSet(haltStatements);
	}
	
	@SuppressWarnings("null")
	@Override
	public void addErrorStatement(final Tuple2<AstNodePojo, String>  error) {
		LOG.error(() -> String.format(
				"The following error is encountered during CFG calculation:"
						+ " %s with location as %s and type as %s",
				error.b, error.a != null ? error.a.getLocation() : null, error.a != null ? error.a.getType() : null));
		errorStatements.add(error);
	}

	@Override
	public Set<Tuple2<AstNodePojo, String>> getErrorStatements() {
		return UnmodifiableSet.unmodifiableSet(errorStatements);
	}
}
