/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractCalculateControlFlowTest;
import innowake.mining.data.core.storeast.impl.AbstractPl1Test;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.Technology;

/**
 * Base class for Pl1 control flow tests.
 */
public abstract class Pl1CalculateControlFlowTest extends AbstractCalculateControlFlowTest {

	public Pl1CalculateControlFlowTest() {
		super(new AbstractPl1Test() {}, Technology.PL1);
	}

	protected void doTest(final String folderName, final String testName) {
		super.doTest(folderName, testName, testName);
	}
	
	@Override
	protected Optional<AstNodePojo> extractStartNodeForPrinting(final AstNodePojo rootNode) {
		return Optional.ofNullable(rootNode);
	}

	@Override
	protected StringBuilder languageSpecificNodeHandling(final Map<AstNodePojo, Long> astIndex, final AstNodePojo nodeNonNull) {
		final Set<String> superTypes = nodeNonNull.getSuperTypes();
		if ( ! superTypes.contains(AstNodeUtils.STATEMENT)) {
			return new StringBuilder();
		}
		final String type = nodeNonNull.getType();
		final StringBuilder sb = new StringBuilder()
				.append(vertexToString(astIndex, nodeNonNull));
		if (type.equals("WriteStmt")) {
			sb.append(" ").append(nodeNonNull.getLabel());
		} else {
			sb.append(" ").append(type);
		}
		if ( ! nodeNonNull.getLabel().isEmpty() && (superTypes.contains(AstNodeUtils.BRANCH_STATEMENT)
													|| superTypes.contains(AstNodeUtils.LOOP_STATEMENT)
													|| superTypes.contains(AstNodeUtils.LOOP_EVENT_STATEMENT))) {
			sb.append(", Label: ").append(nodeNonNull.getLabel());
		}
		return sb;
	}
}
