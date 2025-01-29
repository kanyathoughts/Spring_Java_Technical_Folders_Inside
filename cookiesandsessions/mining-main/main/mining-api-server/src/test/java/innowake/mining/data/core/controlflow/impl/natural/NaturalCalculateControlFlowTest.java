/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractCalculateControlFlowTest;
import innowake.mining.data.core.storeast.impl.AbstractNaturalTest;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.Technology;

/**
 * Base class for all Natural control flow tests.
 */
abstract class NaturalCalculateControlFlowTest extends AbstractCalculateControlFlowTest{

	protected NaturalCalculateControlFlowTest() {
		super(new AbstractNaturalTest(){}, Technology.NATURAL);
	}

	@Override
	protected Optional<AstNodePojo> extractStartNodeForPrinting(AstNodePojo rootNode) {
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
