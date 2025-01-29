/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import innowake.mining.data.core.JclAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractCalculateControlFlowTest;
import innowake.mining.data.core.storeast.impl.AbstractJclTest;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.Technology;

/**
 * Base class for JCL control flow tests.
 */
public abstract class JclCalculateControlFlowTest extends AbstractCalculateControlFlowTest {

	public JclCalculateControlFlowTest() {
		super(new AbstractJclTest() {}, Technology.JCL);
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
		if ( ! (superTypes.contains(AstNodeUtils.STATEMENT) || nodeNonNull.getType().equals(JclAstNodeType.JOB)
				|| nodeNonNull.getType().equals(JclAstNodeType.CONDITION))) {
			return new StringBuilder();
		}
		final String type = nodeNonNull.getType();
		final StringBuilder sb = new StringBuilder();
		sb.append(vertexToString(astIndex, nodeNonNull));
		sb.append(" ").append(type);
		if ( ! nodeNonNull.getLabel().isEmpty() && (superTypes.contains(AstNodeUtils.ENTRY) || superTypes.contains(AstNodeUtils.BRANCH_STATEMENT)
				|| superTypes.contains(AstNodeUtils.STATEMENT) || superTypes.contains(AstNodeUtils.CALL_EXTERNAL_STATEMENT))
				|| nodeNonNull.getType().equals(JclAstNodeType.CONDITION)) {
			sb.append(", Label: ").append(nodeNonNull.getLabel());
		}
		return sb;
	}
}
