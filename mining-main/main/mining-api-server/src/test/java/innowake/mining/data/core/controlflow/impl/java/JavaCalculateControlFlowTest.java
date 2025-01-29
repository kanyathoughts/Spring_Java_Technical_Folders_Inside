/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import innowake.mining.data.core.JavaAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractCalculateControlFlowTest;
import innowake.mining.data.core.storeast.impl.AbstractJavaTest;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.Technology;

/**
 * Base class for Java control flow tests.
 */
public abstract class JavaCalculateControlFlowTest extends AbstractCalculateControlFlowTest {

	public JavaCalculateControlFlowTest() {
		super(new AbstractJavaTest() {}, Technology.JAVA);
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
		if (!(superTypes.contains(AstNodeUtils.STATEMENT) || nodeNonNull.getType().equals(JavaAstNodeType.FUNCTION_DEFINITION))) {
			return new StringBuilder();
		}
		final String type = nodeNonNull.getType();
		final StringBuilder sb = new StringBuilder();
		sb.append(vertexToString(astIndex, nodeNonNull));
		sb.append(" ").append(type);
		if ( ! nodeNonNull.getLabel().isEmpty() && (superTypes.contains(AstNodeUtils.BRANCH_STATEMENT) || superTypes.contains(AstNodeUtils.LOOP_STATEMENT)
				|| superTypes.contains(AstNodeUtils.LOOP_EVENT_STATEMENT))) {
			sb.append(", Label: ").append(nodeNonNull.getLabel());
		}
		return sb;
	}
}
