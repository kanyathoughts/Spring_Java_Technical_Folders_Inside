/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractCalculateControlFlowTest;
import innowake.mining.data.core.storeast.impl.AbstractCobolTest;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.Technology;
import innowake.ndt.core.parsing.ast.model.Invocable;

/**
 * Base class for Cobol control flow tests.
 */
public abstract class CobolCalculateControlFlowTest extends AbstractCalculateControlFlowTest {

	public CobolCalculateControlFlowTest() {
		super(new AbstractCobolTest() {}, Technology.COBOL);
	}

	protected void doTest(final String folderName, final String testName) {
		super.doTest(folderName, testName, testName);
	}
	
	@Override
	protected Optional<AstNodePojo> extractStartNodeForPrinting(final AstNodePojo rootNode) {
		return new AstNodeCollector(n -> "ProcedureDivision".equals(n.getType())).first(rootNode);
	}

	@Override
	protected StringBuilder languageSpecificNodeHandling(final Map<AstNodePojo, Long> astIndex, final AstNodePojo nodeNonNull) {
		final Set<String> superTypes = nodeNonNull.getSuperTypes();
		final String type = nodeNonNull.getType();
		final StringBuilder sb = new StringBuilder()
				.append(vertexToString(astIndex, nodeNonNull))
				.append(" ").append(type);
		if (superTypes.contains(AstNodeUtils.BRANCH_STATEMENT)) {
			sb.append(" <branch stmt>");
		}
		
		switch (CobolAstNodeType.fromString(type)) {
			case DISPLAY:
				sb.append(" ")
					.append((new ControlflowNodeChildrenToString()).traverse(nodeNonNull));
				break;
			case LABEL:
				final String lablelName = (String) nodeNonNull.getProperties().get(Invocable.INVOCATION_NAME);
				sb.append(" ")
					.append(lablelName)
					.append(" ");
				break;
			case PERFORM:
				final String performTarget = (String) nodeNonNull.getProperties().get("target");
				final String performThru = (String) nodeNonNull.getProperties().get("thru");
				if (performTarget != null) {
					sb.append(" ")
						.append(performTarget);
					if (performThru != null) {
						sb.append(" THRU ")
						.append(performThru)
						.append(" ");
					}
				}
				break;
			case GO_TO:
				final List<?> goToTargetes = (List<?>) nodeNonNull.getProperties().get("targets");
				goToTargetes.forEach(t -> sb.append(" ").append(t.toString()).append(" "));
				break;
			default:
				if ( ! nodeNonNull.getLabel().isEmpty() && (superTypes.contains(AstNodeUtils.BRANCH_STATEMENT) || superTypes.contains(AstNodeUtils.LOOP_STATEMENT))) {
					sb.append(", Label: ").append(nodeNonNull.getLabel());
				}
		}
		return sb;
	}
}

class ControlflowNodeChildrenToString extends AbstractTraverser<AstNodePojo, StringBuilder> {

	private final StringBuilder stringBuilder = new StringBuilder();
	
	/**
	 * Constructor.
	 */
	public ControlflowNodeChildrenToString() {
		super(node -> node.getChildren());
	}
	
	@Override
	protected StringBuilder visit(final AstNodePojo node) {
		final StringBuilder result = stringBuilder;
		if (CobolAstNodeType.CONSTANT_REFERENCE.getType().equals(node.getType())) {
			final String value = (String) node.getProperties().get("value");
			result.append(value);
		}
		return result;
	}
}