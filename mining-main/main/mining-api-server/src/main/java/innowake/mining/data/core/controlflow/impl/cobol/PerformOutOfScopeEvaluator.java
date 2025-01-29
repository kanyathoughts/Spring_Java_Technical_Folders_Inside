/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.Optional;

import graphql.com.google.common.base.Objects;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Evaluator for the scope of PERFPRM LABEL (THRU LABEL) statements.
 */
class PerformOutOfScopeEvaluator implements CobolOutOfScopeEvaluator {
	
	@Nullable
	private final AstNodePojo returnNode;
	
	PerformOutOfScopeEvaluator(@Nullable final AstNodePojo returnNode) {
		this.returnNode = returnNode;
	}
	
	@Override
	public Optional<AstNodePojo> getLastBlockIfOutOfScope(final AstNodePojo currentNode, final AstNodePojo nextNode) {
		if (returnNode != null) {
			final String type = nextNode.getType();
			if (CobolAstNodeType.LABEL.getType().equals(type) || CobolAstNodeType.SECTION.getType().equals(type)) {
				final AstNodePojo paragraph = AstNodeUtils.getParagraph(currentNode);
				if (Objects.equal(returnNode, paragraph)) {
					return Optional.of(paragraph);
				}
			}
			if (CobolAstNodeType.SECTION.getType().equals(type)) {
				final AstNodePojo section = AstNodeUtils.getSection(currentNode);
				if (Objects.equal(returnNode, section)) {
					return Optional.of(section);
				}
			}
		}
		return Optional.empty();
	}
	
}
