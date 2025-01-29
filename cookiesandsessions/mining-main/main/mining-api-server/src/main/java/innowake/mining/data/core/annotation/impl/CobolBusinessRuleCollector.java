/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Cobol specific business rule collector.
 * <p>This collector is <b>stateful</b> for performance reasons, see method {@code #getFieldDefinitions()}.</p>
 */
class CobolBusinessRuleCollector extends BusinessRuleCollector {

	@Override
	AstNodeCollectingTraverser createBrBranchNodeTraverser() {
		return new AstNodeCollectingTraverser(AstNodeUtils::isBranchStatementNode) {

			@Override
			public AstNodePojo traverse(final AstNodePojo node) {
				return ! AstNodeUtils.isInclusionNode(node) && ! AstNodeUtils.isCobolExitParagraph(node) ? super.traverse(node) : node;
			}
		};
	}
}
