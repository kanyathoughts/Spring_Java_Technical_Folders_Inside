/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import java.util.List;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationMetadata;
import innowake.ndt.naturalparser.ast.Statement.IfSelectionStmt;

/**
 * Natural specific business rule collector.
 * <p>This collector is <b>stateful</b> for performance reasons, see method {@code #getFieldDefinitions()}.</p>
 */
class NaturalBusinessRuleCollector extends BusinessRuleCollector {
	
	@Override
	void checkForIfElseCondition(final List<AnnotationMetadata> candidates, final AstNodePojo node, final AstNodePojo root) {
		/* For Natural IF SELECTION is no real condition so the fields used for selection are returned as conditions
		 * so we need a special handling to count the field references */
		final List<AstNodePojo> conditions = AstNodeUtils.getBranchConditions(node);
		if (conditions.size() > 1 && node.getSuperTypes().contains(BRANCH_STATEMENT) && IfSelectionStmt.class.getSimpleName().equals(node.getType())
				&& conditions.stream().filter(condition -> condition.getSuperTypes().contains(FIELD_REFERENCE)).count() > 1) {
			candidates.add(new AnnotationMetadata(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
		} else {
			super.checkForIfElseCondition(candidates, node, root);
		}
	}
}
