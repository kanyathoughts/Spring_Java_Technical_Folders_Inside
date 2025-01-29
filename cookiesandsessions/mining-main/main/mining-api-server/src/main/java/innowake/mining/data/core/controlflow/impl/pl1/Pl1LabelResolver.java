/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.Pl1AstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.CfgEdgeLabelProvider;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves labels for control flow edges of a Pl1 program.
 */
public class Pl1LabelResolver implements CfgEdgeLabelProvider {

	public static final String FALSE = "FALSE";
	public static final String TRUE = "TRUE";
	public static final String OTHERWISE = "OTHERWISE";
	private static final String LOOP_BODY = "LOOP BODY";
	private static final String LOOP_END = "LOOP END";
	private static final String FUNCTION_CALL = "CALL";
	private static final String GOTO = "JUMP";

	@Override
	@Nullable
	public String resolveLabelFor(final AstNodePojo node) {
		final String type = node.getType();
		if (Pl1AstNodeType.THEN_BRANCH.equals(type)) {
			return TRUE;
		} else if (Pl1AstNodeType.ELSE_BRANCH.equals(type)) {
			return FALSE;
		} else if (Pl1AstNodeType.OTHERWISE_STMT.equals(type)) {
			return OTHERWISE;
		} else if (Pl1AstNodeType.WHEN_STMT.equals(type)) {
			// condition will always be at 3rd children in case of WhenStatement node
			return node.getChildren().get(2).getLabel();
		} else if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT)) {
			return LOOP_BODY;
		} else if (Pl1AstNodeType.PROCEDURE_BLOCK.equals(type)) {
			return FUNCTION_CALL;
		} else if (node.getSuperTypes().contains(AstNodeUtils.JUMP_STATEMENT)) {
			return GOTO;
		}
		return null;
	}

	@Override
	@Nullable
	public String resolveDefaultLabelFor(final AstNodePojo node) {
		if (isIfStatement(node.getType())) {
			return FALSE;
		} else if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT)) {
			return LOOP_END;
		}
		return null;
	}

	private boolean isIfStatement(final String type) {
		return Pl1AstNodeType.IF_STMT.equals(type);
	}
}
