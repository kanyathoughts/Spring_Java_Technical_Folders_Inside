/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.CAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.CfgEdgeLabelProvider;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves labels for control flow edges of a C program.
 */
public class CLabelResolver implements CfgEdgeLabelProvider {

	public static final String FALSE = "FALSE";
	public static final String TRUE = "TRUE";
	public static final String DEFAULT = "DEFAULT";
	private static final String LOOP_BODY = "LOOP BODY";
	private static final String LOOP_END = "LOOP END";
	private static final String FUNCTION_CALL = "CALL";
	private static final String GOTO = "JUMP";

	@Override
	@Nullable
	public String resolveLabelFor(final AstNodePojo node) {
		final String type = node.getType();
		if (CAstNodeType.THEN_BRANCH.equals(type)) {
			return TRUE;
		} else if (CAstNodeType.ELSE_BRANCH.equals(type)) {
			return FALSE;
		} else if (CAstNodeType.DEFAULT_BRANCH.equals(type)) {
			return DEFAULT;
		} else if (CAstNodeType.CASE_BRANCH.equals(type)) {
			return node.getChildren().get(0).getLabel();
		} else if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT)) {
			return LOOP_BODY;
		} else if (CAstNodeType.FUNCTION_DEFINITION.equals(type)) {
			return FUNCTION_CALL;
		} else if (CAstNodeType.JUMP_STATEMENT.equals(type)) {
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
		return CAstNodeType.IFELSE_STATEMENT.equals(type);
	}
}
