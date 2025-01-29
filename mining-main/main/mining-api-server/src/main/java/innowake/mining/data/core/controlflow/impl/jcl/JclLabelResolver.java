/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import innowake.lib.core.lang.Nullable;
import innowake.mining.data.core.JclAstNodeType;
import innowake.mining.data.core.controlflow.impl.CfgEdgeLabelProvider;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves labels for control flow edges of a JCL program.
 */
public class JclLabelResolver implements CfgEdgeLabelProvider {
	public static final String FALSE = "FALSE";
	public static final String TRUE = "TRUE";

	@Override
	@Nullable
	public String resolveLabelFor(final AstNodePojo node) {
		final String type = node.getType();
		if (JclAstNodeType.IF_BRANCH.equals(type)) {
			return TRUE;
		} else if (JclAstNodeType.ELSE_BRANCH.equals(type)) {
			return FALSE;
		}
		return null;
	}

	@Override
	@Nullable
	public String resolveDefaultLabelFor(final AstNodePojo node) {
		if (isIfStatement(node.getType())) {
			return FALSE;
		}
		return null;
	}

	private boolean isIfStatement(final String type) {
		return JclAstNodeType.STEP_IF.equals(type);
	}
}
