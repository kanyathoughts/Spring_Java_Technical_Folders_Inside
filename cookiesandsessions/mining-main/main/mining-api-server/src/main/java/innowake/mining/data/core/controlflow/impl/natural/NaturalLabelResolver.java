/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.NaturalAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.CfgEdgeLabelProvider;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves labels for control flow edges of a Natural program.
 */
public class NaturalLabelResolver implements CfgEdgeLabelProvider {

	public static final String TRUE = "TRUE";
	public static final String FALSE = "FALSE";
	private static final String NONE = "NONE";
	private static final String LOOP_BODY = "LOOP BODY";
	private static final String LOOP_END = "LOOP END";
	private static final String IF_NO_RECORDS = "IF NO RECORDS FOUND";
	private static final String START_OF_DATA = "AT START OF DATA";
	private static final String END_OF_DATA = "AT END OF DATA";
	private static final String BEFORE_BREAK = "BEFORE BREAK PROCESSING";
	private static final String WHEN_ANY = "WHEN ANY";
	private static final String WHEN_ALL = "WHEN ALL";
	
	@Nullable
	@Override
	public String resolveLabelFor(final AstNodePojo node) {
		final String type = node.getType();
		if (NaturalAstNodeType.THEN_BRANCH.equals(type)) {
			return TRUE;
		} else if (NaturalAstNodeType.ELSE_BRANCH.equals(type)) {
			return FALSE;
		} else if (NaturalAstNodeType.IF_NO_RECORDS.equals(type)) {
			return IF_NO_RECORDS;
		} else if (NaturalAstNodeType.START_OF_DATA.equals(type)) {
			return START_OF_DATA;
		} else if (NaturalAstNodeType.END_OF_DATA.equals(type)) {
			return END_OF_DATA;
		} else if (NaturalAstNodeType.BEFORE_BREAK.equals(type)) {
			return BEFORE_BREAK;
		} else if (NaturalAstNodeType.BREAK.equals(type)) {
			return node.getLabel();
		} else if (NaturalAstNodeType.DECIDE_ON_CLAUSE.equals(type)) {
			return node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH) ? NONE : node.getChildren().get(0).getLabel();
		} else if (NaturalAstNodeType.DECIDE_FOR_CLAUSE.equals(type)) {
			if (node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH)) {
				return NONE;
			} else {
				final AstNodePojo whenCondition = node.getChildren().get(0);
				switch (whenCondition.getType()) {
					case NaturalAstNodeType.WHEN_ANY:
						return WHEN_ANY;
					case NaturalAstNodeType.WHEN_ALL:
						return WHEN_ALL;
					default:
						return whenCondition.getChildren().get(0).getLabel();
				}
			}
			
		} else if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT)) {
			return LOOP_BODY;
		}
		return null;
	}

	@Nullable
	@Override
	public String resolveDefaultLabelFor(final AstNodePojo node) {
		if (isIfStatement(node.getType())) {
			return FALSE;
		} else if (node.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT)) {
			return LOOP_END;
		}
		return null;
	}
	
	private boolean isIfStatement(final String type) {
		return NaturalAstNodeType.IF_STMT.equals(type) || NaturalAstNodeType.IF_SELECTION_STMT.equals(type);
	}
}
