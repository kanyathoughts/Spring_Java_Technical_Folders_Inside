/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the edge labels for certain AST nodes
 */
public final class CobolLabelResolver {
	
	private CobolLabelResolver() {
		/* should not be instantiated. */
	}
	
	public static final String TRUE = "TRUE";
	public static final String FALSE = "FALSE";
	public static final String ON_SIZE_ERROR = "ON SIZE ERROR";
	public static final String NOT_ON_SIZE_ERROR = "NOT ON SIZE ERROR";
	public static final String INVALID_KEY = "INVALID KEY";
	public static final String NOT_INVALID_KEY = "NOT INVALID KEY";
	public static final String AT_END = "AT END";
	public static final String NOT_AT_END = "NOT AT END";
	public static final String OTHER = "OTHER";
	public static final String PERFORM_LOOP = "EXECUTE";
	public static final String EXIT_PERFORM_LOOP = "EXIT LOOP";

	private static final String IS_NOT = "isNot";
	
	/**
	 * Resolves the label for a given AST node
	 * @param node the  AST node
	 * @return the label
	 */
	@Nullable
	public static String resolveLabelFor(final AstNodePojo node) {
		switch (CobolAstNodeType.fromString(node.getType())) {
			case THEN:
				return TRUE;
			case ELSE:
				return FALSE;
			case WHEN:
				return createWhenLabel(node);
			case AT_END:
				if (Boolean.parseBoolean(node.getProperties().get(IS_NOT).toString())) {
					return NOT_AT_END;
				} else {
					return AT_END;
				}
			case INVALID_KEY:
				if (Boolean.parseBoolean(node.getProperties().get(IS_NOT).toString())) {
					return NOT_INVALID_KEY;
				} else {
					return INVALID_KEY;
				}
			case ON_SIZE_ERROR:
				if (Boolean.parseBoolean(node.getProperties().get(IS_NOT).toString())) {
					return NOT_ON_SIZE_ERROR;
				} else {
					return ON_SIZE_ERROR;
				}
			case PERFORM:
				return PERFORM_LOOP;
			default:
				return null;
		}
	}
	
	/**
	 * Resolves the default label for a given AST node
	 * The default label is the label used for the default branch of a statement that contains the given  AST node
	 * 
	 * @param node the  AST nodes
	 * @return the default label
	 */
	@Nullable
	public static String resolveDefaultLabelFor(final AstNodePojo node) {
		switch (CobolAstNodeType.fromString(node.getType())) {
			case IF:
				return FALSE;
			case GO_TO:
				return OTHER;
			case AT_END:
				if (AstNodeUtils.isTrue(node, IS_NOT)) {
					return AT_END;
				} else {
					return NOT_AT_END;
				}
			case INVALID_KEY:
				if (AstNodeUtils.isTrue(node, IS_NOT)) {
					return INVALID_KEY;
				} else {
					return NOT_INVALID_KEY;
				}
			case ON_SIZE_ERROR:
				if (AstNodeUtils.isTrue(node, IS_NOT)) {
					return ON_SIZE_ERROR;
				} else {
					return NOT_ON_SIZE_ERROR;
				}
			case PERFORM:
				return EXIT_PERFORM_LOOP;
			default:
				return null;
		}
	}
	
	private static String createWhenLabel(final AstNodePojo node) {
		if (node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH)) {
			return OTHER;
		}
		String label = node.getLabel();
		label = label.replaceFirst("(?i)when", "");
		label = label.replaceAll("\\R", " ");
		return label.trim();
	}
}
