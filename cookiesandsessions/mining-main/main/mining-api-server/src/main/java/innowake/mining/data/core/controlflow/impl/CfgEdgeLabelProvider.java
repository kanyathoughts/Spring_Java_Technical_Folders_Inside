/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves labels for control flow edges.
 */
public interface CfgEdgeLabelProvider {

	/**
	 * Determines the label text of a control flow edge.
	 * This may extract information from the of the actual node e.g. the condition of a branch.
	 * 
	 * @param node the node used to determine the label text
	 * @return the label for the control flow edge
	 */
	@Nullable
	String resolveLabelFor(AstNodePojo node);
	
	/**
	 * Determines the default label text of a control flow edge.
	 * This is only determined by the type of node e.g. "false" for the else branch of a if statements.
	 * 
	 * @param node the node used to determine the label text
	 * @return the label for the control flow edge
	 */
	@Nullable
	String resolveDefaultLabelFor(AstNodePojo node);

}
