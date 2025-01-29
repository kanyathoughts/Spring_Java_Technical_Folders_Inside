/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.controlflow.impl.CfgEdgeLabelProvider;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolve edges for Control Flow Edges of a Java Program.
 */
public class JavaLabelResolver implements CfgEdgeLabelProvider {

	@Override
	@Nullable
	public String resolveLabelFor(final AstNodePojo node) {
		return null;
	}

	@Override
	@Nullable
	public String resolveDefaultLabelFor(AstNodePojo node) {
		return null;
	}
}
