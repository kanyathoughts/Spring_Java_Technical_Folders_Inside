/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import org.junit.Assert;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.AstTestUtil;
import innowake.mining.data.core.storeast.impl.AbstractCobolTest;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * Base class for annotation identification tests.
 */
public class AnnotationIdentificationTest extends AbstractCobolTest {

	@Override
	protected String nodeToString(@Nullable final StoreAstPrototype node) {
		Assert.assertNotNull(node);
		final StringBuilder sb = new StringBuilder();
		if ("CobolCopyStmt".equals(node.type.orElse(null))) {
			sb.append("\n");
		}
		return AstTestUtil.nodeToString(sb, node).toString();
	}

}
