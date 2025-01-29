/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.HashSet;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow for GO TO statements.
 */
public class GoToResolver extends AbstractCobolControlFlowResolver {
	
	boolean calculate = false;
	
	GoToResolver(final AstNodePojo source, final CobolControlFlowContext context, @Nullable final AstNodePojo target, @Nullable String label) {
		super(new HashSet<>(), context);
		if (target != null) {
			addControlFlowEdge(source, target, label);
			if (context.shouldGoTo(target)) {
				next.add(target);
				calculate = true;
				
			}
		} else {
			context.getFinalResult().addErrorStatement(Tuple2.of(source, "Target could not be resolved."));
		}
	}
	
	@Override
	protected ControlFlowSubResult<DefaultStatement> createControlFlow() {
		if (calculate) {
			return super.createControlFlow();
		}
		return result;
	}
	
}
