/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.HashSet;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow for PERFORM LABEL / PERFORM LABEL THRU LABEL
 */
public class PerformLabelResolver extends AbstractCobolControlFlowResolver {
	
	private boolean calculate = false;
	private final Tuple2<AstNodePojo, Optional<AstNodePojo>> target;
	
	PerformLabelResolver(final AstNodePojo currentNode, @Nullable final String label, final CobolControlFlowContext context,
			final Tuple2<AstNodePojo, Optional<AstNodePojo>> target) {
		super(new HashSet<>(), createContext(context, target.b, currentNode));
		this.target = target;
		addControlFlowEdge(currentNode, target.a, label);
		final ControlFlowSubResult<DefaultStatement> performResult = context.getPerformResult(target);
		if (performResult == null) {
			if (context.shouldPerform(currentNode)) {
				next.add(target.a);
				calculate = true;
			}
		} else {
			result = performResult;
		}
	}
	
	@Override
	protected ControlFlowSubResult<DefaultStatement> createControlFlow() {
		if (calculate) {
			final ControlFlowSubResult<DefaultStatement> performResult = super.createControlFlow();
			context.addPerformResult(performResult, target);
			return performResult;
		}
		return result;
	}
	
	private static CobolControlFlowContext createContext(final CobolControlFlowContext superContext, final Optional<AstNodePojo> thru, final AstNodePojo perform) {
		return superContext.createSubContext(new PerformOutOfScopeEvaluator(thru.orElse(null)), thru.orElse(null), perform);
	}
	
}
