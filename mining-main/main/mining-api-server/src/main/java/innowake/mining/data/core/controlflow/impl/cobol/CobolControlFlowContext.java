/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowResultImpl;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Context object for control flow resolver
 */
public class CobolControlFlowContext {

	private final CobolOutOfScopeEvaluator scopeEvaluator;
	private final CobolJumpStatementStorage jumpStorage;
	@Nullable
	private final AstNodePojo returnLabel;
	private final List<ControlFlowPrototype> controlFlowEdges;
	private final ControlFlowResult finalResult;
	@Nullable
	private final AstNodePojo currentPerform;
	
	/**
	 * Constructor
	 * 
	 * @param jumpStorage contains all jump statements of the program
	 */
	CobolControlFlowContext(final CobolJumpStatementStorage jumpStorage) {
		this.scopeEvaluator = (c, n) -> Optional.empty();
		this.jumpStorage = jumpStorage;
		controlFlowEdges = new ArrayList<>();
		finalResult = new ControlFlowResultImpl();
		this.returnLabel = null;
		this.currentPerform = null;
	}

	private CobolControlFlowContext(final CobolOutOfScopeEvaluator scopeEvaluator, 
			final CobolJumpStatementStorage jumpStorage, 
			@Nullable final AstNodePojo returnLabel, 
			@Nullable final AstNodePojo currentPerform,
			final List<ControlFlowPrototype> controlFlowEdges, 
			final ControlFlowResult result) {
		this.scopeEvaluator = scopeEvaluator;
		this.returnLabel = returnLabel;
		this.jumpStorage = jumpStorage;
		this.controlFlowEdges = controlFlowEdges;
		this.finalResult = result;
		this.currentPerform = currentPerform;
	}
	
	void addControlFlowEdge(final ControlFlowPrototype edge) {
		for (final var existingEdge : controlFlowEdges) {
			if (Objects.equals(existingEdge.src.get(), edge.src.get()) && Objects.equals(existingEdge.dst.get(), edge.dst.get())) {
				if (Objects.equals(existingEdge.label.orElse(null), edge.label.orElse(null))) {
					return;
				}
				if (existingEdge.label.isPresent() != edge.label.isPresent()) {
					existingEdge.setLabel(null);
					return;
				}
			}
		}
		controlFlowEdges.add(edge);
	}
	
	List<ControlFlowPrototype> getControlFlowEdges() {
		return controlFlowEdges;
	}

	CobolOutOfScopeEvaluator getScopeEvaluator() {
		return scopeEvaluator;
	}

	Optional<Tuple2<Optional<AstNodePojo>, Optional<AstNodePojo>>> getPerformTarget(final AstNodePojo perform) {
		return jumpStorage.getPerformTarget(perform);
	}
	
	Optional<List<AstNodePojo>> getGoToTargets(final AstNodePojo goTo) {
		return jumpStorage.getGoToTargets(goTo);
	}

	/**
	 * Returns the result that is filled during the calculation of the control flow.
	 *
	 * @return the final result
	 */
	ControlFlowResult getFinalResult() {
		return finalResult;
	}
	
	@Nullable
	ControlFlowSubResult<DefaultStatement> getPerformResult(final Tuple2<AstNodePojo, Optional<AstNodePojo>> target) {
		return jumpStorage.getPerformResult(target);
	}
	
	boolean shouldPerform(final AstNodePojo perfrom) {
		if (jumpStorage.shouldVisitPerform(perfrom)) {
			jumpStorage.addHandledPerform(perfrom);
			return true;
		}
		return false;
	}
	
	boolean shouldGoTo(final AstNodePojo target) {
		if (jumpStorage.shouldVisitGoTo(target, currentPerform)) {
			jumpStorage.addHandledGoTo(target, currentPerform);
			return true;
		}
		return false;
	}
	
	void addPerformResult(final ControlFlowSubResult<DefaultStatement> performResult, final Tuple2<AstNodePojo, Optional<AstNodePojo>> target) {
		jumpStorage.addPerformResult(performResult, target);
	}

	CobolJumpStatementStorage getJumpStorage() {
		return jumpStorage;
	}

	@Nullable
	AstNodePojo getReturnLabel() {
		return returnLabel;
	}
	
	Optional<AstNodePojo> isOutOfScope(final AstNodePojo currentNode, final AstNodePojo nextNode) {
		return scopeEvaluator.getLastBlockIfOutOfScope(currentNode, nextNode);
	}
	
	CobolControlFlowContext createSubContext(final CobolOutOfScopeEvaluator newScopeEvaluator,
			 @Nullable final AstNodePojo newReturnLabel, final AstNodePojo currentPerform) {
		return new CobolControlFlowContext(newScopeEvaluator, jumpStorage, newReturnLabel, currentPerform, controlFlowEdges, finalResult);
	}
	
	CobolControlFlowContext createSubContext(final CobolOutOfScopeEvaluator newScopeEvaluator, final AstNodePojo newReturnLabel) {
		return new CobolControlFlowContext(newScopeEvaluator, jumpStorage, newReturnLabel, currentPerform, controlFlowEdges, finalResult);
	}
	
}
