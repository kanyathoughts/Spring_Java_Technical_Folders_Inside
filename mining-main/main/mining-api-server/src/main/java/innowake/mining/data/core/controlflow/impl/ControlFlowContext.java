/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.Logging;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Context object for control flow resolver
 * @param <S> the type of statements
 * @param <C> the actual type of the control flow context
 */
public class ControlFlowContext<S extends Statement, C extends ControlFlowContext<S, C>> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CFG);
	private final OutOfScopeEvaluator<S> scopeEvaluator;
	private final JumpStatementStorage<S> jumpStorage;
	private final List<ControlFlowPrototype> controlFlowEdges;
	private final ControlFlowResult finalResult;
	private final CfgEdgeLabelProvider labelResolver;
	private final LanguageSpecificHandler<S, C> languageSpecificHandler;
	
	/**
	 * Constructor
	 * 
	 * @param jumpStorage contains all jump statements of the program
	 * @param labelResolver resolves the labels for control flow edges
	 * @param languageSpecificHandler the handler for language specific control flow calculation
	 */
	public ControlFlowContext(final JumpStatementStorage<S> jumpStorage, final CfgEdgeLabelProvider labelResolver, 
			final LanguageSpecificHandler<S, C> languageSpecificHandler) {
		this.scopeEvaluator = (c, n) -> Optional.empty();
		this.labelResolver = labelResolver;
		this.jumpStorage = jumpStorage;
		this.languageSpecificHandler = languageSpecificHandler;
		controlFlowEdges = new ArrayList<>();
		finalResult = new ControlFlowResultImpl();
	}

	private ControlFlowContext(final OutOfScopeEvaluator<S> scopeEvaluator, 
			final JumpStatementStorage<S> jumpStorage, 
			final CfgEdgeLabelProvider labelResolver,
			final List<ControlFlowPrototype> controlFlowEdges, 
			final ControlFlowResult result,
			final LanguageSpecificHandler<S,C> languageSpecificHandler) {
		this.scopeEvaluator = scopeEvaluator;
		this.labelResolver = labelResolver;
		this.jumpStorage = jumpStorage;
		this.controlFlowEdges = controlFlowEdges;
		this.finalResult = result;
		this.languageSpecificHandler = languageSpecificHandler;
	}
	
	/**
	 * Adds a control flow edge.
	 *
	 * @param edge the control flow edge
	 */
	public void addControlFlowEdge(final ControlFlowPrototype edge) {
		LOG.trace(() -> String.format(
			"Adding edge from [at location %s of type %s] to [at location %s of type %s]",
			edge.srcNode.getNonNull().getLocation(),
			edge.srcNode.getNonNull().getType(),
			edge.dstNode.getNonNull().getLocation(),
			edge.dstNode.getNonNull().getType()));
		for (final var existingEdge : controlFlowEdges) {
			if (Objects.equals(existingEdge.src.get(), edge.src.get())
				 && Objects.equals(existingEdge.dst.get(), edge.dst.get())
				 && Objects.equals(existingEdge.label.orElse(null), edge.label.orElse(null))) {
				LOG.trace("Duplicate edge skipped!");
				return;
			}
		}
		controlFlowEdges.add(edge);
	}

	/**
	 * @return all calculated control flow edges
	 */
	public List<ControlFlowPrototype> getControlFlowEdges() {
		return controlFlowEdges;
	}
	
	/**
	 * @return the {@link CfgEdgeLabelProvider}
	 */
	public OutOfScopeEvaluator<S> getScopeEvaluator() {
		return scopeEvaluator;
	}
	
	/**
	 * @return the {@link LanguageSpecificHandler}
	 */
	public LanguageSpecificHandler<S,C> getLanguageSpecificHandler() {
		return languageSpecificHandler;
	}

	/**
	 * Returns the result that is filled during the calculation of the control flow.
	 *
	 * @return the final result
	 */
	public ControlFlowResult getFinalResult() {
		return finalResult;
	}
	
	/**
	 * @return the {@link JumpStatementStorage}
	 */
	public JumpStatementStorage<S> getJumpStorage() {
		return jumpStorage;
	}
	
	/**
	 * @return the {@link CfgEdgeLabelProvider}
	 */
	public CfgEdgeLabelProvider getLabelResolver() {
		return labelResolver;
	}
	 
	/**
	 * Creates a new context with a new {@link OutOfScopeEvaluator}
	 *
	 * @param newScopeEvaluator the new {@link OutOfScopeEvaluator}
	 * @return the new context
	 */
	 public ControlFlowContext<S,C> createSubContext(final OutOfScopeEvaluator<S> newScopeEvaluator) {
		return new ControlFlowContext<>(newScopeEvaluator, jumpStorage, labelResolver, controlFlowEdges, finalResult, languageSpecificHandler);
	}
}
