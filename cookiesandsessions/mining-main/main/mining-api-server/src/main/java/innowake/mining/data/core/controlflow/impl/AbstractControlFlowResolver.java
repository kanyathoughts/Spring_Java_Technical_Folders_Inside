/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.Logging;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;
import innowake.ndt.core.parsing.ast.model.LoopControlStatement;
import innowake.ndt.core.parsing.ast.model.LoopStatement;
import innowake.ndt.core.parsing.ast.model.statement.CallStatement;

/**
 * Base class to follow the control flow.
 * @param <S> the type of the statements
 * @param <C> the control flow context
 */
public abstract class AbstractControlFlowResolver<S extends Statement, C extends ControlFlowContext<S,C>> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CFG);
	protected ControlFlowSubResult<S> result = new ControlFlowSubResultImpl<>();
	protected final C context;
	
	public final C getContext() {
		return context;
	}

	protected StatementSet<S> current;
	protected StatementSet<S> next;
	
	/**
	 * the node that a next statement has to be a (deep) child of, or {@code null} if no parent check should be performed
	 */
	@Nullable
	protected AstNodePojo parentForNextNode = null;
	
	/**
	 * Constructor.
	 * 
	 * @param entryStatements the statements where the control flow starts
	 * @param context the context of the control flow resolver
	 */
	protected AbstractControlFlowResolver(final StatementSet<S> entryStatements, final C context) {
		this.context = context;
		next = entryStatements;
		current = createEmptyStatementSet();
	}
	
	/**
	 * Constructor.
	 * 
	 * @param context the context of the control flow resolver
	 */
	protected AbstractControlFlowResolver(final C context) {
		this.context = context;
		next = createEmptyStatementSet();
		current = createEmptyStatementSet();
	}
	
	public void addControlFlowEdge(final S statement, final AstNodePojo to) {
		addFlowsControlEdgeWithLabel(statement, to, (String) null);
	}
	
	public void addControlFlowEdge(final Map.Entry<S, String> statement, final AstNodePojo to) {
		addFlowsControlEdgeWithLabel(statement.getKey(), to, statement.getValue());
	}
	
	/**
	 * Adds a new FlowsControl edge.
	 *
	 * @param fromStatement the from statement node
	 * @param toNode the to statement node
	 * @param labelNode the statement node used to resolve the label
	 */
	protected void addFlowsControlEdgeWithLabel(final S fromStatement, final AstNodePojo toNode, final AstNodePojo labelNode) {
		addFlowsControlEdgeWithLabel(fromStatement, toNode, context.getLabelResolver().resolveLabelFor(labelNode));
	}
	
	protected void addFlowsControlEdgeWithLabel(final S fromStatement, final AstNodePojo toNode, @Nullable final String label) {
		final ControlFlowPrototype edge = new ControlFlowPrototype();
		edge.setSrc(fromStatement.getAstNode());
		edge.setDst(toNode);
		if (label != null) {
			edge.setLabel(label);
		}
		context.addControlFlowEdge(edge);
	}
	
	/**
	 * Links the last statement to the next statement, or adds the last statement to the result.
	 *
	 * @param precedingStatement the preceding statement
	 * @param embeddingStatement the statement that embeds the last statement (in case of trivial statements this is equal to lastStatement)
	 * @param label optional label
	 */
	public void linkToNextStatement(final S precedingStatement, final S embeddingStatement, @Nullable final String label) {
		final Optional<S> nextStatement = nextStatement(embeddingStatement, precedingStatement);
		if ( ! nextStatement.isPresent()) {
			/* if there is no next statement then lastStatement is the last statement of the current statement */
			if (label == null) {
				result.addLastSimpleStatement(precedingStatement);
			} else {
				result.addLastSimpleStatement(precedingStatement, label);
			}
		} else {
			final Optional<AstNodePojo> outOfScope = context.getScopeEvaluator().getLastBlockIfOutOfScope(embeddingStatement, nextStatement.get());
			if (outOfScope.isPresent()) {
				/* if the nextStatement is out of scope it means that a jump back condition has been triggered and the statement needs to be handled after the jump back */
				result.addLastUnhandledStatement(precedingStatement, outOfScope.get(), label);
			} else {
				/* if there is a next statement that is in scope of the current resolver, then lastStatement just needs to be connected with this next statement */
				handleLinkingAndNestedFunctionCalls(precedingStatement, label, nextStatement.get().getAstNode());
				next.add(nextStatement.get());
			}
		}
	}

	/**
	 * Determines the next node for a given node.
	 * The default implementation returns the next sibling or next parent sibling.
	 *
	 * @param currentNode the current node
	 * @return the next node
	 */
	public Optional<AstNodePojo> nextNode(final AstNodePojo currentNode) {
		final Optional<AstNodePojo> maybeNextNode = AstNodeUtils.getNextSiblingOrParentSiblingDeep(currentNode, AstNodeUtils.STATEMENT);
		final AstNodePojo parentForNextNodeNullSave = parentForNextNode;
		if (parentForNextNodeNullSave != null && maybeNextNode.isPresent() && ! isChildDeepOf(maybeNextNode.get(), parentForNextNodeNullSave)) {
			return Optional.empty();
		}
		if (maybeNextNode.isPresent() && context.getLanguageSpecificHandler().isNoFallthroughStatement(maybeNextNode.get())) {
			return nextNode(maybeNextNode.get());
		}
		return maybeNextNode;
	}

	/**
	 * Determines the next statement for a given statement.
	 * This method uses {@code AbstractControlFlowResolver.nextNode(AstNodePojo)} for obtaining the next nodes.
	 *
	 * @param currentEmbeddingStatement the current embedding statement
	 * @param currentStatement the current statement
	 * @return the next statement
	 */
	public final Optional<S> nextStatement(final S currentEmbeddingStatement, final S currentStatement) {
		Optional<AstNodePojo> nextNode = Optional.empty();
		if (context.getLanguageSpecificHandler().statementChildrenContainsNextStatement(currentEmbeddingStatement)) {
			nextNode = AstNodeUtils.getFirstChild(currentEmbeddingStatement.getAstNode(), AstNodeUtils.STATEMENT);
		}
		if ( ! nextNode.isPresent()) {
			nextNode = nextNode(currentEmbeddingStatement.getAstNode());
		}
		final Optional<S> nextStatement = context.getLanguageSpecificHandler().createNextStatement(currentStatement, nextNode);
		final boolean isNextStatement = ! nextStatement.isPresent() || context.getLanguageSpecificHandler().isControlflowRelevantStatement(nextStatement.get().getAstNode());
		return isNextStatement ? nextStatement : nextStatement(nextStatement.get(), currentStatement);
	}
	
	/**
	 * Convenience method to access the method from the language specific handler
	 * 
	 * @return An empty {@link StatementSet}
	 */
	protected final StatementSet<S> createEmptyStatementSet() {
		return context.getLanguageSpecificHandler().createEmptyStatementSet();
	}
	
	/**
	 * Evaluates the control flow for a given sub statement.
	 *
	 * @return the result of the control flow
	 */
	protected ControlFlowSubResult<S> createControlFlow() {
		while ( ! next.isEmpty()) {
			current = next;
			next = createEmptyStatementSet();
			for (final S currentStatement : current) {
				LOG.trace(() -> String.format(
						"Calculating cfg at location %s of type %s for all members of the list of %s",
						currentStatement.getAstNode().getLocation(),
						currentStatement.getAstNode().getType(),
						currentStatement.getAstNode().getSuperTypes()));
				if (context.getLanguageSpecificHandler().handle(currentStatement, this)) {
					continue;
				}
				final Set<String> superTypes = currentStatement.getAstNode().getSuperTypes();
				if (superTypes.contains(AstNodeUtils.LOOP_STATEMENT)) {
					applyLoopControlFlowResult(currentStatement, new LoopStatementResolver<S, C>(currentStatement, context).createControlFlow());
					linkToNextStatement(currentStatement, currentStatement, context.getLabelResolver().resolveDefaultLabelFor(currentStatement.getAstNode()));
				} else if (superTypes.contains(AstNodeUtils.BRANCH_STATEMENT)) {
					applyControlFlowResult(currentStatement, new BranchStatementResolver<S, C>(currentStatement, context).createControlFlow());
				} else if (superTypes.contains(AstNodeUtils.MODULE_RETURN_STATEMENT)) {
					context.getFinalResult().addReturnStatement(currentStatement.getAstNode());
				} else if (superTypes.contains(AstNodeUtils.LOOP_CONTROL_STATEMENT) || superTypes.contains(AstNodeUtils.RETURN_STATEMENT)) {
					result.addLastUnhandledStatement(currentStatement, currentStatement.getAstNode());
				} else if (superTypes.contains(AstNodeUtils.CALL_STATEMENT)) {
					handleCallStatement(currentStatement);
				} else if (superTypes.contains(AstNodeUtils.HALT_STATEMENT)) {
					context.getFinalResult().addHaltStatement(currentStatement.getAstNode());
				} else if (superTypes.contains(AstNodeUtils.JUMP_STATEMENT)) {
					handleGoToStatementSuperTypes(currentStatement);
				} else {
					linkToNextStatement(currentStatement, currentStatement, null);
				}
			}
		}
		return result;
	}
	
	/**
	 * Applies a {@link ControlFlowSubResult}, linking the statements.
	 * 
	 * @param currentStatement the current statement of this resolver
	 * @param subResult to apply to the current resolver
	 */
	public void applyControlFlowResult(final S currentStatement, final ControlFlowSubResult<S> subResult) {
		subResult.getLastSimpleStatements().forEach(s -> linkToNextStatement(s.getKey(), currentStatement, s.getValue()));
		subResult.getLastUnhandledStatements().forEach(s -> result.addLastUnhandledStatement(s.getKey().getKey() , s.getValue(), s.getKey().getValue()));
	}
	
	/**
	 * Applies a {@link ControlFlowSubResult} of a loop statement, linking the statements.
	 * 
	 * @param currentLoopStatement the current statement of this resolver
	 * @param loopResult to apply to the current resolver
	 */
	public void applyLoopControlFlowResult(final S currentLoopStatement, final ControlFlowSubResult<S> loopResult) {
		final AstNodePojo currentLoopAstNode = currentLoopStatement.getAstNode();
		loopResult.getLastSimpleStatements().forEach(s -> addControlFlowEdge(s, currentLoopAstNode));
		for (final var unhandledStatement : loopResult.getLastUnhandledStatements()) {
			final Object label = unhandledStatement.getValue().getProperties().get(LoopControlStatement.LABEL);
			if (label == null || label.equals(currentLoopStatement.getAstNode().getProperties().get(LoopStatement.LABEL))) {
				if (unhandledStatement.getValue().getSuperTypes().contains(AstNodeUtils.BREAK_STATEMENT)) {
					linkToNextStatement(unhandledStatement.getKey().getKey(), currentLoopStatement, unhandledStatement.getKey().getValue());
				} else if (unhandledStatement.getValue().getSuperTypes().contains(AstNodeUtils.CONTINUE_STATEMENT)) {
					addControlFlowEdge(unhandledStatement.getKey().getKey(), currentLoopStatement.getAstNode());
				}
			} else {
				result.addLastUnhandledStatement(unhandledStatement.getKey().getKey(), unhandledStatement.getValue(), unhandledStatement.getKey().getValue());
			}
		}
	}
	
	/**
	 * Merges a {@link ControlFlowSubResult} with the current one.
	 *
	 * @param subResult the sub result that gets merged into the current one.
	 */
	public void mergeControlFlowResult(final ControlFlowSubResult<S> subResult) {
		subResult.getLastSimpleStatements().forEach(s -> result.addLastSimpleStatement(s.getKey(), s.getValue()));
		subResult.getLastUnhandledStatements().forEach(s -> result.addLastUnhandledStatement(s.getKey().getKey() , s.getValue(), s.getKey().getValue()));
	}
	
	/**
	 * Checks if a node is a child or child of a child of another node
	 * 
	 * @param child the node that is checked if it is a child or child of a child
	 * @param potentialParent the node that is checked if it is a parent
	 * @return {@code true} if the node is a child or child of a child of the potential parent node
	 */
	protected boolean isChildDeepOf(final AstNodePojo child, final AstNodePojo potentialParent) {
		for (Optional<AstNodePojo> parent = child.getParent(); parent.isPresent(); parent = parent.get().getParent()) {
			if (potentialParent.equals(parent.get())) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns the first relevant statement from the children of an node
	 *
	 * @param node the node
	 * @param lastStatement the last statement, needed to resolve the state of the next statement
	 * @return the first child statement node
	 */
	protected Optional<S> getFirstChildStatement(final AstNodePojo node, final S lastStatement) {
		final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
		Optional<S> statement = context.getLanguageSpecificHandler().createNextStatement(lastStatement, firstStatement);
		if (statement.isPresent() && ! context.getLanguageSpecificHandler().isControlflowRelevantStatement(statement.get().getAstNode())) {
			statement = nextStatement(statement.get(), statement.get());
		}
		return statement;
	}
	
	protected void handleCallStatement(final S currentStatement) {
		final AstNodePojo astNode = currentStatement.getAstNode();
		final boolean isReturning = Boolean.parseBoolean(astNode.getProperties().get(CallStatement.IS_RETURNING).toString());
		final Set<String> superTypes = astNode.getSuperTypes();
		if (superTypes.contains(AstNodeUtils.CALL_EXTERNAL_STATEMENT)) {
			/* currently no external handling is done, return call statements are just linked to the next statement, while non returning call statements are linked to the halt point */
			if (isReturning) {
				linkToNextStatement(currentStatement, currentStatement, null);
			} else {
				context.getFinalResult().addHaltStatement(astNode);
			}
		} else if (superTypes.contains(AstNodeUtils.CALL_INTERNAL_STATEMENT)) {
			final ControlFlowSubResult<S> subResult = new JumpWithReturnResolver<S, C>(currentStatement, context).createControlFlow();
			if (isReturning) {
				subResult.getLastSimpleStatements().forEach(lastSimpleStatement -> linkToNextStatement(lastSimpleStatement.getKey(), currentStatement, lastSimpleStatement.getValue()));
				for (final var unhandledStatement : subResult.getLastUnhandledStatements()) {
					if (unhandledStatement.getValue().getSuperTypes().contains(AstNodeUtils.RETURN_STATEMENT)) {
						linkToNextStatement(unhandledStatement.getKey().getKey(), currentStatement, unhandledStatement.getKey().getValue());
					} else {
						result.addLastUnhandledStatement(unhandledStatement.getKey().getKey(), unhandledStatement.getValue(), unhandledStatement.getKey().getValue());
					}
				}
			} else {
				subResult.getLastSimpleStatements().forEach(s -> context.getFinalResult().addHaltStatement(s.getKey().getAstNode()));
			}
		}
	}
	
	protected void handleGoToStatementSuperTypes(final S currentStatement) {
		final ControlFlowSubResult<S> subResult = new GoToStatementResolver<S, C>(currentStatement, context).createControlFlow();
		subResult.getLastSimpleStatements().forEach(lastSimpleStatement -> context.getFinalResult().addReturnStatement(lastSimpleStatement.getKey().getAstNode()));
		subResult.getLastUnhandledStatements().forEach(s -> result.addLastUnhandledStatement(s.getKey().getKey(), s.getValue(), s.getKey().getValue()));
	}
	
	protected final List<AstNodePojo> extractNestedFunctioncalls(final AstNodePojo current) {
		final List<AstNodePojo> nestedFunctionCalls = new ArrayList<>();
		final List<AstNodePojo> children = current.getChildren();
		if (context.getLanguageSpecificHandler().shouldVisitFunctionCalls(current)) {
			children.forEach(child -> extractNestedFunctioncalls(child, nestedFunctionCalls));
		}
		return nestedFunctionCalls;
	}
	
	protected void extractNestedFunctioncalls(final AstNodePojo current, final List<AstNodePojo> functionCalls) {
		final List<AstNodePojo> children = current.getChildren();
		final boolean isFunctionCall = current.getSuperTypes().contains(AstNodeUtils.CALL_INTERNAL_STATEMENT);
		if (isFunctionCall || !current.getSuperTypes().contains(AstNodeUtils.STATEMENT)
				|| !context.getLanguageSpecificHandler().isControlflowRelevantStatement(current)) {
			children.forEach(child -> extractNestedFunctioncalls(child, functionCalls));
		}
		if (isFunctionCall) {
			functionCalls.add(current);
		}
	}
	
	protected void handleLinkingAndNestedFunctionCalls(final S lastStatement, @Nullable String label, final AstNodePojo nextStatementNode) {
		final List<AstNodePojo> nestedFunctionCalls = extractNestedFunctioncalls(nextStatementNode);
		final Set<Map.Entry<S, String>> edgesToConnect = new HashSet<>();
		edgesToConnect.add(new AbstractMap.SimpleEntry<>(lastStatement, label));
		for (final AstNodePojo functionCall : nestedFunctionCalls) {
			final StatementSet<S> functionCallStatements = createEmptyStatementSet();
			edgesToConnect.forEach(statement -> {
				addControlFlowEdge(statement, functionCall);
				context.getLanguageSpecificHandler().createNextStatement(statement.getKey(), Optional.of(functionCall))
						.ifPresent(functionCallStatements::add);
			});
			edgesToConnect.clear();
			if (functionCall.getSuperTypes().contains(AstNodeUtils.CALL_INTERNAL_STATEMENT)) {
				for (final S functionCallStatement : functionCallStatements) {
					final ControlFlowSubResult<S> functionResult =
							new JumpWithReturnResolver<S, C>(functionCallStatement, context).createControlFlow();
					functionResult.getLastSimpleStatements().forEach(edgesToConnect::add);
					for (var unhandledStatement : functionResult.getLastUnhandledStatements()) {
						if (unhandledStatement.getValue().getSuperTypes().contains(AstNodeUtils.RETURN_STATEMENT)) {
							edgesToConnect.add(new AbstractMap.SimpleEntry<>(unhandledStatement.getKey().getKey(), unhandledStatement.getKey().getValue()));
						} else {
							result.addLastUnhandledStatement(unhandledStatement.getKey().getKey(), unhandledStatement.getValue(), unhandledStatement.getKey().getValue());
						}
					}
				}
			} else {
				functionCallStatements.forEach(functionCallStatement -> edgesToConnect.add(new AbstractMap.SimpleEntry<>(functionCallStatement, null)));
			}
		}
		if (edgesToConnect.isEmpty()) {
			LOG.error(() -> "Error resolving the nested function calls of " + getAstNodeLogString(nextStatementNode) + " Controlflow graph is missing edges.");
		}
		edgesToConnect.forEach(edge -> addControlFlowEdge(edge, nextStatementNode));
	}
	
	protected String getAstNodeLogString(final AstNodePojo node) {
		return node.toString();
	}

	public void addNext(final S defaultStatement) {
		next.add(defaultStatement);
	}
}
