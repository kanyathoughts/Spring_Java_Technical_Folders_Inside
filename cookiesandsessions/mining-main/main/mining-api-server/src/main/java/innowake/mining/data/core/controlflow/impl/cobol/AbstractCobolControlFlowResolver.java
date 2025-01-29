/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResultImpl;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Base class to follow the Cobol control flow.
 */
abstract class AbstractCobolControlFlowResolver {
	
	protected ControlFlowSubResult<DefaultStatement> result = new ControlFlowSubResultImpl<>();
	protected final CobolControlFlowContext context;
	protected Set<AstNodePojo> current = Collections.emptySet();
	protected Set<AstNodePojo> next;
	
	/**
	 * Constructor.
	 * 
	 * @param entryStatements the statements where the control flow starts
	 * @param context the context of the control flow resolver
	 */
	protected AbstractCobolControlFlowResolver(final Set<AstNodePojo> entryStatements, final CobolControlFlowContext context) {
		next = entryStatements;
		this.context = context;
	}
	
	protected void addControlFlowEdge(final AstNodePojo from, final AstNodePojo to, @Nullable final String label) {
		if (CobolAstNodeType.SIZE_GUARDED.getType().equals(to.getType())) {
			AstNodeUtils.getChildren(to, AstNodeUtils.STATEMENT).stream()
					.filter(statement -> CobolAstNodeType.ON_SIZE_ERROR != CobolAstNodeType.fromString(statement.getType()))
					.forEach(statement -> context.addControlFlowEdge(BuildingConsumer.of(new ControlFlowPrototype(),
							flow -> flow.setSrc(to).setDst(statement).setLabel(label))));
		}
		context.addControlFlowEdge(BuildingConsumer.of(new ControlFlowPrototype(),
				flow -> flow.setSrc(from).setDst(to).setLabel(label)));
	}
	
	/**
	 * Links the last statement to the next statement, or adds the last statement to the result.
	 *
	 * @param lastStatement the preceding statement
	 * @param embeddingStatement the statement that embeds the last statement (in case of trivial statements this is equal to lastStatement)
	 * @param label optional label for the link
	 */
	protected void linkToNextStatement(final AstNodePojo lastStatement, final AstNodePojo embeddingStatement, @Nullable final String label) {
		final Optional<AstNodePojo> nextStatement = nextStatement(embeddingStatement);
		if ( ! nextStatement.isPresent()) {
			/* if there is no next statement then lastStatement is the last statement of the current statement */
			result.addLastSimpleStatement(new DefaultStatement(lastStatement), label);
		} else {
			final Optional<AstNodePojo> outOfScope = context.isOutOfScope(embeddingStatement, nextStatement.get());
			if (outOfScope.isPresent()) {
				/* if the nextStatement is out of scope it means that a jump back condition has been triggered and the statement needs to be handled after the jump back */
				result.addLastUnhandledStatement(new DefaultStatement(lastStatement), outOfScope.get(), label);
			} else {
				/* if there is a next statement that is in scope of the current resolver, then lastStatement just needs to be connected with this next statement */
				addControlFlowEdge(lastStatement, nextStatement.get(), label);
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
	protected Optional<AstNodePojo> nextNode(final AstNodePojo currentNode) {
		return AstNodeUtils.getNextSiblingOrParentSiblingDeep(currentNode, AstNodeUtils.STATEMENT);
	}

	/**
	 * Determines the next statement for a given statement. If the given statement is of type {@link CobolAstNodeType#LABEL}
	 * or {@link CobolAstNodeType#SECTION}, the next statement will be the first child with superType {@link AstNodeUtils#STATEMENT}.
	 * If not, this method uses {@code AbstractControlFlowResolver.nextNode(AstNodePojo)} for obtaining the next nodes.
	 *
	 * @param currentStatement the current statement
	 * @return the next statement
	 */
	protected final Optional<AstNodePojo> nextStatement(final AstNodePojo currentStatement) {
		Optional<AstNodePojo> nextStatement = Optional.empty();
		final String type = currentStatement.getType();
		if (CobolAstNodeType.LABEL.getType().equals(type) || CobolAstNodeType.SECTION.getType().equals(type) || CobolAstNodeType.COPY.getType().equals(type)) {
			nextStatement = AstNodeUtils.getFirstChild(currentStatement, AstNodeUtils.STATEMENT);
		}
		return nextStatement.isPresent() ? nextStatement : nextNode(currentStatement);
	}

	/**
	 * Adds a new FlowsControl edge with a label.
	 *
	 * @param fromNode the from statement node
	 * @param toNode the to statement node
	 * @param labelNode the statement node used to resolve the label
	 */
	protected void addFlowsControlEdgeWithLabel(final AstNodePojo fromNode, final AstNodePojo toNode, final AstNodePojo labelNode) {
		addControlFlowEdge(fromNode, toNode, CobolLabelResolver.resolveLabelFor(labelNode));
	}
	
	/**
	 * Adds a new FlowsControl edge.
	 *
	 * @param fromNode the from statement node
	 * @param toNode the to statement node
	 */
	protected void addFlowsControlEdgeWithoutLabel(final AstNodePojo fromNode, final AstNodePojo toNode) {
		addControlFlowEdge(fromNode, toNode, null);
	}
	
	/**
	 * Evaluates the control flow for a given sub statement.
	 *
	 * @return the result of the control flow
	 */
	protected ControlFlowSubResult<DefaultStatement> createControlFlow() {
		while ( ! next.isEmpty()) {
			current = next;
			next = new HashSet<>();
			for (final AstNodePojo currentNode : current) {
				switch (CobolAstNodeType.fromString(currentNode.getType())) {
					case IF:
						applyControlFlowResult(currentNode, (new IfElseResolver(currentNode, context)).createControlFlow());
						break;
					case EVALUATE:
						applyControlFlowResult(currentNode, (new EvaluateResolver(currentNode, context)).createControlFlow());
						break;
					case GO_BACK:
						context.getFinalResult().addReturnStatement(currentNode);
						break;
					case PERFORM:
						handlePerform(currentNode);
						break;
					case GO_TO:
						handleGoTo(currentNode);
						break;
					case EXIT:
						handleExit(currentNode);
						break;
					case STOP:
						context.getFinalResult().addHaltStatement(currentNode);
						break;
					case SIZE_GUARDED:
						applyControlFlowResult(currentNode, (new OnSizeGuardedResolver(currentNode, context)).createControlFlow());
						break;
					case READ:
					case WRITE:
					case REWRITE:
					case START:
					case RETURN:
						applyControlFlowResult(currentNode, (new DatasetOperationResolver(currentNode, context)).createControlFlow());
						break;
					case SEARCH:
						applyControlFlowResult(currentNode, (new SearchResolver(currentNode, context)).createControlFlow());
						break;
					case CICS_XCTL:
						/* for now only the control flow inside the module is calculated and there are no special nodes for call statements 
						 * actually it is a call without return */
						context.getFinalResult().addHaltStatement(currentNode);
						break;
					case CICS_RETURN:
						handleCicsReturn(currentNode);
						break;
					case CALL:
					case CICS_LINK:
						/* for now only the control flow inside the module is calculated and there are no special nodes for call statements 
						 * actually these are a call with a return, for now they are just linked to the next statement */
					default:
						linkToNextStatement(currentNode, currentNode, null);
				}
			}
		}
		return result;
	}

	private void handleCicsReturn(final AstNodePojo currentNode) {
		/* for now only the control flow inside the module is calculated and there are no special nodes for call statements */
		if (currentNode.getProperties().get("transId") != null){
			/* a different transaction is called, only valid at the highest level so for now it is represented by a link to halt 
			 * actually it is a call without return */
			context.getFinalResult().addHaltStatement(currentNode);
		} else {
			context.getFinalResult().addReturnStatement(currentNode);
		}
	}

	private void handleExit(final AstNodePojo currentNode) {
		/* This special treatment is unfortunately needed since the CobolExitStmt AST node is used for more than just EXIT PROGRAM */
		if ("PROGRAM".equals(currentNode.getProperties().get("target"))) {
			context.getFinalResult().addReturnStatement(currentNode);
		} else {
			linkToNextStatement(currentNode, currentNode, null);

		}
	}

	private void handleGoTo(final AstNodePojo currentNode) {
		final Optional<List<AstNodePojo>> goToTargets = context.getGoToTargets(currentNode);
		if ( ! goToTargets.isPresent()) {
			context.getFinalResult().addErrorStatement(Tuple2.of(currentNode, "GO TO target is not present."));
		} else {
			final List<AstNodePojo> targets = goToTargets.get();
			if (targets.isEmpty()) {
				context.getFinalResult().addErrorStatement(Tuple2.of(currentNode, "Target could not be resolved."));
			} else if (AstNodeUtils.isTrue(currentNode, "isDependingOn")) {
				linkToNextStatement(currentNode, currentNode, CobolLabelResolver.resolveDefaultLabelFor(currentNode));
				for (int i = 0; i < targets.size(); i++) {
					mergeControlFlowResult(new GoToResolver(currentNode, context,
							targets.get(i), String.valueOf(i + 1)).createControlFlow()); /* DEPENDING ON is 1 based */
				}
			} else if (targets.size() == 1) {
				mergeControlFlowResult(new GoToResolver(currentNode, context, targets.get(0), null).createControlFlow());
			} else {
				context.getFinalResult().addErrorStatement(Tuple2.of(currentNode, "A non conditional GO TO needs to have exactly one target."));
			}
		}
	}

	private void handlePerform(final AstNodePojo currentNode) {
		final Map<String, Object> properties = currentNode.getProperties();
		final String target = (String) properties.get("target");
		final boolean isLoop = AstNodeUtils.isTrue(currentNode, "isLoop");
		String currentLabel = null;
		if (isLoop) {
			currentNode.addSuperType(AstNodeUtils.BRANCH_STATEMENT);
			currentLabel = CobolLabelResolver.resolveLabelFor(currentNode);
			linkToNextStatement(currentNode, currentNode, CobolLabelResolver.resolveDefaultLabelFor(currentNode));
		}
		if (target == null) {
			handleInlinePerform(currentNode, currentLabel, isLoop);
		} else {
			handlePerformLabel(currentNode, currentLabel, isLoop);
		}
	}

	private void handleInlinePerform(final AstNodePojo currentNode, @Nullable final String label, final boolean isLoop) {
		if (isLoop) {
			final ControlFlowSubResult<DefaultStatement> performResult = (new InlinePerformResolver(currentNode, label, context)).createControlFlow();
			performResult.getLastSimpleStatements().forEach(s -> addControlFlowEdge(s.getKey().getAstNode(), currentNode, s.getValue()));
			performResult.getLastUnhandledStatements().forEach(s -> result.addLastUnhandledStatement(s.getKey().getKey(), s.getValue(), s.getKey().getValue()));
		} else {
			applyControlFlowResult(currentNode, (new InlinePerformResolver(currentNode, label, context)).createControlFlow());
		}
	}
	
	private void handlePerformLabel(final AstNodePojo currentNode, @Nullable final String label, final boolean isLoop) {
		final Optional<Tuple2<Optional<AstNodePojo>, Optional<AstNodePojo>>> performTarget = context.getPerformTarget(currentNode);
		if ( ! performTarget.isPresent()) {
			context.getFinalResult().addErrorStatement(Tuple2.of(currentNode, "Target is not present."));
		} else {
			final Tuple2<Optional<AstNodePojo>, Optional<AstNodePojo>> target = performTarget.get();
			if (target.a.isEmpty()) {
				context.getFinalResult().addErrorStatement(Tuple2.of(currentNode, "Target could not be resolved."));
			} else if (target.b.isEmpty()) {
				context.getFinalResult().addErrorStatement(Tuple2.of(currentNode, "Thru could not be resolved."));
			} else {
				final ControlFlowSubResult<DefaultStatement> performResult
								= new PerformLabelResolver(currentNode, label, context, new Tuple2<>(target.a.get(), target.b)).createControlFlow();
				applyPerfomLabelControlFlowResult(currentNode, performResult, target.b.get(), isLoop);
			}
		}
	}
	
	private void applyControlFlowResult(final AstNodePojo currentNode, final ControlFlowSubResult<DefaultStatement> subResult) {
		subResult.getLastSimpleStatements().forEach(s -> linkToNextStatement(s.getKey().getAstNode(), currentNode, s.getValue()));
		subResult.getLastUnhandledStatements().forEach(s -> result.addLastUnhandledStatement(s.getKey().getKey(), s.getValue(), s.getKey().getValue()));
	}
	
	private void applyPerfomLabelControlFlowResult(final AstNodePojo currentNode, final ControlFlowSubResult<DefaultStatement> subResult,
			final AstNodePojo thru, final boolean isLoop) {
		subResult.getLastSimpleStatements().forEach(s -> linkPerformLabelToNextStatement(s.getKey().getAstNode(), currentNode, isLoop, s.getValue()));
		subResult.getLastUnhandledStatements().forEach(s -> {
			if (thru.equals(s.getValue())) {
				linkPerformLabelToNextStatement(s.getKey().getKey().getAstNode(), currentNode, isLoop, s.getKey().getValue());
			} else {
				result.addLastUnhandledStatement(s.getKey().getKey(), s.getValue(), s.getKey().getValue());
			}
		});
	}
	
	private void mergeControlFlowResult(final ControlFlowSubResult<DefaultStatement> subResult) {
		subResult.getLastSimpleStatements().forEach(s -> result.addLastSimpleStatement(s.getKey(), s.getValue()));
		subResult.getLastUnhandledStatements().forEach(s -> result.addLastUnhandledStatement(s.getKey().getKey(), s.getValue(), s.getKey().getValue()));
	}

	private void linkPerformLabelToNextStatement(final AstNodePojo lastStatement, final AstNodePojo embeddingStatement,
			final boolean isLoop, @Nullable String label) {
		if (isLoop) {
			addControlFlowEdge(lastStatement, embeddingStatement, label);
		} else {
			linkToNextStatement(lastStatement, embeddingStatement, label);
		}
	}
}
