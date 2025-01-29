/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import innowake.mining.data.core.NaturalAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.LanguageSpecificHandler;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Natural specific logic for the control flow calculation.
 */
public class NaturalSpecificHandler implements LanguageSpecificHandler<NaturalStatement, NaturalControlFlowContext> {
	
	private static final Set<String> controlFlowIrrelevantNaturalStatements = new HashSet<>();
	private static final Set<String> wrappingStatements = new HashSet<>();
	static {
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.FORMAT);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.DEFINE_DATA);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.EVERY_RECORD);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.END_ALL);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.DEFINE_SUBROUTINE);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.BREAK);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.BEFORE_BREAK);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.START_OF_DATA);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.END_OF_DATA);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.IF_NO_RECORDS);
		controlFlowIrrelevantNaturalStatements.add(NaturalAstNodeType.LABELED);
		
		wrappingStatements.add(NaturalAstNodeType.LABELED);
		wrappingStatements.add(NaturalAstNodeType.EVERY_RECORD);
		wrappingStatements.add(NaturalAstNodeType.SELECT);
		wrappingStatements.add(NaturalAstNodeType.INCLUDE_STMT);
	}
	
	public static boolean isNaturalControlflowRelevantStatement(final AstNodePojo statementNode) {
		return ! controlFlowIrrelevantNaturalStatements.contains(statementNode.getType());
	}

	@Override
	public StatementSet<NaturalStatement> createEmptyStatementSet() {
		return new NaturalStatementSet();
	}

	@Override
	public boolean statementChildrenContainsNextStatement(final NaturalStatement currentStatement) {
		return wrappingStatements.contains(currentStatement.getAstNode().getType()) && ! currentStatement.getAstNode().getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT);
	}

	@Override
	public Optional<NaturalStatement> createNextStatement(final NaturalStatement currentStatement, final Optional<AstNodePojo> nextNode) {		
		if ( ! nextNode.isPresent()) {
			return Optional.empty();
		}
		final NaturalStatement nextStatement = new NaturalStatement(nextNode.get());
		nextStatement.mergeLastInputStatementNodes(currentStatement.getLastInputStatementNodes());
		return Optional.of(nextStatement);
	}

	@Override
	public boolean handle(final NaturalStatement currentStatement, final AbstractControlFlowResolver<NaturalStatement, NaturalControlFlowContext> currentControlFlowResolver) {
		final AstNodePojo astNode = currentStatement.getAstNode();
		final String type = astNode.getType();
		final Set<String> superTypes = astNode.getSuperTypes();
		if (superTypes.contains(AstNodeUtils.LOOP_STATEMENT)) {
			handleLoopEvents(currentStatement, currentControlFlowResolver);
		}
		switch (type) {
			case NaturalAstNodeType.DECIDE_FOR_STMT:
			case NaturalAstNodeType.DECIDE_ON_STMT:
				if(NaturalAstNodeType.EVERY.equals(astNode.getChildren().get(0).getType())) {
					currentControlFlowResolver.applyControlFlowResult(currentStatement,
							new DecideEveryStatementResolver(currentStatement, currentControlFlowResolver.getContext()).createControlFlow());
					return true;
				}
				return false;
			case NaturalAstNodeType.INPUT:
			case NaturalAstNodeType.INPUT_USING_MAP:
				/* new last input statement, reset the previous last input statements and continue with the default handling for the statement */
				currentStatement.resetAndAddNewLastInputStatementNode(astNode);
				return false;
			case NaturalAstNodeType.REINPUT:
				final NaturalControlFlowContext context = currentControlFlowResolver.getContext();
				currentStatement.getLastInputStatementNodes().forEach(inputNode -> {
					final ControlFlowPrototype edge = new ControlFlowPrototype();
					edge.setSrc(currentStatement.getAstNode());
					edge.setDst(inputNode);
					context.addControlFlowEdge(edge);	
				});
				return true;
			default:
		}
		return false;
	}

	@Override
	public boolean isControlflowRelevantStatement(final AstNodePojo statementNode) {
		return isNaturalControlflowRelevantStatement(statementNode);
	}

	private void handleLoopEvents (final NaturalStatement currentStatement, final AbstractControlFlowResolver<NaturalStatement, NaturalControlFlowContext> currentControlFlowResolver) {
		final Set<AstNodePojo> events = currentControlFlowResolver.getContext().getLoopEventsForLoop(currentStatement.getAstNode());
		for (final AstNodePojo event : events) {
			currentControlFlowResolver.applyLoopControlFlowResult(currentStatement, new NaturalEventResolver(currentStatement, event, currentControlFlowResolver.getContext()).createControlFlow());
		}
	}

	@Override
	public boolean shouldVisitFunctionCalls(final AstNodePojo statementNode) {
		return false;
	}

	@Override
	public boolean isNoFallthroughStatement(AstNodePojo statementNode) {
		return false;
	}
}
