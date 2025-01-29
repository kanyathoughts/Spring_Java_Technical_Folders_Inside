package innowake.mining.server.discovery.metrics.easytrieve;
/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */

import easytrievemacro.EasytrieveMacroParser.MacroProtoTypeStatementContext;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.RuleNode;

import easytrievemacro.EasytrieveMacroParserBaseVisitor;
import innowake.lib.core.api.lang.Nullable;

/**
 * A visitor which gathers all the macro calls in a easytrieve program only...
 */
public class EasytrieveDiscoveryMacroFileDetectionVisitor extends EasytrieveMacroParserBaseVisitor<Integer> {
	
	/**
	 * Visitor return dummy value to stop traversing deeper.
	 */
	private static final Integer STOP = Integer.valueOf(0);
	
	private int totalErrors = 0;
	private int errorLimit = -1;
	private int macroPrototypeStatemenst = 0;
	
	public EasytrieveDiscoveryMacroFileDetectionVisitor setErrorLimit(int errorLimit) {
		this.errorLimit = errorLimit;
		return this;
	}
	
	public boolean isMacroFile() { return macroPrototypeStatemenst > 0; }
	

	@Override
	public Integer visitMacroProtoTypeStatement(@Nullable MacroProtoTypeStatementContext ctx) {
		++macroPrototypeStatemenst;
		return STOP;
	}
	
	@Override
	public Integer visitErrorNode(@Nullable ErrorNode node) {
		++totalErrors;
		return super.visitErrorNode(node);
	}
	
	
	
	@Override
	protected boolean shouldVisitNextChild(final @Nullable RuleNode node, final @Nullable Integer currentResult) {
		if ( errorLimit > -1 && totalErrors > errorLimit ) {
			return false;
		}
		return super.shouldVisitNextChild(node, currentResult);
	}
	
}
