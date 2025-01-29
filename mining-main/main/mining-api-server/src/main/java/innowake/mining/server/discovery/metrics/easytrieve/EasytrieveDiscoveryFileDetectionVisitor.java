/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.easytrieve;

import easytrieve.EasytrieveParser.ActivitySectionContext;
import easytrieve.EasytrieveParser.EnvironmentSectionContext;
import easytrieve.EasytrieveParser.JobSectionContext;
import easytrieve.EasytrieveParser.LibrarySectionContext;
import easytrieve.EasytrieveParser.ProgramSectionContext;
import easytrieve.EasytrieveParser.ScreenSectionContext;
import easytrieve.EasytrieveParser.SortSectionContext;
import easytrieve.EasytrieveParser.UnboundStatementContext;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.RuleNode;

import easytrieve.EasytrieveParserBaseVisitor;
import innowake.lib.core.api.lang.Nullable;


/**
 * Visitor class that extends default visitor class to visit only the nodes we care about to safely
 * identify this file as easytrieve and nothing more.
 */
public class EasytrieveDiscoveryFileDetectionVisitor extends EasytrieveParserBaseVisitor<Integer> {
	/**
	 * Visitor return dummy value to stop traversing deeper.
	 */
	private static final Integer STOP = Integer.valueOf(0);

	private int totalErrors = 0;
	private int errorLimit = -1;
	private int activitySections = 0;

	public EasytrieveDiscoveryFileDetectionVisitor setErrorLimit(int errorLimit) {
		this.errorLimit = errorLimit;
		return this;
	}
	
	public boolean isValidProgram() {
		return activitySections > 0;
	}

	@Override
	public Integer visitEnvironmentSection(@Nullable final EnvironmentSectionContext ctx) {
		return STOP;
	}
	
	@Override
	public Integer visitLibrarySection(@Nullable final LibrarySectionContext ctx) {
		return STOP;
	}
	
	@Override
	public Integer visitUnboundStatement(@Nullable final UnboundStatementContext ctx) {
		return STOP;
	}
	
	@Override
	public Integer visitActivitySection(@Nullable final ActivitySectionContext ctx) {
		/* while I could have possibly used this to mark if Easytrieve or not, I wanted to be a bit more specific.
		 * note also that if we do not visit the activity section, the code will never call the sub-sections like job, program, etc */
		return super.visitActivitySection(ctx);
	}
	
	@Override
	public Integer visitJobSection(@Nullable final JobSectionContext ctx) {
		++activitySections;
		return STOP;
	}
	
	@Override
	public Integer visitSortSection(@Nullable final SortSectionContext ctx) {
		++activitySections;
		return STOP;
	}
	
	@Override
	public Integer visitProgramSection(@Nullable final ProgramSectionContext ctx) {
		++activitySections;
		return STOP;
	}
	
	@Override
	public Integer visitScreenSection(@Nullable final ScreenSectionContext ctx) {
		++activitySections;
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
