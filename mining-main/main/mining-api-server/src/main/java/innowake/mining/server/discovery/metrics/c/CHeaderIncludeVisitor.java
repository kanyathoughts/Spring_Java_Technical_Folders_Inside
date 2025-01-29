package innowake.mining.server.discovery.metrics.c;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.RuleNode;

import CInlcudeOnly.CInlcudeOnlyBaseVisitor;
import CInlcudeOnly.CInlcudeOnlyParser.HashIncludeContext;
import innowake.lib.core.api.lang.Nullable;

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */



/**
 *
 * Visitor class that extends default visitor class to visit only the nodes we care about to safely
 * identify this file as C and nothing more.
 */
public class CHeaderIncludeVisitor extends CInlcudeOnlyBaseVisitor<Integer> {
	
	static final Pattern QUOTATION_PATTERN = Pattern.compile("(\"|')");
	static final Pattern INCLUDE_PATTERN = Pattern.compile("(?i)# *include");
	private static final Pattern GT_PATTERN = Pattern.compile(">");
	private static final Pattern LT_PATTERN = Pattern.compile("<");
	private int totalErrors = 0;
	
	private int errorLimit = -1;
	public CHeaderIncludeVisitor setErrorLimit(int errorLimit) {
		this.errorLimit = errorLimit;
		return this;
	}
	
	private final Set<String> includeSystemReferences = new HashSet<>();
	private final Set<String> includeLocalReferences = new HashSet<>();
	
	public Set<String> getIncludeSystemReferences() { return includeSystemReferences; }
	public Set<String> getIncludeLocalReferences() { return includeLocalReferences; }
	
	
	@Override
	public Integer visitHashInclude(@Nullable final HashIncludeContext ctx) {
		if( ctx == null ) return super.visitHashInclude(ctx); /* will never happen */
		
		String includeText = INCLUDE_PATTERN.matcher(ctx.getText()).replaceFirst("").trim();
		
		/* trim include text */
		if( includeText.startsWith("<") ) {
			/* system include */
			includeText = LT_PATTERN.matcher(includeText).replaceAll("");
			includeText = GT_PATTERN.matcher(includeText).replaceAll("").trim();
			includeSystemReferences.add(includeText);
		} else {
			/* system include */
			includeText = QUOTATION_PATTERN.matcher(includeText).replaceAll("").trim();
			includeLocalReferences.add(includeText);
		}
		
		return super.visitHashInclude(ctx);
	}
	
	@Override
	public Integer visitErrorNode(@Nullable ErrorNode node) {
		++totalErrors;
		return super.visitErrorNode(node);
	}
	
	@Override
	protected boolean shouldVisitNextChild(@Nullable RuleNode node, @Nullable Integer currentResult) {
		if( this.errorLimit > -1 && this.totalErrors > this.errorLimit ) {
			return false;
		}
		return super.shouldVisitNextChild(node, currentResult);
	}
	
}
