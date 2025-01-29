/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.easytrieve;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

import easytrieve.EasytrieveLexer;
import easytrieve.EasytrieveParser.ActivityStatementContext;
import easytrieve.EasytrieveParser.ActivitySubStatementContext;
import easytrieve.EasytrieveParser.ElseConditionalContext;
import easytrieve.EasytrieveParser.ElseIfConditionalContext;
import easytrieve.EasytrieveParser.EndStatementContext;
import easytrieve.EasytrieveParser.EnvironmentStatementContext;
import easytrieve.EasytrieveParser.GotoStatementContext;
import easytrieve.EasytrieveParser.IfStatementContext;
import easytrieve.EasytrieveParser.LibraryStatementContext;
import easytrieve.EasytrieveParser.MacroInvocationStatementContext;
import easytrieve.EasytrieveParser.UnboundStatementContext;
import easytrieve.EasytrieveParserBaseListener;
import innowake.lib.core.api.lang.Nullable;

/**
 * 
 * A listener that gathers metrics about the Easytrieve program. Not sure if
 * this is something that should be included in the parser code itself.
 */
public class EasytrieveMetricsListener extends EasytrieveParserBaseListener {

	private final BufferedTokenStream tokens;
	
	private long newlineCount;
	private final Set<Token> commentTokens;
	private long complexity;
	private long numberOfStatements;
	
	/**
	 * 
	 * The metrics listener. Only difference between this and {@link EasytrieveParserBaseListener} is that
	 * this requires a {@link BufferedTokenStream}. This is so we can get hidden token data like comments.
	 * @param tokens the {@link BufferedTokenStream} we use to check hiddent tokens since these will be skipped by the parser.
	 */
	public EasytrieveMetricsListener(final BufferedTokenStream tokens) {
		this.tokens = tokens;
		commentTokens = new HashSet<>();
	}
	
	/* GET METHODS */
	public long getNewlineCount() { return newlineCount;}
	
	public long getNewlineCountWithComments() { return getNewlineCount() + getCommentLineCount(); }
	
	public long getCommentLineCount() { return commentTokens.size(); }
	
	public long getComplexity() { return complexity; }
	
	public long getNumberOfStatements() { return numberOfStatements; }
	
	/* HELPER METHODS */
	
	private long getAllNewlinesInRule( final ParserRuleContext ctx ) {
		return ctx.getText().codePoints()
				 			.filter(c -> c == '\n' || c == '\f')
				 			.count();
	}
	
	private Set<Token> getAllCommentLineTokensInRule( final ParserRuleContext ctx ) {
		final int tokenIndex = ctx.getStop().getTokenIndex();
		final List<Token> hiddenTokensToRight = tokens.getHiddenTokensToRight(tokenIndex, EasytrieveLexer.COMMENTS );
		if( hiddenTokensToRight != null ) {
			return hiddenTokensToRight.stream()
					 		   		  .filter(t -> t.getText().contains("\n") || t.getText().contains("\f"))
					 		   		  .collect(Collectors.toSet()); /* only count if the whole line is a comment */
		}
		/* else */
		return Collections.emptySet();
	}
	
	/* OVERRIDE PARSER RULES */
	
	@Override
	public void enterEveryRule(final @Nullable ParserRuleContext ctx) {
		super.enterEveryRule(ctx);
		
		if( ctx == null ) return;
		
		this.commentTokens.addAll( getAllCommentLineTokensInRule(ctx) );
	}
	
	@Override
	public void enterEndStatement(final @Nullable EndStatementContext ctx) {
		super.enterEndStatement(ctx);
		
		if( ctx == null ) return;
		
		newlineCount += getAllNewlinesInRule(ctx);
	}
	
	@Override
	public void exitEnvironmentStatement(final @Nullable EnvironmentStatementContext ctx) {
		super.enterEnvironmentStatement(ctx);
		
		if( ctx == null ) return;
		
		++numberOfStatements;
	}
	
	@Override
	public void exitLibraryStatement(final @Nullable LibraryStatementContext ctx) {
		super.exitLibraryStatement(ctx);
		
		if( ctx == null ) return;
		
		++numberOfStatements;
	}
	
	@Override
	public void exitActivityStatement(final @Nullable ActivityStatementContext ctx) {
		super.exitActivityStatement(ctx);
		
		if( ctx == null ) return;
		
		++numberOfStatements;
	}
	
	@Override
	public void exitActivitySubStatement(final @Nullable ActivitySubStatementContext ctx) {
		super.exitActivitySubStatement(ctx);
		
		if( ctx == null ) return;
		
		/* count sub statement as seperate statement */
		++numberOfStatements;
	}
	
	@Override
	public void exitUnboundStatement(final @Nullable UnboundStatementContext ctx) {
		super.exitUnboundStatement(ctx);
		
		if( ctx == null ) return;
		
		++numberOfStatements;
	}
	
	@Override
	public void exitIfStatement(final @Nullable IfStatementContext ctx) {
		super.exitIfStatement(ctx);
		
		if( ctx == null ) return;
		
		++complexity;
	}
	
	@Override
	public void exitElseIfConditional(final @Nullable ElseIfConditionalContext ctx) {
		super.exitElseIfConditional(ctx);
		
		if( ctx == null ) return;
		
		++complexity;
	}
	
	@Override
	public void exitElseConditional(final @Nullable ElseConditionalContext ctx) {
		super.exitElseConditional(ctx);
		
		if( ctx == null ) return;
		
		++complexity;
	}
	
	@Override
	public void exitGotoStatement(final @Nullable GotoStatementContext ctx) {
		super.exitGotoStatement(ctx);
		
		if( ctx == null ) return;
		
		complexity += 2;
	}
	
	@Override
	public void exitMacroInvocationStatement(final @Nullable MacroInvocationStatementContext ctx) {
		super.exitMacroInvocationStatement(ctx);
		
		if( ctx == null ) return;
		
		complexity += 7;
	}
	
}
