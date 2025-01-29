/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.c;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.RuleNode;

import C.CBaseVisitor;
import C.CParser.DeclarationContext;
import C.CParser.HashDefineIncludeContext;
import C.CParser.StorageClassSpecifierContext;
import C.CParser.StructOrUnionContext;
import innowake.lib.core.api.lang.Nullable;

/**
 *
 * Visitor class that extends default visitor class to visit only the nodes we care about to safely
 * identify this file as C and nothing more.
 */
public class CDiscoveryFileDetectionVisitor extends CBaseVisitor<Integer> {

	private int totalErrors = 0;
	private int errorLimit = -1;
	private final int confidenceThreshold;
	private Integer languageConfidencePercentage = Integer.valueOf(0); /* out of 100 */
	
	public CDiscoveryFileDetectionVisitor() {
		this(99);
	}

	public CDiscoveryFileDetectionVisitor(final int confidenceThreshold) {
		this.confidenceThreshold = confidenceThreshold;
	}

	public CDiscoveryFileDetectionVisitor setErrorLimit(final int errorLimit) {
		this.errorLimit = errorLimit;
		return this;
	}

	public Integer getLanguageConfidencePercentage() { return languageConfidencePercentage; }

	public boolean isValidProgram() {
		return languageConfidencePercentage.intValue() >= confidenceThreshold;
	}

	@Override
	public Integer visitDeclaration(final @Nullable DeclarationContext ctx) {
		setMaxLanguageConfidencePercentage(50);
		return super.visitDeclaration(ctx);
	}

	@Override
	public Integer visitHashDefineInclude(final @Nullable HashDefineIncludeContext ctx) {
		/* don't bother to continue */
		setMaxLanguageConfidencePercentage(99);
		return languageConfidencePercentage;
	}

	@Override
	public Integer visitStorageClassSpecifier(final @Nullable StorageClassSpecifierContext ctx) {
		setMaxLanguageConfidencePercentage(50);
		if (ctx != null ) {
			final String text = ctx.getText().toLowerCase();
			if ( text.contains("extern") || text.contains("typedef")) {
				setMaxLanguageConfidencePercentage(99);
				return languageConfidencePercentage;
			}
		}
		return super.visitStorageClassSpecifier(ctx);
	}

	@Override
	public Integer visitStructOrUnion(final @Nullable StructOrUnionContext ctx) {
		setMaxLanguageConfidencePercentage(75);
		return super.visitStructOrUnion(ctx);
	}

	@Override
	public Integer visitErrorNode(final @Nullable ErrorNode node) {
		++ totalErrors;
		return super.visitErrorNode(node);
	}

	@Override
	protected boolean shouldVisitNextChild(final @Nullable RuleNode node, final @Nullable Integer currentResult) {
		if ( languageConfidencePercentage.intValue() >= confidenceThreshold || ( errorLimit > -1 && totalErrors > errorLimit )) {
			return false;
		}
		return super.shouldVisitNextChild(node, currentResult);
	}

	private void setMaxLanguageConfidencePercentage(final int val) {
		languageConfidencePercentage = Integer.valueOf(Math.max(val, languageConfidencePercentage.intValue()));
	}
	
}
