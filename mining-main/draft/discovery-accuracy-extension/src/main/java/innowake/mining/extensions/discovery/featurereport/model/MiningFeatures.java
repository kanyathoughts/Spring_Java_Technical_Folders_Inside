/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.extensions.discovery.featurereport.model;

/**
 * Supported mining features
 */
public enum MiningFeatures {

	PHY_LINES_COUNT("Physical Lines of Code"),
	CODE_LINES_COUNT("Source Lines of Code"), 
	COMMENT_LINES_COUNT("Comment Lines of Code"),
	DEAD_CODE_LINES_COUNT("Dead code lines count"),
	COMPLEXITY_MCCABE("Complexity McCabe"), 
	SQL_STATEMENTS("Sql Statements"),
	STATEMENTS("Statements");

	private final String supportedFeature;

	public String getSupportedFeature() {
		return supportedFeature;
	}

	private MiningFeatures(final String supportedFeature) {
		this.supportedFeature = supportedFeature;
	}
}
