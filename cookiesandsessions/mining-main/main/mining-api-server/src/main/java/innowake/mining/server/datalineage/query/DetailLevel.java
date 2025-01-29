/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.query;

/**
 * What level of detail the produced data flow graph will contain.
 */
public enum DetailLevel {
	/**
	 * only data flow between modules is shown (minimum detail)
	 */
	MODULE(0),
	/**
	 * data flow between modules and between fields inside a module is shown
	 */
	FIELD(1),
	/**
	 * data flow between modules, fields and statements is shown (full detail)
	 */
	STATEMENT(2);

	private final int detailLevel;

	DetailLevel(final int detailLevel) {
		this.detailLevel = detailLevel;
	}

	int getDetailLevel() {
		return detailLevel;
	}
}
