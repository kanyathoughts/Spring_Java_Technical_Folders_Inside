/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package db.migration;

import innowake.mining.shared.model.StatementType;

/**
 * Flyway migration class for updating the StatementTypeEnum whenever there is a new {@link StatementType}.
 */
public class R__UpdateStatementTypeEnum extends AbstractUpdateEnumEntities<StatementType> {

	@Override
	protected StatementType[] getEnum() {
		return StatementType.values();
	}

	@Override
	protected String getTable() {
		return "StatementTypeEnum";
	}	

	@Override
	protected Integer arbitraryVersion() {
		return Integer.valueOf(1);
	}	

}
