/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package db.migration;

import innowake.mining.shared.model.StatementType;

/**
 * Flyway migration class for inserting the StatementTypeEnum values.
 */
public class V1_2_67__Insert_StatementTypeEnum_Values extends AbstractUpdateEnumEntities<StatementType> {

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
