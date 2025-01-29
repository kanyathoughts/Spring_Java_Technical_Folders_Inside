/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package db.migration;

import innowake.mining.shared.model.Type;

/**
 * Flyway migration class for updating the TypeEnum whenever there is a new {@link Type}.
 */
public class R__UpdateTypeEnum extends AbstractUpdateEnumEntities<Type> {

	@Override
	protected Type[] getEnum() {
		return Type.values();
	}

	@Override
	protected String getTable() {
		return "TypeEnum";
	}	

	@Override
	protected Integer arbitraryVersion() {
		return Integer.valueOf(1);
	}	

}
