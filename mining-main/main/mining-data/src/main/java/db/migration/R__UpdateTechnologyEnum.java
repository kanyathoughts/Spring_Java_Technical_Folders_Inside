/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package db.migration;

import innowake.mining.shared.model.Technology;

/**
 * Flyway migration class for updating the TechnologyEnum whenever there is a new {@link Technology}.
 */
public class R__UpdateTechnologyEnum extends AbstractUpdateEnumEntities<Technology> {

	@Override
	protected Technology[] getEnum() {
		return Technology.values();
	}

	@Override
	protected String getTable() {
		return "TechnologyEnum";
	}

	@Override
	protected Integer arbitraryVersion() {
		return Integer.valueOf(1);
	}	

}
