/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

/**
 * The relevant Dependencies columns in the Discovery Excel for importing.
 */
public enum DiscoveryExcelDependenciesColumn {

	UID("Uid"),
	TARGET_UID("Target Uid"),
	TARGET_LANGUAGE("Target Language"),
	TARGET_TYPE("Target Type"),
	TARGET_NAME("Target Name"),
	ATTRIBUTES("Attributes");
	
	private final String name;
	
	private DiscoveryExcelDependenciesColumn(final String name) {
		this.name = name;
	}
	
	@Override
	public String toString() {
		return name;
	}
}
