/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

/**
 * The relevant Modules columns in the Discovery Excel for importing.
 */
public enum DiscoveryExcelModulesColumn {

	UID("Uid"),
	NAME("Name"),
	PATH("Path"),
	LANGUAGE("Language"),
	TYPE("Type"),
	COMMENTLINES("Comment Line Count"),
	CODELINES("Code Line Count"),
	COMPLEXITY("Complexity");
	
	private String name;

	private DiscoveryExcelModulesColumn(final String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return name;
	}
}
