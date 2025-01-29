/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.oracle;

/**
 * A record definition within a CDO file.
 */
public class CDORecord {

	private final String name;
	
	/**
	 * Creates a new record with the given name.
	 * 
	 * @param name the name of the record
	 */
	public CDORecord(final String name) {
		this.name = name;
	}

	/**
	 * @return the name of the record
	 */
	public String getName() {
		return name;
	}

}
