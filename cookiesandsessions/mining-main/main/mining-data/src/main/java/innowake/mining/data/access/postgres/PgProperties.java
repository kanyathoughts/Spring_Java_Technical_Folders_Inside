/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

/**
 * Definitions of Mining database property names on Postgres.
 */
public class PgProperties {
	
	private PgProperties() { }
	
	/**
	 * UUID randomly assigned to the database upon schema creation.
	 */
	public static final String DATABASE_ID = "db.id";
	
	/**
	 * Current version number of the database schema.
	 */
	public static final String SCHEMA_VERSION = "schema.version";
	
}
