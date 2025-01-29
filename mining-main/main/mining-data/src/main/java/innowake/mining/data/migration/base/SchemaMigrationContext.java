/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.sql.Connection;
import java.util.Set;
import java.util.UUID;

import javax.sql.DataSource;

import innowake.mining.shared.UUIDv5;

/**
 * Holds the database connections and global parameters for a database migration process.
 */
public class SchemaMigrationContext {
	
	public static final String EXTENSION_TRGM = "pg_trgm";
	
	private final Connection postgresConnection;
	private final Set<String> postgresExtensions;
	private final Connection orientConnection;
	private final DataSource orient;
	private UUID databaseId;
	private UUIDv5 uuid5;
	private int version;
	
	SchemaMigrationContext(final Connection postgresConnection, final Connection orientConnection, final DataSource orient, final Set<String> postgresExtensions) {
		this.postgresConnection = postgresConnection;
		this.postgresExtensions = postgresExtensions;
		this.orientConnection = orientConnection;
		this.orient = orient;
	}
	
	/**
	 * Connection to the Postgres Database that is the target for the migration.
	 * @return Open Postgres JDBC connection.
	 */
	public Connection getPostgresConnection() {
		return postgresConnection;
	}
	
	/**
	 * Connection to the OrientDB that data is to be migrated from.
	 * @return  Open OrientDB JDBC connection.
	 */
	public Connection getOrientConnection() {
		return orientConnection;
	}
	
	void setDatabaseId(final UUID databaseID) {
		this.databaseId = databaseID;
		this.uuid5 = new UUIDv5(databaseID);
	}
	
	/**
	 * Retrieves the unique identifier of the target database, used for deriving hashed identifiers for entities within that database.
	 * @return Random UUID specific to the database instance.
	 */
	public UUID getDatabaseId() {
		return databaseId;
	}
	
	/**
	 * Retrieves a generator for UUID Version 5 hashed identifiers.
	 * @return UUIDv5 generator instance.
	 */
	public UUIDv5 getUUIDv5() {
		return uuid5;
	}

	/**
	 * @return the OrientDB {@link DataSource}.
	 */
	public DataSource getOrient() {
		return orient;
	}
	
	/**
	 * Gets the version the database schema is currently being migrated to.
	 * @return Schema version.
	 */
	public int getCurrentVersion() {
		return version;
	}
	
	/**
	 * Sets the version the database schema is currently being migrated to.
	 * @param version Schema version.
	 */
	public void setVersion(final int version) {
		this.version = version;
	}
	
	/**
	 * Determines if a certain PostgreSQL extension is enabled.
	 * @param name Name of the extension.
	 * @return If the extension is available on the database.
	 */
	public boolean isPostgresExtenstionAvailable(final String name) {
		return postgresExtensions.contains(name);
	}
	
}
