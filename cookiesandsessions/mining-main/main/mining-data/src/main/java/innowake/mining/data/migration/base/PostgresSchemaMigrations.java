/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.function.Supplier;

import javax.sql.DataSource;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.access.postgres.PgProperties;
import innowake.mining.data.access.postgres.PgPropertiesDao;

/**
 * Perform a sequence of migrations to update the version of a database schema.
 */
public abstract class PostgresSchemaMigrations {
	
	protected static final Logger LOG = LoggerFactory.getLogger(innowake.mining.data.Logging.MIGRATION);
	
	private static final String POSTGRES_BANNER = """
			Start of database schema validation
			  _____          _                  _____  ____  _      
			 |  __ \\        | |                / ____|/ __ \\| |     
			 | |__) |__  ___| |_ __ _ _ __ ___| (___ | |  | | |     
			 |  ___/ _ \\/ __| __/ _` | '__/ _ \\\\___ \\| |  | | |     
			 | |  | (_) \\__ \\ || (_| | | |  __/____) | |__| | |____ 
			 |_|   \\___/|___/\\__\\__, |_|  \\___|_____/ \\___\\_\\______|
			                     __/ |                              
			                    |___/                               
			""";
	private static final List<String> REQUIRED_EXTENSIONS = List.of( /* SchemaMigrationContext.EXTENSION_TRGM */ );

	/*    _____         _    _ _______ _____ ____  _   _   _ _ _ 
	 *   / ____|   /\  | |  | |__   __|_   _/ __ \| \ | | | | | |
	 *  | |       /  \ | |  | |  | |    | || |  | |  \| | | | | |
	 *  | |      / /\ \| |  | |  | |    | || |  | | . ` | | | | |
	 *  | |____ / ____ \ |__| |  | |   _| || |__| | |\  | |_|_|_|
	 *   \_____/_/    \_\____/   |_|  |_____\____/|_| \_| (_|_|_)
	 * 
	 * Try to avoid giving this property to projects! We should never ever have to skip the activation of an extension
	 * If an extension is disabled, data migrations will not work as intended. This can cause a lot of manual work later,
	 * which then has to be done by the developer (YOU!), who gave the projects the possibility to skip the activation.
	 */
	private static final String SKIP_EXTENSION_PROPERTY = "innowake.mining.data.migration.skip.ext.";
	private static final String MISSING_EXTENSION_BANNER = """
			
			
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!!! REQUIRED DATABASE EXTENSION NOT AVAILABLE !!!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			
			""";
	
	private final int versionOffset;
	
	private final List<Supplier<SchemaMigration>> migrations;
	
	/**
	 * Supplies the implementation for initializing an empty database.
	 * @param context Migration context to be passed to the implementation.
	 * @return Migration to be run on an empty database.
	 */
	protected abstract PostgresSchemaMigration supplyInitial(final SchemaMigrationContext context);
	
	/**
	 * Supplies the implementations for migrations from the minimum version onward, using {@link #migration(int, Supplier)}.
	 * @param context Migration context to be passed to each implementation.
	 */
	protected abstract void supplyMigrations(final SchemaMigrationContext context);
	
	/**
	 * Initializes a migration sequence that can migrate a schema from version 0 or higher.
	 */
	protected PostgresSchemaMigrations() {
		this(0);
	}
	
	/**
	 * Initializes a migration sequence.
	 * @param versionOffset Minimum version this implementation can migrate from.
	 */
	protected PostgresSchemaMigrations(final int versionOffset) {
		this.migrations = new ArrayList<>();
		this.versionOffset = versionOffset;
	}
	
	/**
	 * Adds a migration for a specific schema version to the sequence.
	 * This will fail if the migration is not in sequence with previously added migrations or the minimum version. See {@link #PostgresSchemaMigrations(int)}.
	 * @param targetVersion Version number the schema will have after this migration ran.
	 * @param migration Implementation of the migration.
	 */
	protected void migration(final int targetVersion, final Supplier<SchemaMigration> migration) {
		final int expectedVersion = migrations.size() + versionOffset + 1;
		if (targetVersion != expectedVersion) {
			throw new IllegalArgumentException("PostgreSQL schema migration conflict."
					+ " Version " + targetVersion + " was defined where " + expectedVersion + " was expected.");
		}
		this.migrations.add(migration);
	}
	
	/**
	 * Perform all migrations in this sequence, from the current version of the schema onward.
	 * If the target database is empty it will be initialized.
	 * @param postgres Connection to the target Postgres database.
	 * @param orient Connection to the OrientDB to migrate from.
	 * @param maxVersion Optionally restricts the schema migration to a certain version. A value <0 means no limit.
	 *                   This can be use to prevent automatic schema changes in certain deployments.
	 * @throws SQLException If a query fails.
	 */
	public void doMigrations(final DataSource postgres, final DataSource orient, final int maxVersion) throws SQLException {
		LOG.info(POSTGRES_BANNER);
		try (
			final Connection dbPostgres = postgres.getConnection();
			final Connection dbOrient = orient != null ? orient.getConnection() : null;
		) {
			final Set<String> extensions = getExistingExtensions(dbPostgres);
			for (final String requiredExtension : REQUIRED_EXTENSIONS) {
				if ( ! extensions.contains(requiredExtension) && tryCreateExtension(dbPostgres, requiredExtension)) {
					extensions.add(requiredExtension);
				}
			}
			dbPostgres.setAutoCommit(false);
			doMigrations(new SchemaMigrationContext(dbPostgres, dbOrient, orient, extensions), maxVersion);
		} catch (final Exception e) {
			throw new SQLException("Error validating/migrating Postgres/Orient database schema", e);
		}
	}
	
	private Set<String> getExistingExtensions(final Connection dbPostgres) throws SQLException {
		final Set<String> extensions = new HashSet<>();
		try (final var st = dbPostgres.createStatement()) {
			final var rs = st.executeQuery("SELECT extname FROM pg_extension");
			while (rs.next()) {
				extensions.add(rs.getString(1));
			}
		}
		return extensions;
	}
	
	private boolean tryCreateExtension(final Connection dbPostgres, final String name) throws SQLException {
		if (Boolean.getBoolean(SKIP_EXTENSION_PROPERTY + name)) {
			LOG.error("Missing PostgreSQL extension " + name
				+ MISSING_EXTENSION_BANNER
				+ "PostgreSQL extension " + name + " is not available and creation was suspended by property "
				+ SKIP_EXTENSION_PROPERTY + name + System.lineSeparator()
				+ "Certain features will not be available or their performance may be severely degraded!");
			return false;
		} else {
			try (final var st = dbPostgres.createStatement()) {
				st.execute("CREATE EXTENSION " + name);
				return true;
			} catch (final SQLException e) {
				LOG.error("Failed to create PostgreSQL extension " + name
						+ MISSING_EXTENSION_BANNER
						+ "Make sure the user '" + dbPostgres.getMetaData().getUserName()
						+ "' is able to call CREATE EXTENSION " + name + " on the database '" + dbPostgres.getCatalog() + "'.");
				throw e;
			}
		}
	}
	
	private void doMigrations(final SchemaMigrationContext context, final int maxVersion) throws SQLException {
		this.migrations.clear();
		
		final PgPropertiesDao propertiesDao = new PgPropertiesDao(context.getPostgresConnection());
		if ( ! propertiesDao.isSchemaPresent()) {
			LOG.info(() -> "PostgreSQL schema not present, initializing");
			supplyInitial(context).migrate();
			context.getPostgresConnection().commit();
		}
		context.setDatabaseId(UUID.fromString(propertiesDao.getOrThrow(PgProperties.DATABASE_ID)));
		final int currentVersion = Integer.parseInt(propertiesDao.getOrThrow(PgProperties.SCHEMA_VERSION));
		
		supplyMigrations(context);
		
		final int latestVersion = migrations.size() + versionOffset;
		LOG.info(() -> "PostgreSQL schema at version " + currentVersion + ", latest " + latestVersion);
		if (currentVersion > latestVersion) {
			throw new SQLException("PostgreSQL database schema is newer than this application release");
		}
		if (currentVersion < versionOffset) {
			throw new SQLException("PostgreSQL database schema is too old to be migrated");
		}
		
		for (int migrateVersion = currentVersion; migrateVersion < latestVersion; migrateVersion++) {
			final int nextVersion = migrateVersion + 1;
			if (maxVersion >= 0 && nextVersion > maxVersion) {
				throw new SQLException("PostgreSQL schema version restricted to " + maxVersion + " but " + latestVersion + " is required");
			}
			context.setVersion(nextVersion);
			LOG.info(() -> "PostgreSQL schema migration to version " + nextVersion + " started");
			try {
				migrations.get(migrateVersion - versionOffset).get().migrate();
			} catch (final Exception e) {
				throw new SQLException("Error migrating to PostgreSQL schema version " + nextVersion, e);
			}
			LOG.info(() -> "PostgreSQL schema now at version " + nextVersion + " of " + latestVersion);
			propertiesDao.set(PgProperties.SCHEMA_VERSION, String.valueOf(nextVersion));
			context.getPostgresConnection().commit();
		}
	}
}
