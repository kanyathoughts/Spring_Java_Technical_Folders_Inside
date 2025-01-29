/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.migration.base;

import java.sql.Connection;
import java.util.Arrays;
import java.util.UUID;

import innowake.mining.shared.UUIDv5;

public abstract class PostgresSchemaMigrationFromOrient extends PostgresSchemaMigration {
	
	private final Connection dbOrient;
	protected final UUIDv5 uuid5;
	
	/**
	 * Initializes the migration with connections to Postgres and OrientDB.
	 * @param context The specification of the environment in which the migration is to be run.
	 */
	protected PostgresSchemaMigrationFromOrient(final SchemaMigrationContext context) {
		super(context);
		this.dbOrient = context.getOrientConnection();
		this.uuid5 = context.getUUIDv5();
	}
	
	protected boolean isOrientAvailable() {
		return dbOrient != null;
	}
	
	protected Connection getOrientDB() {
		if (dbOrient == null) {
			throw new IllegalStateException("OrientDB not available");
		}
		return dbOrient;
	}
	
	/**
	 * Migrates data from OrientDB to Postgres
	 * @param title Identifies this migration in the log.
	 * @param readQuery Select data from OrientDB.
	 * @param writeQuery Insert data to Postgres.
	 * @param batchSize Maximum number of records to be transfered in a single request.
	 * @param mapper Procedure for converting data field between source and destination.
	 * @param ifNoOrient Operations to perform alternatively to the migration in case OrientDB is not available.
	 */
	protected void migrateData(final String title, final String readQuery, final String writeQuery,
			final int batchSize, final DataMigration.RecordMapper mapper, final SqlRunnable ifNoOrient) {
		if (dbOrient == null) {
			LOG.info(title + ": No OrientDB available, not migrating any data.");
			if (ifNoOrient != null) {
				ifNoOrient.run();
			}
			return;
		}
		DataMigration.setup("Orient->Postgres/" + title, dbOrient, readQuery, dbPostgres, writeQuery, batchSize, mapper).run();
	}
	
	protected void migrateData(final String title, final String readQuery, final String writeQuery,
			final int batchSize, final DataMigration.RecordMapper mapper) {
		migrateData(title, readQuery, writeQuery, batchSize, mapper, null);
	}
	
	/**
	 * Generate a UUIDv5 in the namespace of the destination database.
	 * @param entity Name of the entity to be identified.
	 * @param id All IDs originally needed to uniquely identify the entity.
	 * @return Generated UUID.
	 */
	protected UUID genUUIDv5(final String entity, final String... id) {
		final UUID uuid = uuid5.generate(entity, id);
		LOG.trace(() -> "UUIDv5: " + uuid + " " + entity + " " + Arrays.asList(id).toString());
		return uuid;
	}
	
}
