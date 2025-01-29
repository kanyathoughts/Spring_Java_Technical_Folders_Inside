/*
 * Copyright (c) 2024 Deloitte innoWake GmbH. All rights reserved.
 */
package innowake.mining.data.migration.orient;

import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;

import innowake.mining.data.migration.base.OrientSchemaMigration;
import innowake.mining.data.migration.base.SchemaMigrationContext;
import innowake.mining.shared.FutureUtil;

/**
 * <b>Repeatable</b> migration script that removes all SourceAttachment related entities.
 */
public class SourceAttachmentRemoval extends OrientSchemaMigration {

	private static final int DELETE_BATCH_SIZE = 10_000;

	public SourceAttachmentRemoval(final SchemaMigrationContext context) {
		super(context);
	}

	@Override
	public void migrateOrient() throws SQLException {
		ifDeletionRequested(() -> {
				deleteEntities();
				dropClasses();
			}, () -> "Skipping deletion of orient classes and entities for: "
					+ "ReferencesSourceObject, SourceObject and SourceAttachment"
			);
	}

	private void deleteEntities() {
		final BlockingQueue<Future<?>> futures = new LinkedBlockingQueue<>();
		final var executorService = Executors.newFixedThreadPool(Math.max(1, Runtime.getRuntime().availableProcessors()));

		futures.add(executorService.submit(() -> deleteEntities("ReferencesSourceObject")));
		futures.add(executorService.submit(() -> deleteEntities("SourceObject")));

		try {
			FutureUtil.awaitAll(futures);
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException(e);
		} catch (final Exception exc) {
			throw new IllegalStateException(exc);
		}

		deleteEntities("SourceAttachment");
	}

	private void deleteEntities(final String name) {
		try (final var connection = context.getOrient().getConnection()) {
			LOG.info("Deleting {}", name);
			final String query = "DELETE FROM " + name + " LIMIT " + DELETE_BATCH_SIZE + " UNSAFE";

			try (final var delete = connection.prepareStatement(query)) {
				var deleted = 0;
				do {
					deleted = delete.executeUpdate();
					LOG.info("Deleted {} {} record", Integer.valueOf(deleted), name);
				} while (deleted >= DELETE_BATCH_SIZE);
			}
			LOG.info("Deletion of {} done", name);
		} catch (final SQLException exc) {
			LOG.warn("Error while deleting " + name + " entities", exc);
		}
	}

	private void dropClasses() throws SQLException {
		LOG.info("Dropping all source classes");

		executeStatements(dbOrient, List.of("DROP CLASS ReferencesSourceObject IF EXISTS UNSAFE",
											"DROP CLASS SourceObject IF EXISTS UNSAFE",
											"DROP CLASS SourceAttachment IF EXISTS UNSAFE"));
	}
}
