/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.hashing.CityHash;

/**
 * Flyway migration script to compute "contentHash" for all the existing source objects.
 */
public class V1_2_70__SourceObjectContentHash extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);
	
	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(Assert.assertNotNull(context).getConnection(), true));

		jdbcTemplate.execute("CREATE PROPERTY SourceObject.contentHash IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);");
		jdbcTemplate.execute("CREATE PROPERTY Module.contentHash IF NOT EXISTS STRING;");

		final long sourceObjectCount = assertNotNull(jdbcTemplate.queryForObject("SELECT COUNT(*) FROM SourceObject;", Long.class)).longValue();
		final int batchSize = 1000;
		final String batchedQuery = "SELECT content, @rid FROM SourceObject SKIP ? LIMIT " + batchSize;
		final StopWatch stopWatch = new StopWatch();
		
		LOG.info(() -> String.format("Adding contentHash to %d SourceObjects", Long.valueOf(sourceObjectCount)));
		stopWatch.start();
		for (int batch = 0; batch < sourceObjectCount; batch += batchSize) {
			final Long batchStart = Long.valueOf(batch);
				
			final SourceObjectExtractor sourceObjectExtractor = new SourceObjectExtractor();
			jdbcTemplate.query(batchedQuery, sourceObjectExtractor, batchStart);
		
			sourceObjectExtractor.contentHash.forEach((id, contentHash) -> 
				jdbcTemplate.update("UPDATE SourceObject SET contentHash = ? WHERE id = ?;", contentHash, id)
			);
			LOG.info(() -> String.format("Added contentHash to %d SourceObjects", batchStart));
		}
		stopWatch.stop();
		LOG.info(() -> String.format("Finished adding contentHash to SourceObjects after %dms", Long.valueOf(stopWatch.getTime())));
	}
	
	private class SourceObjectExtractor implements ResultSetExtractor<Void> {
		
		final Map<Long, String> contentHash = new HashMap<>();

		@Nullable
		@Override
		public Void extractData(final ResultSet rs) throws SQLException {
			while (rs.next()) {
				final String content = rs.getString("content");
				if (content != null && ! content.isEmpty()) {
					contentHash.put(Long.valueOf(rs.getLong("id")), CityHash.cityHash128Hex(content));
				} else {
					contentHash.put(Long.valueOf(rs.getLong("id")), CityHash.EMPTY_CONTENT_HASH);
				}
			}
			return null;
		}
		
	}

}
