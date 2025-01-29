/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.hashing.CityHash.EMPTY_CONTENT_HASH;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;
import com.orientechnologies.orient.core.id.ORecordId;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.hashing.CityHash;

/**
 * Flyway migration script to re-compute "contentHash" for the existing source objects missing it.
 */
public class V1_2_95__Recompute_SourceObjectContentHash extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);
	private static final int BATCH_SIZE = 10_000;
	private static final String BATCHED_QUERY = "SELECT content, @rid FROM SourceObject WHERE contentHash IS NULL OR contentHash = '' LIMIT " + BATCH_SIZE;

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));

		final long sourceObjectCount = assertNotNull(jdbcTemplate.queryForObject(
				"SELECT COUNT(*) FROM SourceObject WHERE contentHash IS NULL OR contentHash = '';", Long.class)).longValue();

		if (sourceObjectCount == 0) {
			return;
		}
		
		final StopWatch stopWatch = new StopWatch();
		LOG.info(() -> String.format("Adding contentHash to %d SourceObjects", Long.valueOf(sourceObjectCount)));

		int updatedRows;
		stopWatch.start();
		do {
			final SourceObjectExtractor sourceExtractor = new SourceObjectExtractor();
			jdbcTemplate.query(BATCHED_QUERY, sourceExtractor);

			updatedRows = sourceExtractor.contentHashs.size();
			sourceExtractor.contentHashs.forEach((rid, contentHash) -> jdbcTemplate.update("UPDATE ? SET contentHash = ?;", new ORecordId(rid), contentHash));
			LOG.info(() -> String.format("Added contentHash to %d SourceObjects", sourceExtractor.contentHashs.size()));
		} while (updatedRows == BATCH_SIZE);
		stopWatch.stop();

		LOG.info(() -> String.format("Finished adding contentHash to SourceObjects after %dms", Long.valueOf(stopWatch.getTime())));
	}

	private class SourceObjectExtractor implements ResultSetExtractor<Void> {

		final Map<String, String> contentHashs = new HashMap<>();

		@Nullable
		@Override
		public Void extractData(final ResultSet rs) throws SQLException {
			while (rs.next()) {
				final String content = rs.getString("content");
				contentHashs.put(rs.getString("@rid"), StringUtils.isEmpty(content) ? EMPTY_CONTENT_HASH : CityHash.cityHash128Hex(content));
			}
			return null;
		}
	}
}
