/* Copyright (c) 2022 Deloitte. All rights reserved. */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.time.StopWatch;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.flywaydb.core.internal.jdbc.JdbcUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementCallback;
import org.springframework.jdbc.core.PreparedStatementCreator;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import com.google.common.collect.Lists;

import db.migration.model.legacy.dna.ClusterAlgorithmId;
import db.migration.model.legacy.dna.DnaCommunity;
import db.migration.model.legacy.dna.SequencerId;
import db.migration.model.legacy.dna.SimilarityId;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;

/**
 * Flyway migration script to create a DnaCommunity for unassigned modules.
 */
@SuppressWarnings("deprecation") /* This migration will be deleted together with all legacy model classes once migration from OrientDB to Postgres is finished */
public class V1_2_128__Create_Uassigned_Modules_Cluster extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);

	private static final String COUNT_OF_UNASSIGNED_MODULES = "SELECT count(moduleUnitLink) FROM DnaString WHERE "
			+ "moduleUnitLink NOT IN (SELECT in_BelongsToCluster.out as modules FROM DnaCommunity WHERE sequencerId=? AND snapshotLink=?"
			+ " UNWIND modules) AND sequencerId=? and projectLink.id=?";
	private static final String QUERY_TO_CREATE_UNASSIGNED_CLUSTER = "CREATE EDGE BelongsToCluster FROM (SELECT expand(moduleUnitLink) FROM DnaString WHERE "
			+ "moduleUnitLink NOT IN (SELECT in_BelongsToCluster.out as modules FROM DnaCommunity WHERE sequencerId='%s' AND snapshotLink=%s "
			+ "UNWIND modules) AND sequencerId='%s' and projectLink.id=%d)"
			+ " TO "
			+ "(INSERT INTO DnaCommunity SET projectLink=(SELECT FROM Project WHERE id=%d), snapshotLink=%s,"
			+ " sequencerId='%s', similarityId='%s', clusterAlgorithmId='%s', clusterId = '-1', title='Unassigned Modules')";

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));
		final List<DnaCommunity> communities = jdbcTemplate.query(
				" SELECT DISTINCT sequencerId, similarityId, projectLink.id, snapshotLink, clusterAlgorithmId FROM DnaCommunity", new DnaCommunityRowMapper());
		if ( ! communities.isEmpty()) {
			LOG.info(() -> String.format("Creating DnaCommunity for unassigned modules for %d communities", communities.size()));
			final StopWatch stopWatch = new StopWatch();
			stopWatch.start();
			final List<String> queriesList = communities.stream()
					.filter(community -> getCountOfUnassignedModules(community.getSequencerId().name(), community.getSnapshotRecordId(),
							community.getProjectId(), jdbcTemplate) > 0)
					.map(community -> String.format(QUERY_TO_CREATE_UNASSIGNED_CLUSTER, community.getSequencerId().name(), community.getSnapshotRecordId(),
							community.getSequencerId().name(), community.getProjectId(), community.getProjectId(), community.getSnapshotRecordId(),
							community.getSequencerId().name(), community.getSimilarityId().name(), community.getClusterAlgorithmId().name()))
					.collect(Collectors.toList());
			final List<List<String>> queriesSplitList = Lists.partition(queriesList, 1000);
			queriesSplitList.forEach(queries -> executeBatchInsert(queries, jdbcTemplate));
			stopWatch.stop();
			LOG.info(() -> String.format("Creation of DnaCommunity for unassigned modules took %s (H:mm:ss.SSS)", stopWatch.toString()));
		} else {
			LOG.info(() -> "There are no valid communities to create unassigned modules cluster.");
		}
	}

	private int getCountOfUnassignedModules(final String sequencerId, final String snapshotRid, final Long projectId, final JdbcTemplate jdbcTemplate) {
		return Optional.ofNullable(jdbcTemplate.queryForObject(COUNT_OF_UNASSIGNED_MODULES, Long.class, sequencerId, snapshotRid, sequencerId, projectId))
				.orElse(Long.valueOf(0)).intValue();
	}
	
	private void executeBatchInsert(final List<String> queriesList, final JdbcTemplate jdbcTemplate) {
		final PreparedStatementCreator creator = connection -> {
			final PreparedStatement prepareStatement = connection.prepareStatement(queriesList.get(0));
			queriesList.forEach(query -> {
				try {
					prepareStatement.addBatch(query);
				} catch (final SQLException e) {
					LOG.error("Error while creating cluster for unassigned modules.", e);
					throw new IllegalStateException(e);
				}
			});
			return prepareStatement;
		};

		final PreparedStatementCallback<int[]> callback = statement -> {
			try {
				return statement.executeBatch();
			} finally {
				JdbcUtils.closeResultSet(statement.getResultSet());
			}
		};
		jdbcTemplate.execute(creator, callback);
	}

	private static class DnaCommunityRowMapper implements RowMapper<DnaCommunity> {

		@Nullable
		@Override
		public DnaCommunity mapRow(final ResultSet rs, final int rowNum) throws SQLException {
			final DnaCommunity dnaCommunity = new DnaCommunity();
			dnaCommunity.setProjectId(rs.getLong("projectLink.id"));
			dnaCommunity.setSnapshotRecordId(rs.getString("snapshotLink"));
			dnaCommunity.setSequencerId(SequencerId.valueOf(rs.getString("sequencerId")));
			dnaCommunity.setSimilarityId(SimilarityId.valueOf(rs.getString("similarityId")));
			dnaCommunity.setClusterAlgorithmId(ClusterAlgorithmId.valueOf(rs.getString("clusterAlgorithmId")));

			return dnaCommunity;
		}
	}
}
