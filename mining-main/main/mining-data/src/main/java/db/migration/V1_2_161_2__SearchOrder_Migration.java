/* Copyright (c) 2022 Deloitte. All rights reserved. */
package db.migration;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.orientechnologies.orient.core.sql.executor.OResultInternal;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Flyway migration script that migrates SearchOrders to JSON.
 */
public class V1_2_161_2__SearchOrder_Migration extends BaseJavaMigration {

	private static final String PROJECT_SELECT_QUERY = "SELECT id, searchOrders FROM Project WHERE searchOrdersTemp IS NULL;";
	private static final String PROJECT_UPDATE_QUERY = "UPDATE Project SET searchOrdersTemp = ? WHERE id=?;";

	@Override
	public void migrate(final @Nullable Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));
		final ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.registerModule(new Jdk8Module()); /* required to serialize Optional properly */
		jdbcTemplate.query(PROJECT_SELECT_QUERY, new ProjectRowMapper()).forEach(projectIdAndSearchOrders -> {
			final Long projectId = projectIdAndSearchOrders.a;
			final List<SearchOrderOld> searchOrdersOld = projectIdAndSearchOrders.b;
			final List<SearchOrder> searchOrders = searchOrdersOld.stream()
					.map(old -> SearchOrder.fromPatterns(old.sourcePattern, old.targetPatterns))
					.collect(Collectors.toList());
			final List<String> searchOrdersJson = new ArrayList<>();
			for (SearchOrder searchOrder : searchOrders) {
				try {
					searchOrdersJson.add(objectMapper.writeValueAsString(searchOrder));
				} catch (JsonProcessingException e) {
					throw new IllegalStateException("Unable to serialize SearchOrders to JSON", e);
				}
			}
			jdbcTemplate.update(PROJECT_UPDATE_QUERY, searchOrdersJson, projectId);
		});
	}

	private static class SearchOrderOld {

		private final String sourcePattern;
		private final List<String> targetPatterns;

		private SearchOrderOld(final String sourcePattern, final List<String> targetPatterns) {
			this.sourcePattern = sourcePattern;
			this.targetPatterns = targetPatterns;
		}
	}

	private static class ProjectRowMapper implements RowMapper<Tuple2<Long, List<SearchOrderOld>>> {

		@Override
		@Nullable
		@SuppressWarnings("unchecked")
		public Tuple2<Long, List<SearchOrderOld>> mapRow(final ResultSet rs, final int rowNum) throws SQLException {
			final Long projectId = (Long) rs.getObject("id");
			final List<OResultInternal> searchOrderInternal = (List<OResultInternal>) rs.getObject("searchOrders");
			final List<SearchOrderOld> searchOrders = new ArrayList<>(searchOrderInternal.size());
			searchOrderInternal.forEach(searchOrder -> {
				final String sourcePattern = searchOrder.getProperty("sourcePattern");
				final List<String> targetPatterns = searchOrder.getProperty("targetPatterns");
				searchOrders.add(new SearchOrderOld(sourcePattern, targetPatterns));
			});
			return new Tuple2<>(projectId, searchOrders);
		}
	}
}
