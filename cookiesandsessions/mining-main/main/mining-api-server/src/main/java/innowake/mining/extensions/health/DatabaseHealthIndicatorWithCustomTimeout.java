/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.health;

import innowake.lib.core.api.lang.Nullable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

@Component("db-health with timeout")
public class DatabaseHealthIndicatorWithCustomTimeout implements HealthIndicator {

	private static final int QUERY_TIMEOUT = 5; /* seconds */

	private static final String MODULES_COUNT_QUERY = "SELECT count(*) FROM module";
	private static final String CLIENT_COUNT_QUERY = "SELECT count(*) FROM client";
	private static final String PROJECT_COUNT_QUERY = "SELECT count(*) FROM project";
	private static final String REFERENCE_COUNT_QUERY = "SELECT count(*) FROM module_relationship";

	private final JdbcTemplate jdbcTemplate;

	private final ScheduledExecutorService executor;

	@Nullable
	private ScheduledFuture<?> timeoutFuture;

	@Autowired
	public DatabaseHealthIndicatorWithCustomTimeout(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		this.jdbcTemplate = jdbcTemplate;
		executor = Executors.newSingleThreadScheduledExecutor();
	}

	@SuppressWarnings("boxing")
	@Override
	public Health health() {
		final List<Long> queryTimes = new ArrayList<>();
		long time = System.currentTimeMillis();
		try {
			final Long moduleCount = jdbcTemplate.query(con -> {
				final PreparedStatement stmt = con.prepareStatement(MODULES_COUNT_QUERY);
				timeoutFuture = executor.schedule(() -> {
					try {
						stmt.cancel();
					} catch (SQLException e) {
						/* ignore for now */
					}
				}, QUERY_TIMEOUT, TimeUnit.SECONDS);
				return stmt;
			}, rs -> {
				if (timeoutFuture != null) {
					timeoutFuture.cancel(false);
				}
				if (rs.next()) {
					return rs.getLong(1);
				} else {
					return null;
				}
			});
			queryTimes.add(System.currentTimeMillis() - time);
			if (moduleCount == null) {
				return Health.down().build();
			}

			time = System.currentTimeMillis();
			final Long clientCount = jdbcTemplate.query(con -> {
				final PreparedStatement stmt = con.prepareStatement(CLIENT_COUNT_QUERY);
				timeoutFuture = executor.schedule(() -> {
					try {
						stmt.cancel();
					} catch (SQLException e) {
						/* ignore for now */
					}
				}, QUERY_TIMEOUT, TimeUnit.SECONDS);
				return stmt;
			}, rs -> {
				if (timeoutFuture != null) {
					timeoutFuture.cancel(false);
				}
				if (rs.next()) {
					return rs.getLong(1);
				} else {
					return null;
				}
			});
			queryTimes.add(System.currentTimeMillis() - time);
			if (clientCount == null) {
				return Health.down().build();
			}

			time = System.currentTimeMillis();
			final Long projectCount = jdbcTemplate.query(con -> {
				final PreparedStatement stmt = con.prepareStatement(PROJECT_COUNT_QUERY);
				timeoutFuture = executor.schedule(() -> {
					try {
						stmt.cancel();
					} catch (SQLException e) {
						/* ignore for now */
					}
				}, QUERY_TIMEOUT, TimeUnit.SECONDS);
				return stmt;
			}, rs -> {
				if (timeoutFuture != null) {
					timeoutFuture.cancel(false);
				}
				if (rs.next()) {
					return rs.getLong(1);
				} else {
					return null;
				}
			});
			queryTimes.add(System.currentTimeMillis() - time);
			if (projectCount == null) {
				return Health.down().build();
			}

			time = System.currentTimeMillis();
			final Long referenceCount = jdbcTemplate.query(con -> {
				final PreparedStatement stmt = con.prepareStatement(REFERENCE_COUNT_QUERY);
				timeoutFuture = executor.schedule(() -> {
					try {
						stmt.cancel();
					} catch (SQLException e) {
						/* ignore for now */
					}
				}, QUERY_TIMEOUT, TimeUnit.SECONDS);
				return stmt;
			}, rs -> {
				if (timeoutFuture != null) {
					timeoutFuture.cancel(false);
				}
				if (rs.next()) {
					return rs.getLong(1);
				} else {
					return null;
				}
			});
			queryTimes.add(System.currentTimeMillis() - time);
			if (referenceCount == null) {
				return Health.down().build();
			}

			return Health.up().withDetail("module_count", moduleCount).withDetail("client_count", clientCount).withDetail("project_count", projectCount)
					.withDetail("reference_count", referenceCount).withDetail("average_query_time_ms", queryTimes.stream().mapToDouble(x -> x).average())
					.build();

		} catch (final Exception e) {
			return Health.down().build();
		}
	}
}
