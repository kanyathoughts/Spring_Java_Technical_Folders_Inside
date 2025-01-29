/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */

package innowake.mining.server.config;

import java.sql.SQLException;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Configuration for the database connection pool for the DbCutter database.
 */
@Configuration
@ConditionalOnProperty(name = "dbcutter-db-postgres.enabled")
public class DbCutterDbPostgresConfiguration {

	private static final Logger LOG = LoggerFactory.getLogger(DbCutterDbPostgresConfiguration.class);

	@Bean
	@ConfigurationProperties("dbcutter-db-postgres.datasource")
	public HikariConfig postgresDbCutterHikariConfig() {
		return new HikariConfig();
	}

	@Bean
	@Qualifier("dbcutter-db-postgres")
	public HikariDataSource postgresDbCutterDataSource() throws SQLException {
		final HikariConfig config = postgresDbCutterHikariConfig();
		final String configHash = "HikariConfig@" + Integer.toHexString(System.identityHashCode(config));
		LOG.debug(
				"Created Hikari configuration. hash: {}, driver: {}, URL: {}, user: {}, pool: {}, max. pool size: {}, auto commit: {}, test query: {}, minimum idle: {}, Leak detection threshold: {}",
				configHash, config.getDriverClassName(), config.getJdbcUrl(), config.getUsername(), config.getPoolName(),
				Integer.valueOf(config.getMaximumPoolSize()), Boolean.valueOf(config.isAutoCommit()), config.getConnectionTestQuery(),
				Integer.valueOf(config.getMinimumIdle()), Long.valueOf(config.getLeakDetectionThreshold()));
		config.setConnectionInitSql("set application_name to 'DbCutter " + configHash + "'");

		final HikariDataSource dataSource = new PostgresConfiguration.HikariPostgresDataSource(config);
		LOG.trace("{} -> HikariDataSource@{}", configHash, Integer.toHexString(System.identityHashCode(dataSource)));

		return dataSource;
	}

	@Bean
	@Qualifier("dbcutter-db-postgres")
	public JdbcTemplate jdbcTemplateDbCutter(@Qualifier("dbcutter-db-postgres") final DataSource dataSource) {
		return new JdbcTemplate(dataSource);
	}
}
