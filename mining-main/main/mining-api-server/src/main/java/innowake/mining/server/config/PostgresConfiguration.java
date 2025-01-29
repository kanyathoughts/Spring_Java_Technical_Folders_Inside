/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.config;

import java.sql.SQLException;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.JdbcTransactionManager;
import org.springframework.transaction.PlatformTransactionManager;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.migration.PostgresMiningSchemaMigrations;
import innowake.spring.data.orientdb.commons.core.SessionManager;

@Configuration
@ConditionalOnProperty(name = "postgres.enabled")
public class PostgresConfiguration {
	
	private static final Logger LOG = LoggerFactory.getLogger(PostgresConfiguration.class);
	
	@Value("${postgres.max-schema-version:-1}")
	private int maxVersion;
	
	public static class HikariPostgresDataSource extends HikariDataSource {
		public HikariPostgresDataSource(final HikariConfig config) {
			super(config);
		}
		
		@Override
		public void close() {
			super.close();
			LOG.debug(() -> "Closed HikariDataSource@" + Integer.toHexString(System.identityHashCode(this)));
		}
	}
	
	@Bean
	@ConfigurationProperties("postgres.datasource")
	public HikariConfig postgresHikariConfig() {
		return new HikariConfig();
	}
	
	@Bean
	@Qualifier("postgres")
	public HikariDataSource postgresDataSource(
			@Qualifier("orientPlainJdbcTemplate") @Autowired(required = false) @Nullable final JdbcTemplate orientJDBC,
			final ApplicationContext appContext) throws SQLException {
		final HikariConfig config = postgresHikariConfig();
		final String configHash =  "HikariConfig@" + Integer.toHexString(System.identityHashCode(config));
		LOG.debug("Created Hikari configuration. hash: {}, driver: {}, URL: {}, user: {}, pool: {}, max. pool size: {}, auto commit: {}, test query: {}, minimum idle: {}, Leak detection threshold: {}", configHash,
				config.getDriverClassName(), config.getJdbcUrl(), config.getUsername(), config.getPoolName(), Integer.valueOf(config.getMaximumPoolSize()),
				Boolean.valueOf(config.isAutoCommit()), config.getConnectionTestQuery(), Integer.valueOf(config.getMinimumIdle()), Long.valueOf(config.getLeakDetectionThreshold()));
		config.setConnectionInitSql("set application_name to 'Mining " + configHash + "'");
		
		final HikariDataSource dataSource = new HikariPostgresDataSource(config);
		LOG.trace("{} -> HikariDataSource@{}", configHash, Integer.toHexString(System.identityHashCode(dataSource)));
		
		new PostgresMiningSchemaMigrations().doMigrations(dataSource, orientJDBC != null ? orientJDBC.getDataSource() : null, maxVersion);
		
		if (orientJDBC != null) {
			final SessionManager sessionManager = appContext.getBean(SessionManager.class);
			sessionManager.markSchemaChanged();
		}
		
		return dataSource;
	}
	
	@Bean
	@Qualifier("postgres")
	public PlatformTransactionManager postgresTransactionManager(@Qualifier("postgres") final DataSource dataSource) {
		final JdbcTransactionManager transactionManager = new JdbcTransactionManager();
		transactionManager.setDataSource(dataSource);
		return transactionManager;
	}

	@Bean
	@Qualifier("postgres")
	public JdbcTemplate postgresJdbcTemplate(@Qualifier("postgres") final DataSource dataSource) {
		return new JdbcTemplate(dataSource);
	}
}
