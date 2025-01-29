/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.config;

import javax.sql.DataSource;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import innowake.spring.data.orientdb.repository.config.EnableOrientRepositories;

/**
 * Configurations of OrientDB {@link DataSource DataSources}.
 */
@Configuration
@ConditionalOnProperty(name = "spring.datasource.enabled", matchIfMissing = true)
@EnableOrientRepositories(basePackages = "innowake.mining.data.repo")
public class OrientConfiguration {

	@Bean
	@ConfigurationProperties("spring.datasource")
	public DataSourceProperties orientDataSourceProperties() {
		return new DataSourceProperties();
	}

	@Bean
	@Primary
	@ConfigurationProperties("spring.datasource.hikari")
	public HikariConfig hikariConfig() {
		return new HikariConfig();
	}

	@Bean
	@Primary
	public DataSource orientDataSource() {
		final var config = hikariConfig();
		final var orientDataSourceProperties = orientDataSourceProperties();
		config.setJdbcUrl(orientDataSourceProperties.getUrl());
		config.setUsername(orientDataSourceProperties.getUsername());
		config.setPassword(orientDataSourceProperties.getPassword());
		config.setDriverClassName(orientDataSourceProperties.getDriverClassName());
		return new HikariDataSource(config);
	}

	@Bean
	@Primary
	/* "plain" because we also have the OrientJdbcTemplate class, defining a bean "orientJdbcTemplate" */
	public JdbcTemplate orientPlainJdbcTemplate(final DataSource dataSource) {
		return new JdbcTemplate(dataSource);
	}
}
