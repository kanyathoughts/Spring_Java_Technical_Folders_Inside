/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.config;

import javax.sql.DataSource;

import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import innowake.spring.data.orientdb.integration.repository.EmployeeRepositoryTestService;
import innowake.spring.data.orientdb.integration.repository.TranscationTestMethod;
import innowake.spring.data.orientdb.repository.EmployeeRepository;
import innowake.spring.data.orientdb.repository.OrientRepository;
import innowake.spring.data.orientdb.repository.config.EnableOrientRepositories;

/**
 * Basic test configuration set up for {@link OrientRepository} integration tests.
 */
@Configuration
@EnableTransactionManagement
@EnableOrientRepositories(basePackages = "innowake.spring.data.orientdb.*")
@EnableConfigurationProperties(DataSourceProperties.class)
public class OrientDbTestConfiguration {

	/**
	 * Create a hikari data source.
	 *
	 * @param properties configured in application.yml
	 * @return dataSource 
	 */
	@Bean
	public DataSource customDataSource(final DataSourceProperties properties) {
		final HikariConfig config = new HikariConfig();
		config.setDriverClassName(properties.getDriverClassName());
		config.setJdbcUrl(properties.getUrl());
		config.setUsername(properties.getUsername());
		config.setPassword(properties.getPassword());
		config.addDataSourceProperty("cachePrepStmts", "true");
		config.addDataSourceProperty("prepStmtCacheSize", "250");
		config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
		config.addDataSourceProperty("allowMultiQueries", "true");
		config.setMaximumPoolSize(100);
		config.setMinimumIdle(10);
		
		final HikariDataSource hikariDataSource = new HikariDataSource(config);
		hikariDataSource.getHikariPoolMXBean().softEvictConnections();
		hikariDataSource.setConnectionTimeout(2000);
		return hikariDataSource;
	}
	
	/**
	 * Instance of {@link TranscationTestMethod}.
	 *
	 * @return instance of {@link TranscationTestMethod}
	 */
	@Bean
	public TranscationTestMethod getTranscationTestMethod() {
		return new TranscationTestMethod();
	}
	
	@Bean
	public EmployeeRepositoryTestService getEmployeeRepositoryTestService(final EmployeeRepository employeeRepository) {
		return new EmployeeRepositoryTestService(employeeRepository);
	}
}
