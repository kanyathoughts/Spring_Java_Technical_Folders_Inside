/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.WebApplicationType;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import innowake.lib.job.status.TimeoutJob;

/**
 * Spring application solely for test execution.
 */
@SpringBootApplication
@EnableTransactionManagement
@ComponentScan(basePackages = {
		"innowake.lib.job", "innowake.lib.job.api", "innowake.lib.job.api.log", "innowake.lib.job.api.management", "innowake.lib.job.internal",
		"innowake.lib.job.internal.executor", "innowake.lib.job.internal.executor.hazelcast", "innowake.lib.job.internal.filter",
		"innowake.lib.job.internal.hazelcast"
}, excludeFilters = {
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = TimeoutJob.Config.class)
})
@EnableAutoConfiguration
@ConfigurationPropertiesScan({
		"innowake.lib.job.internal.properties"
})
public class TestApplication {

	/**
	 * Runs the Spring Boot Application with Web app disabled.
	 *
	 * @param args The command line Arguments
	 */
	public static void main(final String[] args) {
		new SpringApplicationBuilder(TestApplication.class)
			.web(WebApplicationType.NONE)
			.run(args);
	}

	/**
	 * Provide default transaction manager with a different qualifier.
	 *
	 * @param dataSource the data source for which the transaction manager should be retrieved
	 * @return the transaction manager for the given data source
	 */
	@Qualifier("jdbc-transaction")
	@Bean
	public PlatformTransactionManager txManager(final DataSource dataSource) {
		return new DataSourceTransactionManager(dataSource);
	}
}
