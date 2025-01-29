/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.internal.StaleJobCleaner;
import innowake.mining.server.cache.MiningCacheConfig;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnContributorsConfiguration;
import innowake.mining.server.discovery.externalparsing.ExternalParsingConfiguration;
import innowake.mining.server.genai.knowledgeservice.KnowledgeServiceConfiguration;
import innowake.mining.server.opensearch.MiningOpenSearchConfiguration;
import innowake.mining.server.service.CookieIdVerifier;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigurationExcludeFilter;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.context.TypeExcludeFilter;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Import;
import org.springframework.context.event.EventListener;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;

/**
 * The Spring Boot Application entry class.
 */
@SpringBootApplication
@EnableTransactionManagement
@EnableAsync
@ComponentScan(basePackages = {
		"innowake.mining.server.aspect",
		"innowake.mining.server.controller",
		"innowake.mining.server.datalineage.service",
		"innowake.mining.server.datalineage",
		"innowake.mining.server.event",
		"innowake.mining.server.functionalblocks",
		"innowake.mining.server.graphql",
		"innowake.mining.server.service",
		"innowake.mining.server.config",
		"innowake.mining.server.error",
		"innowake.mining.server.export",
		"innowake.mining.server.filter",
		"innowake.mining.server.discovery.config",
		"innowake.mining.server.discovery.metrics",
		"innowake.mining.server.discovery.dna",
		"innowake.mining.server.discovery.dna.similarity",
		"innowake.mining.server.discovery.dawn.metrics.impl.cache",
		"innowake.mining.server.discovery.dawn.metrics.impl.core",
		"innowake.mining.server.discovery.dawn.metrics.impl.persistence",
		"innowake.mining.server.discovery.dawn.metrics.impl.temporarystorage",
		"innowake.mining.server.discovery.dawn.metrics.impl.service",
		"innowake.mining.server.discovery.parser",
		"innowake.mining.server.importer",
		"innowake.mining.server.locking",
		"innowake.mining.server.util",
		"innowake.mining.server.converter",
		"innowake.mining.server.stores",
		"innowake.mining.server.integration.discovery",
		"innowake.mining.server.universalsearch",
		"innowake.mining.data.error",
		"innowake.mining.data",
		"innowake.mining.extensions",
		"innowake.lib.job",
		"org.ff4j.web.api.resources",
		"innowake.mining.server.cache",
		"innowake.mining.server.scheduler",
		"innowake.mining.server.permission"
	},
	excludeFilters = { @Filter(type = FilterType.CUSTOM, classes = TypeExcludeFilter.class),
			           @Filter(type = FilterType.CUSTOM, classes = AutoConfigurationExcludeFilter.class) })
@EnableAutoConfiguration(exclude = {
		DataSourceAutoConfiguration.class
})
@ConfigurationPropertiesScan({ "innowake.mining.server.properties", "innowake.mining.data.properties",
		"innowake.lib.job.internal.properties" })
@EnableAspectJAutoProxy
@EnableScheduling
@Import({
		DawnContributorsConfiguration.class,
		ExternalParsingConfiguration.class,
		MiningOpenSearchConfiguration.class,
		MiningCacheConfig.class,
		KnowledgeServiceConfiguration.class
})
public class MiningApiApplication {

	static {
		/*
		 * WMIN-6295: sometimes OrientDB just closes connections "under the hood" this
		 * setting enforces re-validation of connections whenever they are returned from
		 * the pool
		 */
		System.setProperty("com.zaxxer.hikari.aliveBypassWindowMs", "-1");
	}

	private static final Logger LOG = LoggerFactory.getLogger(MiningApiApplication.class);

	@Autowired
	private CookieIdVerifier cookieIdVerifier;

	@Autowired
	private StaleJobCleaner staleJobCleaner;

	/**
	 * Wrap a JDBC {@link DataSource} to the Spring transaction manager by creating
	 * a new instance of {@link DataSourceTransactionManager}.
	 *
	 * @param dataSource The JDBC datasource provided thru the configuration.
	 * @return The Spring Data Source Transaction Manager wrapping the JDBC data
	 *         source.
	 */
	@Bean("jdbc-transaction")
	@ConditionalOnProperty(name = "spring.datasource.enabled", matchIfMissing = true)
	public PlatformTransactionManager txManager(final DataSource dataSource) {
		return new DataSourceTransactionManager(dataSource);
	}

	/**
	 * Run the Spring Boot Application.
	 *
	 * @param args The command line Arguments
	 */
	public static void main(final String[] args) {
		new SpringApplicationBuilder(MiningApiApplication.class)
				.listeners(new ApplicationConfigLoggerStartupListener())
				.run(args);
	}

	/**
	 * Logic run when the application is completely ready.
	 */
	@EventListener(ApplicationReadyEvent.class)
	public void startup() {
		verifyCookieIdConfiguration();
		cleanStaleRunningJobs();
	}

	private void verifyCookieIdConfiguration() {
		if (cookieIdVerifier.cookieIdVerificationIsEnabled()) {
			cookieIdVerifier.verifyCookieId();
		} else {
			LOG.info("Cookie ID verification is Disabled");
		}
	}

	private void cleanStaleRunningJobs() {
		staleJobCleaner.cleanStaleJobs();
	}

}
