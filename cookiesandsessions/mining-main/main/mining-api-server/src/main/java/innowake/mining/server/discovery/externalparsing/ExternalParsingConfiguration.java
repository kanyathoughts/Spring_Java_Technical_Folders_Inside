/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.externalparsing;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.hazelcast.config.Config;
import com.hazelcast.config.MapConfig;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.spring.context.SpringManagedContext;
import innowake.lib.job.api.config.HazelcastConfigurationSupport;
import innowake.lib.job.api.config.properties.ClusterProperties;
import innowake.mining.server.discovery.externalparsing.rpc.HazelcastRpcTransport;
import innowake.mining.server.discovery.externalparsing.rpc.JsonRpcHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import static innowake.mining.server.discovery.externalparsing.rpc.HazelcastRpcTransport.RESPONSES_MAP_NAME;
import static innowake.mining.server.discovery.externalparsing.rpc.HazelcastRpcTransport.RESPONSE_TTL;

/**
 * Configuration that enables calling of external parsing providers.
 * <p>
 * This configuration is disabled unless the configuration property {@code external-parsing.enabled}
 * is set to {@code true}. It is disabled by default because this configuration doesn't add any functionality
 * unless some external parsing providers are configured and deployed.
 * </p>
 */
@Configuration
@ConditionalOnProperty("external-parsing.enabled")
@EnableConfigurationProperties
public class ExternalParsingConfiguration extends HazelcastConfigurationSupport {

	private static final String CLUSTER_NAME = "external-parsing";

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private SpringManagedContext springManagedContext;

	@Bean
	@ConfigurationProperties(prefix = "external-parsing")
	public ExternalParsingProperties externalParsingProperties() {
		return new ExternalParsingProperties();
	}

	@Bean
	@Qualifier("external-parsing")
	public HazelcastInstance externalParsingHz() {
		return Hazelcast.newHazelcastInstance(getHazelcastConfig(springManagedContext, externalParsingProperties().getCluster()));
	}

	@Bean
	public HazelcastRpcTransport hzRpcTransport() {
		return new HazelcastRpcTransport(externalParsingHz());
	}

	@Bean
	public JsonRpcHandler jsonRpcHandler() {
		return new JsonRpcHandler(hzRpcTransport(), objectMapper);
	}

	@Bean
	public ExternalParsingService externalParsingService() {
		return new ExternalParsingService(jsonRpcHandler());
	}

	@Bean
	@ConditionalOnMissingBean
	public SpringManagedContext springManagedContext() {
		/* usually already provided by lib-job, but required here for tests */
		return new SpringManagedContext();
	}

	@Override
	protected Config getHazelcastConfig(final SpringManagedContext springManagedContext, final ClusterProperties clusterProperties) {
		final Config config = super.getHazelcastConfig(springManagedContext, clusterProperties);

		/* override cluster name */
		config.setClusterName(CLUSTER_NAME);

		/* automatically expire responses to external parsing requests after 5 minutes if they are not consumed */
		final MapConfig mapConfig = new MapConfig(RESPONSES_MAP_NAME);
		mapConfig.setTimeToLiveSeconds(RESPONSE_TTL);
		config.addMapConfig(mapConfig);

		return config;
	}
}
