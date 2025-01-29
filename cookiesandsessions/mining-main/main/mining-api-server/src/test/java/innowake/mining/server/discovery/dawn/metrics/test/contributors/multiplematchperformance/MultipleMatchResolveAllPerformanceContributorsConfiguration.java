/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematchperformance;


import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.integration.discovery.MultipleMatchResolveAllPerformanceTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class for {@link MultipleMatchResolveAllPerformanceTest}.
 */
@Configuration
public class MultipleMatchResolveAllPerformanceContributorsConfiguration {

	@Bean
	public DiscoveryContributor testContributor() {
		return new MultipleMatchResolveAllPerformanceContributor();
	}
}
