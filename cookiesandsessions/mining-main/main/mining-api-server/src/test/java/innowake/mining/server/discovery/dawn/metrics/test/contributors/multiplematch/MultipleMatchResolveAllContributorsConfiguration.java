/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematch;


import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.integration.discovery.MultipleMatchResolveAllTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class for {@link MultipleMatchResolveAllTest}.
 */
@Configuration
public class MultipleMatchResolveAllContributorsConfiguration {

	@Bean
	public DiscoveryContributor testContributor() {
		return new MultipleMatchResolveAllContributor();
	}
}
