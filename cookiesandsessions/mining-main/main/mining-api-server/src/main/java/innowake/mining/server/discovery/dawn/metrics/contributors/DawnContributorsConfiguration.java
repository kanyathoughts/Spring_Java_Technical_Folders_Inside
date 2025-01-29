/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class that enables component scanning of Dawn contributors.
 */
@Configuration
@ConditionalOnProperty("configuration.discovery-enable-dawn-contributors")
@ComponentScan("innowake.mining.server.discovery.dawn.metrics.contributors")
public class DawnContributorsConfiguration {
}
