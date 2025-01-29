package innowake.mining.opensearch.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.reactive.config.WebFluxConfigurationSupport;

/**
 * Configuration for the REST API of mining-opensearch.
 */
@Configuration
@EnableWebFlux
@ComponentScan("innowake.mining.opensearch.controller")
public class ControllerConfiguration extends WebFluxConfigurationSupport {
    /* empty for now */
}
