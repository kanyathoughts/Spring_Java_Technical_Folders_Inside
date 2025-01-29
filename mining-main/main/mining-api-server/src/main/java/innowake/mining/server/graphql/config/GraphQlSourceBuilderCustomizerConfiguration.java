/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.config;

import org.springframework.boot.autoconfigure.graphql.ConditionalOnGraphQlSchema;
import org.springframework.boot.autoconfigure.graphql.GraphQlAutoConfiguration;
import org.springframework.boot.autoconfigure.graphql.GraphQlSourceBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Required, because {@link GraphQlAutoConfiguration} has a new condition
 * {@link ConditionalOnGraphQlSchema} which checks that either
 * <ul>
 *     <li>GraphQL schema files are available on the classpath</li>
 *     <li>at least one {@code GraphQlSourceBuilderCustomizer} bean is registered</li>
 * </ul>
 * We don't need either of that because we directly configure our own {@link MiningGraphQlSource}
 * with custom schema. Still we need to register one dummy {@code GraphQlSourceBuilderCustomizer}
 * or else the auto configuration will not run.
 */
@Configuration
public class GraphQlSourceBuilderCustomizerConfiguration {

	@Bean
	public GraphQlSourceBuilderCustomizer dummyCustomizer() {
		return builder -> {};
	}
}
