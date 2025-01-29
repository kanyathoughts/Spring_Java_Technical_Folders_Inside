/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test.contributors.hello;


import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class HelloContributorsConfiguration {

	@Bean
	public HelloContributor helloContributor() {
		return new HelloContributor();
	}

	@Bean
	public HelloContributorFromSource helloContributorFromSource() {
		return new HelloContributorFromSource();
	}
	
	@Bean
	public HelloDependencyContributor helloDependencyContributor() {
		return new HelloDependencyContributor();
	}
	
	@Bean
	public HelloContributorForDefaultModules helloContributorForDefaultModules() {
		return new HelloContributorForDefaultModules();
	}
}
