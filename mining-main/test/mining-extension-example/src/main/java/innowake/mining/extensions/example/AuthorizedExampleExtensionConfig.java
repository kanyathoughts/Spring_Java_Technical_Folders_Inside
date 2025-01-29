/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.example;

import innowake.mining.server.config.Profiles;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
 
@Configuration
@EnableWebSecurity
@Profile({
	Profiles.AUTH, Profiles.NO_AUTH
})
public class AuthorizedExampleExtensionConfig {
 
    @Bean
    @Order(1)
    public SecurityFilterChain configure(final HttpSecurity http) throws Exception {
        return http.antMatcher("/my/extension-url/**")
                .headers().frameOptions().sameOrigin()
                .and()
                .authorizeRequests()
                .antMatchers("/my/extension-url/**")
                .permitAll()
                .and()
                .build();
    }
}