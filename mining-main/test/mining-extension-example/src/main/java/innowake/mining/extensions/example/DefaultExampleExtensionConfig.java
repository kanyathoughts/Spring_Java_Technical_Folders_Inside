/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.example;

import innowake.mining.server.config.Profiles;
import org.springframework.context.annotation.Profile;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
 
@EnableWebSecurity
@Order(Ordered.HIGHEST_PRECEDENCE)
@Profile(Profiles.LEGACY_AUTH)
public class DefaultExampleExtensionConfig extends WebSecurityConfigurerAdapter {
 
    @Override
    protected void configure(final HttpSecurity http) throws Exception {
        http.antMatcher("/my/extension-url/**")
            .headers().frameOptions().sameOrigin()
            .and()
            .authorizeRequests()
            .antMatchers("/my/extension-url/**")
            .permitAll();
    }
}