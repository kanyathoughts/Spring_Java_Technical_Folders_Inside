/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import org.ff4j.FF4j;
import org.ff4j.web.FF4jDispatcherServlet;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configure the ff4j feature web console.
 */
@Configuration
@ConditionalOnClass(FF4jDispatcherServlet.class)
@AutoConfigureAfter(FF4JConfiguration.class)
public class FF4jWebConfiguration extends SpringBootServletInitializer {
	
    /**
     * Create the servlet registration and url mapping for the ff4j web console.
     *
     * @param ff4jDispatcherServlet The ff4j dispatch servlet {@link #getFF4jDispatcherServlet(FF4j)}.
     * @return The dispatch servlet instance.
     */
    @Bean
    public ServletRegistrationBean<FF4jDispatcherServlet> consoleVersion(final FF4jDispatcherServlet ff4jDispatcherServlet) {
    	return new ServletRegistrationBean<>(ff4jDispatcherServlet, "/feature-console/*");
    }

    /**
     * Create the ff4j web console servlet.
     *
     * @param ff4j The ff4j instance to serve in the web console.
     * @return The servlet instance.
     */
    @Bean
    @ConditionalOnMissingBean
    public FF4jDispatcherServlet getFF4jDispatcherServlet(final FF4j ff4j) {
        final FF4jDispatcherServlet ff4jDispatcherServlet = new FF4jDispatcherServlet();
        ff4jDispatcherServlet.setFf4j(ff4j);
        return ff4jDispatcherServlet;
    }

}
