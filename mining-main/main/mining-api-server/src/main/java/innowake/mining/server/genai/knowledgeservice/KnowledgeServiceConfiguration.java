/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.knowledgeservice;

import innowake.lib.core.api.lang.Nullable;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;

/**
 * Configuration for establishing connectivity to the GenAI knowledge service via HTTP.
 * <p>
 * The GenAI knowledge service URL can be specified via the {@code mining.genai.knowledgeService.url} configuration parameter (e.g. via the application.yaml or command-line).
 * <p>
 */
@Configuration
@ConditionalOnExpression("!T(org.springframework.util.StringUtils).isEmpty('${mining.genAI.knowledgeService.url:}')")
public class KnowledgeServiceConfiguration {

    @Nullable
    @Value("${mining.genAI.knowledgeService.url}")
    private String url;

    @Autowired
    private ApplicationEventPublisher eventPublisher;

    @PostConstruct
    public void init() {
        if ( ! StringUtils.isBlank(url)) {
            eventPublisher.publishEvent(new KnowledgeServiceAvailabilityEvent(url));
        }
    }

    @Bean
    public KnowledgeQuery knowledgeService() {
        return new KnowledgeQuery(StringUtils.trimToNull(url));
    }

}
