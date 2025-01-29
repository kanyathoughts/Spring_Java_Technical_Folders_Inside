/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.knowledgeservice;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.genai.requestresponse.KnowledgeServiceDocumentsRequest;
import innowake.mining.server.genai.requestresponse.KnowledgeServiceResponseModel;
import innowake.mining.server.genai.requestresponse.GenAiRequest;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.web.client.RootUriTemplateHandler;
import org.springframework.context.event.EventListener;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.*;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Service for querying documents of the GenAI knowledge service in order to retrieve additional context for AI generation.
 */
@Service
@Component
@ConditionalOnExpression("!T(org.springframework.util.StringUtils).isEmpty('${mining.genAI.knowledgeService.url:}')")
public class KnowledgeQuery {

    @Nullable
    private final String baseUrl;

    @Nullable
    private RestTemplate restTemplate;

    private static final Logger LOG = LoggerFactory.getLogger(KnowledgeQuery.class);

    public KnowledgeQuery(@Nullable final String baseUrl) {
        this.baseUrl = baseUrl;
        initRestTemplate(baseUrl);
    }

    @EventListener
    public void onKnowledgeServiceAvailability(final KnowledgeServiceAvailabilityEvent event) {
        initRestTemplate(event.getBaseUrl());
    }

    private void initRestTemplate(final String baseUrl) {
        if (baseUrl == null) {
            restTemplate = null;
        } else {
            restTemplate = new RestTemplate();
            restTemplate.setUriTemplateHandler(new RootUriTemplateHandler(baseUrl));
        }
    }

    /**
     * Returns whether (to the best of our knowledge) the GenAI knowledge service is currently available and can be contacted. Requests may still fail,
     * so callers should still be prepared to catch and handle exceptions when calling.
     *
     * @return {@code true} if the service is available.
     */
    public boolean isAvailable() {
        if (restTemplate == null) {
            return false;
        }
        final UriComponentsBuilder builder = UriComponentsBuilder.fromPath("/health");
        try {
             return Optional.of(Objects.equals(assertNotNull(restTemplate).exchange(builder.toUriString(), HttpMethod.GET, null, new ParameterizedTypeReference<String>() {}).getBody(),
					 "\"ok\"")).orElse(Boolean.FALSE);
        } catch (final Exception e) {
            return false;
        }
    }

    /**
     * Query documents of the GenAI Knowledge Service. This method will return context information queried from documents based on the passed parameters.
     * As the GenAI Knowledge Service is an external service which may not always be available, callers should be prepared to handle exceptions thrown from this method
     * and divert to fall-backs where needed.
     *
     * @param category category of the document(s) to be queried
     * @param payload the actual payload to query against, e.g. data fields
     * @param annotation the annotation the queried documents are related to
     * @param numOfDataFields number of data fields that are part of the annotation
     * @param requestUUID the uuid associated with the annotation description generation
     *
     * @return documents based on the passed parameters
     */
    public KnowledgeServiceResponseModel queryDocuments(final String category, final String payload, final AnnotationPojoPrototype annotation,
                                                        final int numOfDataFields, final UUID requestUUID) {
        LOG.debug("Querying GenAI Knowledge Service with category '" + category + "', payload '" + payload +"'");
        final KnowledgeServiceDocumentsRequest knowledgeServiceRequest = new KnowledgeServiceDocumentsRequest(category, payload,
                RequestMetadataUtil.getKnowledgeQueryMetadata(annotation, numOfDataFields, requestUUID));
        final KnowledgeServiceResponseModel responseObject = callKnowledgeService(knowledgeServiceRequest, KnowledgeServiceResponseModel.class, restTemplate);
        LOG.debug("Response from Knowledge Service: " + responseObject);

        return responseObject;
    }

    private <T extends GenAiRequest, R> R callKnowledgeService(final T request, final Class<R> responseType, final RestTemplate restTemplate) {
        LOG.debug("Request Payload for Knowledge Service : {}", request);
        try {
            final var headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            final ResponseEntity<R> exchange = restTemplate.exchange(baseUrl + KnowledgeServiceDocumentsRequest.ENDPOINT, HttpMethod.POST, new HttpEntity<>(request, headers), responseType);
            final R responseBody = Optional.ofNullable(exchange.getBody()).orElseThrow(() -> new IllegalStateException("The response body is null"));
            LOG.debug("Response from GenAI Knowledge Service - Status: {} ({}), Response JSON: {}", exchange.getStatusCode(), exchange.getStatusCodeValue(), responseBody);
            return responseBody;
        } catch (final Exception e) {
            LOG.error("Error while calling GenAI Knowledge Service", e);
            throw e;
        }
    }

}
