/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.service.semanticsearch.SemanticSearchResultPojo;
import innowake.mining.server.service.semanticsearch.SemanticSearchService;
import innowake.mining.shared.access.EntityId;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Proxy controller for handling requests to the semantic-search service.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class SemanticSearchController extends BaseController {

	private static final String SEARCH_RAG_URL = "/v1/projects/{projectId}/semantic-search/search-rag";
	private final SemanticSearchService semanticSearchService;

	public SemanticSearchController(final SemanticSearchService semanticSearchService) {
		this.semanticSearchService = semanticSearchService;
	}

	/**
	 * Runs a semantic-search request.
	 *
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId the ID of the project we search in
	 * @param query the search query, needs to be at least 5 characters long
	 * @return the semantic-search response
	 */
	@PostMapping(SEARCH_RAG_URL)
	@Operation(summary = "", operationId="searchRag")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature({MINING})
	@Role({VIEWER})
	public SemanticSearchResultPojo searchRag(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "1") @PathVariable final EntityId projectId,
			@Parameter(description = "the search query, needs to be at least 5 characters long", required = true, example = "Where is the interest rate calculated?") @RequestParam final String query) {
		validate(request);
		return semanticSearchService.doSemanticSearch(projectId, query);
	}
}
