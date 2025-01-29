/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.service.UniversalSearchService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.universalsearch.UniversalSearchResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for the universal search requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class UniversalSearchController extends BaseController {
	
	/**
	 * URL pattern for Universal search.
	 */
	public static final String UNIVERSAL_SEARCH_URL = "/v1/projects/{projectId}/universal-search";
	
	private final UniversalSearchService universalSearchService;
	
	@Autowired
	public UniversalSearchController(final UniversalSearchService universalSearchService) {
		this.universalSearchService = universalSearchService;
	}
	
	/**
	 * Searches the query among modules, data dictionaries, etc. in a project and returns matched results.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id
	 * @param query the term to be searched
	 * @return List of {@link UniversalSearchResult}
	 */
	@GetMapping(value = UNIVERSAL_SEARCH_URL)
	@Operation(summary = "Searches in the entire project", operationId = "searchQueryInProject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the project does not exist")
	@Nature({DISCOVERY, MINING})
	@Role({VIEWER})
	public List<UniversalSearchResult>  searchQueryInProject(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the term to be searched", required = true, example = "MMRS71O")
			@RequestParam final String query) {
		validate(request);
		return universalSearchService.query(projectId, query);
	}

}
