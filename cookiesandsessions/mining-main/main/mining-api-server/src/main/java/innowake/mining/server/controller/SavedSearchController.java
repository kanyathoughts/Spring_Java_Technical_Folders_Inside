/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.server.ResponseStatusException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.ConstraintValidator;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.SavedSearchModifiedEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SavedSearchService;
import innowake.mining.shared.entities.SavedSearchPojo;
import innowake.mining.shared.entities.SavedSearchPojoPrototype;
import innowake.mining.shared.model.SavedSearchCountResponse;
import innowake.mining.shared.model.ScopeEnum;
import innowake.mining.shared.service.UserRoleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for the {@code SavedSearch} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class SavedSearchController extends BaseController {
	
	public static final String SAVED_SEARCH_COLLECTION_URL = "/v1/projects/{projectId}/savedSearches";
	
	public static final String SAVED_SEARCH_BY_ID = "/v1/projects/{projectId}/savedSearches/{id}";
	
	public static final String SAVED_SEARCH_BY_USAGE = "/v1/projects/{projectId}/savedSearches/{usage}";
	
	public static final String SAVED_SEARCH_COUNTS_BY_PROJECT_ID = "/v1/projects/{projectId}/dashboard/savedSearchCounts";
	
	public static final String SAVED_SEARCH_AGGREGATION = "SavedSearchAggregation";
	
	@Autowired
	private SavedSearchService savedSearchService;
	
	@Autowired
	private UserRoleService userRoleService;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	/**
	 * Creates a new {@code SavedSearch} record in provided project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the {@code SavedSearch}
	 * @param savedSearch the {@code SavedSearch} to create
	 * @return Created {@code SavedSearch} record
	 */
	@PostMapping(SAVED_SEARCH_COLLECTION_URL)
	@Operation(summary = "Create a new Saved Search record", operationId = "createSavedSearch")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given savedSearch is not valid")
	@ApiResponse(responseCode = "403", description = "not authorized to create saved search")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public SavedSearchPojo create(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The savedSearch to create", required = true) 
			@RequestBody final SavedSearchPojoPrototype savedSearch) {
		validate(request);

		if (savedSearch.project.isPresent() || savedSearch.client.isPresent()) {
			validateGlobalSavedSearchProject(savedSearch);
			savedSearch.setCreatedByUserId(authentication.getUserId());
			validateCreateUpdateDeleteAccessGlobalSavedSearch(savedSearch.createdByUserId.orElse(null), savedSearch.scope.orElse(null), savedSearch::toString);
		}
		response.setStatus(HttpStatus.CREATED.value());

		final Long id = savedSearchService.create(savedSearch);
		eventPublisher.publishEvent(new SavedSearchModifiedEvent(projectId));
		return setUserName(savedSearchService.get(id));
	}
	
	/**
	 * Deletes a {@code SavedSearch} record from provided project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the {@code SavedSearch}
	 * @param id of the {@code SavedSearch} to be deleted
	 */
	@DeleteMapping(SAVED_SEARCH_BY_ID)
	@Operation(summary = "Delete a Saved Search record", operationId = "deleteSavedSearch")
	@ApiResponse(responseCode = "204", description = "if the Saved Search was successfully deleted")
	@ApiResponse(responseCode = "403", description = "not authorized to delete saved search")
	@ApiResponse(responseCode = "404", description = "if the project does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void delete(final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the id of the Saved Seach to be deleted", required = true, example = "1")
			@PathVariable final Long id) {
		validate(request, "id");
		final Optional<SavedSearchPojo> savedSearch = savedSearchService.findAny(q -> q.byId(id));
		if (savedSearch.isEmpty()) {
			throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Saved search not found for id: " + id);
		}

		validateCreateUpdateDeleteAccessGlobalSavedSearch(savedSearch.get().getCreatedByUserId().orElse(null), savedSearch.get().getScope(), savedSearch::toString);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		savedSearchService.delete(q -> q.byId(id));
		eventPublisher.publishEvent(new SavedSearchModifiedEvent(projectId));
	}
	
	/**
	 * Find all the global and custom {@code SavedSearch} by the provided usage and project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the {@code SavedSearch}
	 * @param usage usage of saved search
	 * @return List of {@code SavedSearch}
	 */
	@GetMapping(value = SAVED_SEARCH_BY_USAGE)
	@Operation(summary = "Find Saved Search records by its usage", operationId = "findByUsage")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<SavedSearchPojo> findByUsage(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the usage of the Saved Search", required = true, example = "Annotation")
			@PathVariable final String usage) {
		validate(request, "usage");
		return setUserNames(savedSearchService.findByUsageAndUserId(projectId, usage, authentication.getUserId()));
	}
	
	/**
	 * Updates an existing {@code SavedSearch} record in provided project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the {@code SavedSearch}
	 * @param id of the {@code SavedSearch}
	 * @param savedSearch the {@code SavedSearch} to update
	 * @return the updated {@code SavedSearch}
	 */
	@PutMapping(SAVED_SEARCH_BY_ID)
	@Operation(summary = "Update a Saved Search record", operationId = "updateSavedSearch")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given savedSearch is not valid")
	@ApiResponse(responseCode = "403", description = "not authorized to update saved search")
	@ApiResponse(responseCode = "404", description = "if the given project or name does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public SavedSearchPojo update(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the id of the Saved Seach to be updated", required = true, example = "1")
			@PathVariable final Long id,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The savedSearch to update", required = true) 
			@RequestBody final SavedSearchPojoPrototype savedSearch) {
		validate(request, "id");
		if (savedSearch.project.isPresent() || savedSearch.client.isPresent()) {
			validateGlobalSavedSearchProject(savedSearch);
			validateCreateUpdateDeleteAccessGlobalSavedSearch(savedSearch.createdByUserId.orElse(null), savedSearch.scope.orElse(null), savedSearch::toString);
		}
		response.setStatus(HttpStatus.OK.value());

		if (savedSearch.id.isPresent()) {
			if ( ! id.equals(savedSearch.id.orElse(null))) {
				throw new ConstraintViolationException("Error while updating saved search : Id cannot be updated");
			}
		} else {
			savedSearch.setId(id);
		}
		if ( ! savedSearch.name.isPresent()) {
			throw new ConstraintViolationException("Name must not be null");
		}

		savedSearchService.update(projectId, savedSearch);
		eventPublisher.publishEvent(new SavedSearchModifiedEvent(projectId));
		return setUserName(savedSearchService.get(id));
	}
	
	/**
	 * Returns the count of the SavedSearches by the provided projectId.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the {@code SavedSearch}
	 * @return List<SavedSearchCountResponse> a List of the {@link SavedSearchCountResponse}
	 */
	@GetMapping(value = SAVED_SEARCH_COUNTS_BY_PROJECT_ID)
	@Operation(summary = "Returns the count of the SavedSearches for the provided projectId.", method = "getDashboardSavedSearchCounts")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<SavedSearchCountResponse> getDashboardSavedSearchCounts(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return savedSearchService.getSavedSearchCounts(projectId);
	}

	private void validateGlobalSavedSearchProject(final SavedSearchPojoPrototype savedSearch) {
		if ( ! savedSearch.project.isPresent()) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN, "Project id must be set in saved search pojo");
		}

		final Long projectNid = projectService.getNid(savedSearch.project.getNonNull());
		final ScopeEnum scope = savedSearch.scope.orElse(null);
		if (scope == ScopeEnum.GLOBAL && projectNid != 0) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN, 
					"Saved search with scope 'GLOBAL' can only be created/updated for project 0");
		} else if (scope != ScopeEnum.GLOBAL && projectNid == 0) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN,
					String.format("Saved search with scope %s cannot be created/updated for project 0", scope));
		}
	}

	private void validateCreateUpdateDeleteAccessGlobalSavedSearch(@Nullable final String createdByUserId, @Nullable final ScopeEnum scope, final Supplier<String> errorString) {
		if (scope == ScopeEnum.GLOBAL && ! (userRoleService.isAdmin())) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN,
				"Saved search with scope 'GLOBAL' can only be created/deleted/updated by admins");
		} else {
			ConstraintValidator.validate(() -> createdByUserId, authentication.getUserId(), "One cannot delete/update others created saved search",
					errorString.get());
		}
	}

	private SavedSearchPojo setUserName(final SavedSearchPojo savedSearch) {
		savedSearch.setCreatedByUserName(userUtil.getUserName(savedSearch.getCreatedByUserId()));
		return savedSearch;
	}

	private List<SavedSearchPojo> setUserNames(final List<SavedSearchPojo> savedSearches) {
		savedSearches.forEach(this::setUserName);
		return savedSearches;
	}
}
