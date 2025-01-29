/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.CLIENT_ADMIN;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.server.ResponseStatusException;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.ProjectNature;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * REST Controller for Project V2 requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class ProjectControllerV2 extends BaseController {

	/**
	 * URL pattern for count of Projects for given Client ID.
	 */
	public static final String PROJECT_COUNT_FOR_CLIENT_URL = "/v2/clients/{clientId}/projects/count";

	/**
	 * URL pattern for Projects for specified Client.
	 */
	public static final String PROJECT_COLLECTIONS_FOR_CLIENT_URL = "/v2/clients/{clientId}/projects";

	/**
	 * URL pattern for Project collection.
	 */
	public static final String PROJECT_COLELCTIONS_URL = "/v2/projects";

	/**
	 * URL pattern for Project by ID.
	 */
	public static final String PROJECT_BY_ID_URL = "/v2/projects/{projectId}";

	/**
	 * URL pattern for Project Natures by ID.
	 */
	public static final String PROJECT_NATURES_BY_ID = "/v2/projects/{projectId}/natures";
	
	private static final SortStringMapper<ProjectService.ProjectOrderBuilder> projectSortStringMapping = (q, field) -> {
		switch (field) {
			case "id":
				return q::sortNid;
			case "name":
				return q::sortName;
			default:
				return null;
		}
	};
	
	/**
	 * End-point to get the number of projects for a specific Client.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the Client
	 * @return The number of projects for the given Client
	 */
	@GetMapping(value = PROJECT_COUNT_FOR_CLIENT_URL)
	@Operation(summary = "Fetch number of projects for client", operationId = "findProjectCount")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role({VIEWER})
	public Long findNumberOfProjectsByClient(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final Long clientId) {
		validate(request);
		checkClientValid(EntityId.of(clientId));
		return projectService.count(q -> q.ofClient(EntityId.of(clientId)));
	}

	/**
	 * End-point to fetch a Page of Projects for a specific Client.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the Client
	 * @param page The page number
	 * @param size The page size
	 * @param sortBy The sorting conditions
	 * @return A Page of Projects.
	 */
	@GetMapping(value = PROJECT_COLLECTIONS_FOR_CLIENT_URL)
	@Operation(summary = "Paginates over the Projects for a Client", operationId = "findProjectsForClient")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role({VIEWER})
	public Paged<ProjectPojo> findProjectsForClient(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final Long clientId,
			@Parameter(description = "the page number to be fetched", schema = @Schema(type = "integer", format="int32", 
			defaultValue = "0"), required = false, example = "0")
			@RequestParam(value = "page", required = false, defaultValue = "0") final int page,
			@Parameter(description = "the size of each page", schema = @Schema(type = "integer", format="int32", defaultValue = "0", example = "10",
                    minimum = "1", maximum = "1073741823"), required = false)
			@RequestParam(value = "size", required = false, defaultValue = "0") final int size,
			@Parameter(description = "the sort conditions as string array", required = false, example = "new String[] {\"id;ASC\", \"name;ASC\"}")
			@RequestParam(value = "sortBy", required = false) final List<String> sortBy) {
		validate(request);
		checkClientValid(EntityId.of(clientId));
		return projectService.find(Pagination.at(page, size), q -> buildOrderFromString(q.ofClient(EntityId.of(clientId)), sortBy, projectSortStringMapping));
	}

	/**
	 * Endpoint to create a new Project and its associated roles in Keycloak.
	 * 
	 * @param request access to the request
	 * @param response HTTP response
	 * @param project Prototype of the Project to create.
	 * @return Newly created Project.
	 */
	@PostMapping(PROJECT_COLELCTIONS_URL)
	@Operation(summary = "Creates a new Project", operationId = "createProject")
	@ApiResponse(responseCode = "201", description = "if the project was created successfully")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@Nature({MINING})
	@Role(value = {CLIENT_ADMIN}, onAnyClient = true)
	public ProjectPojo create(final HttpServletRequest request, final HttpServletResponse response,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The project to create", required = true) 
			@RequestBody final ProjectPojoPrototype project) {
		validate(request);
		final Long clientId = project.client.getNonNull().getNid();
		if ( ! (user.isAdmin() || user.getClientAdminIds().contains(clientId))) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN, "No permission to create Projects for Client ID " +  clientId);
		}
		response.setStatus(HttpStatus.CREATED.value());
		return projectService.create(project);
	}

	/**
	 * Endpoint to delete Project in a cascading manner, along with the Keycloak roles.
	 *
	 * @param request access to the request
	 * @param projectId The ID of the Project
	 */
	@DeleteMapping(PROJECT_BY_ID_URL)
	@Operation(summary = "Deletes a project with complete cascading effect", operationId = "deleteProject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({CLIENT_ADMIN})
	public void delete(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the Project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		projectService.markForDeletion(projectId, true);
	}

	/**
	 * Endpoint to get default {@link ProjectNature project natures} for a specific project.
	 *
	 * @param request The HTTP Request
	 * @param projectId The ID of the Project
	 * @return The List of unique {@link ProjectNature project natures} assigned to the project
	 */
	@GetMapping(PROJECT_NATURES_BY_ID)
	@Operation(summary = "find the project natures", operationId = "findProjectNatures")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if project does not exist for the given projectId")
	@Nature({MINING})
	@Role({CLIENT_ADMIN})
	public List<ProjectNature> findProjectNatures(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project for which natures have to be fetched", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return projectService.findProjectNatures(projectId);
	}

	/**
	 * Endpoint to change default {@link ProjectNature project natures} for a specific project.
	 * This also changes the nature roles assigned to the project users replacing the default roles with the new ones being assigned.
	 * All users' "custom" natures are left untouched, this means that for every user we only remove/change the natures that are part of the default set.
	 *
	 * @param request access to the request
	 * @param projectId The ID of the Project
	 * @param natures The Set of {@link ProjectNature project natures} to assign
	 */
	@PutMapping(PROJECT_NATURES_BY_ID)
	@Operation(summary = "Changes the project natures", operationId = "changeProjectNatures")
	@ApiResponse(responseCode = "200", description = "if the project natures were changed successfully")
	@ApiResponse(responseCode = "400", description = "if the given natures param is not a valid collection of project natures")
	@ApiResponse(responseCode = "404", description = "if project does not exist for the given projectId")
	@Nature({MINING})
	@Role({CLIENT_ADMIN})
	public void changeProjectNatures(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project for which natures have to be changed", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The Set of project natures to assign. "
					+ "Available values : DISCOVERY, DISCOVERY_LIGHT, MINING, DB_CUTTER")
			@RequestBody final Set<ProjectNature> natures) {
		validate(request);
		projectService.changeProjectNatures(projectId, natures);
	}
}
