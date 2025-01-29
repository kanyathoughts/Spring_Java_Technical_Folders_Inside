/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.CLIENT_ADMIN;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.server.ResponseStatusException;

import innowake.mining.server.config.security.ManualSecurityWithoutProjectAssociation;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.ProjectCreatedEvent;
import innowake.mining.server.service.AuthorizationManagementService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for Project requests.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class ProjectController extends BaseController {

	/**
	 * URL pattern for the project collection.
	 */
	public static final String PROJECT_COLLECTION_URL = "/v1/projects";

	/**
	 * URL pattern for a single project by ID.
	 */
	public static final String PROJECT_BY_ID_URL = "/v1/projects/{projectId}";
	
	/**
	 * URL pattern for a project's default configuration.
	 */
	public static final String PROJECT_DEFAULT_CONFIGURATION = "/v1/projects/{projectId}/defaultConfiguration";
	
	/**
	 * URL pattern for a project's auto-completion map.
	 */
	public static final String PROJECT_AUTO_COMPLETION = "/v1/projects/{projectId}/autoCompletion";
	
	/*
	 * URL pattern for a project's custom enum values.
	 */
	public static final String PROJECT_ENUM_VALUES = "/v1/projects/{projectId}/enumValues";
	
	/**
	 * URL pattern for a project's auto-completion lists.
	 */
	public static final String PROJECT_AUTO_COMPLETION_LIST = PROJECT_AUTO_COMPLETION + "/{key}";
	
	/**
	 * URL pattern for a project's enum values.
	 */
	public static final String PROJECT_ENUM_VALUES_LIST = PROJECT_ENUM_VALUES + "/{key}";
	
	/**
	 * URL pattern for Renaming a value of auto-completion list of a project.
	 */
	public static final String RENAME_AUTO_COMPLETION_VALUE = PROJECT_AUTO_COMPLETION_LIST + "/{oldValue}/{newValue}";
	
	/**
	 * URL pattern for Renaming an enum value of a project.
	 */
	public static final String RENAME_ENUM_VALUE = PROJECT_ENUM_VALUES_LIST + "/rename";
	
	/**
	 * URL pattern for Deleting a value of auto-completion list of a project.
	 */
	public static final String DELETE_AUTO_COMPLETION_VALUE = PROJECT_AUTO_COMPLETION_LIST + "/{value}";
	
	/**
	 * URL pattern for Deleting an enum value of a project.
	 */
	public static final String DELETE_ENUM_VALUE = PROJECT_ENUM_VALUES_LIST + "/{value}";
	
	/**
	 * URL pattern for Projects for specified Client.
	 */
	public static final String PROJECT_COLLECTIONS_FOR_CLIENT_URL = "/v1/clients/{clientId}/projects";
	/**
	 * URL pattern to get and save reachability analysis config for upper bound and lower bound.
	 */
	public static final String FUNCTIONAL_BLOCK_REACHABILITY_CONFIG = "/v1/projects/{projectId}/reachability-analysis-config";

	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	@Autowired
	private AuthorizationManagementService authorizationManagementService;
	
	/**
	 * Lists all available {@linkplain ProjectPojo projects}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param deleted whether to include projects marked for deletion
	 * @return a list of Projects
	 */
	@GetMapping(PROJECT_COLLECTION_URL)
	@Operation(summary = "List all existing projects", operationId = "findAllProjects")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({VIEWER})
	@ManualSecurityWithoutProjectAssociation
	public List<ProjectPojo> findAll(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "whether to include projects marked for deletion", required = false)
			@RequestParam final Optional<Boolean> deleted) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return projectService.find(q -> {
			if (deleted.orElse(false)) {
				q.filterMarkedForDeletion(null);
			}
			if (user.isAdmin()) {
				q.withIdAbove(0l);
			} else {
				q.withIds(user.getProjectIds(), user.getClientAdminIds());
			}
		});
	}
	
	/**
	 * Finds a Project by ID.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the project to find
	 * @return a {@link ProjectPojo}
	 */
	@GetMapping(PROJECT_BY_ID_URL)
	@Operation(summary = "Find a project by id", operationId = "findProjectById")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project ID does not exist")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({VIEWER})
	public ProjectPojo findById(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return projectService.get(projectId);
	}
	
	/**
	 * Creates a new Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param project the Project to create 
	 * @return the new Project
	 */
	@PostMapping(PROJECT_COLLECTION_URL)
	@Operation(summary = "Create a new project", operationId = "createProject")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "405", description = "if the endpoint is access in IAM profile")
	@Nature({MINING})
	@Role(value = {CLIENT_ADMIN}, onAnyClient = true)
	public ProjectPojo create(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description  = "The project to create", required = true) 
			@RequestBody final ProjectPojoPrototype project) {
		validate(request);
		if (authorizationManagementService.isAuthorizedAccess()) {
			throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED, "Please use the V2 endpoint.");
		}
		final ProjectPojo newProject = projectService.create(project, false);
		response.setStatus(HttpStatus.CREATED.value());
		eventPublisher.publishEvent(new ProjectCreatedEvent(newProject.identity()));
		return newProject;
	}
	
	/**
	 * Updates an existing Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the project to update
	 * @param project the Project to update 
	 * @return the updated Project
	 */
	@PutMapping(PROJECT_BY_ID_URL)
	@Operation(summary = "Update an existing project", operationId = "updateProject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({CLIENT_ADMIN})
	public ProjectPojo update(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project to be updated", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated project", required = true) 
			@RequestBody final ProjectPojoPrototype project) {
		validate(request);
		
		if (project.client.isDefined()) {
			final Long changedClientId = project.client.getNonNull().getNid();
			final Long currentClientId = projectService.get(project.identityProvisional()).getClientNid();
			if (currentClientId.equals(changedClientId)) {
				project.client.unset();
			} else if (! user.getClientAdminIds().containsAll(List.of(currentClientId, changedClientId))) {
				throw new AccessDeniedException("Admin rights required to assign Project to Client");
			}
		}
		response.setStatus(HttpStatus.OK.value());
		projectId.matchOrApply(project.identityProvisional(), project::withId);
		
		return projectService.get(projectService.update(project));
	}
	
	/**
	 * @deprecated This method has been deprecated in favor 
	 * of {@link #getEnumKeys(HttpServletRequest, HttpServletResponse, EntityId)} 
	 * (/v1/projects/{projectId}/enumValues) 
	 * which performs the same operation, but with a name that better reflects the flexible 
	 * functionality.
	 * 
	 * Retrieves the auto-completion keys of a Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @return set of defined auto-completion identifiers
	 */
	@GetMapping(PROJECT_AUTO_COMPLETION)
	@Operation(summary = "Get the auto-completion list identifiers defined on a project", operationId = "getAutoCompletionKeys")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	@Deprecated(forRemoval = true)
	public Set<String> getAutoCompletionKeys(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return new HashSet<>(customPropertiesService.getEnumNames(projectId));
	}
	
	/**
	 * Retrieves the custom enum keys of a Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @return set of defined custom enum identifiers
	 */
	@GetMapping(PROJECT_ENUM_VALUES)
	@Operation(summary = "Get the auto-completion list identifiers defined on a project", operationId = "getAutoCompletionKeys")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	public Set<String> getEnumKeys(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return customPropertiesService.getEnumNames(projectId);
	}
	
	/**
	 * @deprecated This method has been deprecated in favor 
	 * of {@link #getEnumValues(HttpServletRequest, HttpServletResponse, EntityId, String)} 
	 * (/v1/projects/{projectId}/enumValues) 
	 * which performs the same operation, but with a name that better reflects the flexible 
	 * functionality.
	 * 
	 * Retrieves an auto-completion list from a Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the auto-completion list 
	 * @return list of auto-completion entries
	 */
	@GetMapping(PROJECT_AUTO_COMPLETION_LIST)
	@Operation(summary = "Retrieve an auto-completion list from a project", operationId = "getAutoCompletionList")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	@Deprecated(forRemoval = true)
	public Set<String> getAutoCompletionList(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the auto-completion list", required = true)
			@PathVariable final String key) {
		validate(request, "key");
		response.setStatus(HttpStatus.OK.value());
		return customPropertiesService.getEnumValues(projectId, key);
	}
	
	/**
	 * Retrieves enum values from a Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the enum values 
	 * @return list of enum values
	 */
	@GetMapping(PROJECT_ENUM_VALUES_LIST)
	@Operation(summary = "Retrieve enum values from a project", operationId = "getEnumValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	public Set<String> getEnumValues(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the enum values", required = true)
			@PathVariable final String key) {
		validate(request, "key");
		response.setStatus(HttpStatus.OK.value());
		return customPropertiesService.getEnumValues(projectId, key);
	}
	
	/**
	 * @deprecated This method has been deprecated in favor of 
	 * {@link #setEnumValues(HttpServletRequest, HttpServletResponse, EntityId, String, List)} 
	 * (v1/projects/{projectId}/enumValues)
	 * which performs the same operation, but with a name that better reflects the flexible 
	 * functionality.
	 * 
	 * Sets the entries on an auto-completion list for a Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the auto-completion list 
	 * @param entries new list of auto-completion entries
	 */
	@PutMapping(PROJECT_AUTO_COMPLETION_LIST)
	@Operation(summary = "Set the entries of an auto-completion list for a project", operationId = "setAutoCompletionList")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	@Deprecated(forRemoval = true)
	public void setAutoCompletionList(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the auto-completion list", required = true)
			@PathVariable final String key,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "Entries") 
			@RequestBody final List<String> entries) {
		validate(request, "key");
		customPropertiesService.putEnumValues(projectId, Collections.singletonMap(key, new HashSet<>(entries)));
		response.setStatus(HttpStatus.OK.value());
	}
	
	/**
	 * Sets the enum values for a Project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the enum value
	 * @param values new list enum value entries
	 */
	@PutMapping(PROJECT_ENUM_VALUES_LIST)
	@Operation(summary = "Set the enum values for a project.", operationId = "setEnumValues")
	@ApiResponse(responseCode = "201", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	public void setEnumValues(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the enum value", required = true)
			@PathVariable final String key,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The values") 
			@RequestBody final Set<String> values) {
		validate(request, "key");
		customPropertiesService.putEnumValues(projectId, Collections.singletonMap(key, values));
		response.setStatus(HttpStatus.CREATED.value());
	}
	
	/**
	 * @deprecated This method has been deprecated in favor of 
	 * {@link #renameEnumValues(HttpServletRequest, HttpServletResponse, EntityId, String, Map)}
	 * (v1/projects/{projectId}/enumValues/{key}})
	 * which performs a similar, but with a name that better reflects the flexible 
	 * functionality.
	 * 
	 * Rename an auto completion list value to a new value.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the auto-completion list
	 * @param oldValue the value to be renamed in auto-completion list
	 * @param newValue the value to be renamed to in auto-completion list
	 */
	@PutMapping(RENAME_AUTO_COMPLETION_VALUE)
	@Operation(summary = "Rename an entry in the auto-completion list for the received key for a project", operationId = "renameAutoCompletionValue")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	@Deprecated(forRemoval = true)
	public void renameAutoCompletionValue(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the auto-completion list", required = true)
			@PathVariable final String key,
			@Parameter(description = "the old value in the auto-completion list", required = true)
			@PathVariable final String oldValue,
			@Parameter(description = "the value to be renamed to in the auto-completion list", required = true)
			@PathVariable final String newValue) {
		validate(request, "key", "oldValue", "newValue");
		customPropertiesService.renameEnumValue(projectId, key, oldValue, newValue);
	}
	
	/**
	 * Rename an enum value to a new value.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the enum value
	 * @param map a mapping of the old values values to the new values in the enum
	 */
	@PostMapping(RENAME_ENUM_VALUE)
	@Operation(summary = "Rename an enum value for the received key for a project", operationId = "renameEnumValue")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	public void renameEnumValues(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the enum value", required = true)
			@PathVariable final String key,
			@Parameter(description = "a mapping of the old enum value to the new enum value", required = true)
			@RequestBody final Map<String, String> map) {
		validate(request, "key", "map");
		for (final Entry<String, String> entry : map.entrySet()) {
			customPropertiesService.renameEnumValue(projectId, key, entry.getKey(), entry.getValue());
			}
		}
	
	/**
	 * @deprecated This method has been deprecated in favor of 
	 * {@link #deleteEnumValue(HttpServletRequest, HttpServletResponse, EntityId, String, String)} 
	 * (v1/projects/{projectId}/enumValues/{key}/{value})
	 * which performs the same operation, but with a name that better reflects the flexible 
	 * functionality.
	 * 
	 * Deletes the received value from auto-completion list for the given key.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the auto-completion list
	 * @param value the value to be deleted
	 */
	@DeleteMapping(DELETE_AUTO_COMPLETION_VALUE)
	@Operation(summary = "Delete an entry in the auto-completion list for the received key for a project", operationId = "deleteAutoCompletionValue")
	@ApiResponse(responseCode = "204", description = "if the entry is successfully deleted")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	@Deprecated(forRemoval = true)
	public void deleteAutoCompletionValue(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the auto-completion list", required = true)
			@PathVariable final String key,
			@Parameter(description = "the value to be deleted from auto-completion list", required = true)
			@PathVariable final String value) {
		validate(request, "key", "value");
		customPropertiesService.removeEnumValues(projectId, key, Collections.singletonList(value));
	}
	
	/**
	 * Deletes the received value from the enum value for the given key.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the id of the project
	 * @param key the identifier of the auto-completion list
	 * @param value the value to be deleted
	 */
	@DeleteMapping(DELETE_ENUM_VALUE)
	@Operation(summary = "Delete an enum value for the received key for a project", operationId = "deleteEnumValue")
	@ApiResponse(responseCode = "204", description = "if the enum value is successfully deleted")
	@ApiResponse(responseCode = "404", description = "if no project with the provided ID exists")
	@Nature({MINING})
	@Role({MANAGER})
	public void deleteEnumValue(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the key of the enum values", required = true)
			@PathVariable final String key,
			@Parameter(description = "the value to be deleted from the enum value list", required = true)
			@PathVariable final String value) {
		validate(request, "key", "value");
		customPropertiesService.removeEnumValues(projectId, key, Collections.singletonList(value));
	}
	
	/**
	 * Resets a new Project's configuration.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId ID of the Project to reset 
	 */
	@PostMapping(PROJECT_DEFAULT_CONFIGURATION)
	@Operation(summary = "Reset a projects configuration", operationId = "setProjectDefaultConfiguration")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given project is not valid")
	@Nature({MINING})
	@Role({MANAGER})
	public void setDefaultConfiguration(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		projectService.resetConfiguration(projectId);
		response.setStatus(HttpStatus.OK.value());
	}
	
	/**
	 * Lists all available Projects for a specific Client.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the Client
	 * @return a list of Projects
	 */
	@GetMapping(value = PROJECT_COLLECTIONS_FOR_CLIENT_URL)
	@Operation(summary = "List all existing projects of a Client", operationId = "findProjectsForClient")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role({VIEWER})
	public List<ProjectPojo> findProjectsForClient(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final Long clientId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		checkClientValid(EntityId.of(clientId));
		return projectService.find(q -> {
			if (user.isAdmin()) {
				q.withIdAbove(Long.valueOf(0l));
			} else {
				q.withIds(user.getProjectIds(), user.getClientAdminIds());
			}

			q.ofClient(EntityId.of(clientId));
		});
	}

	/**
	 * Saves the reachability analysis configuration for the project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @param reachabilityAnalysisConfiguration the {@link ReachabilityAnalysisConfig} to update
	 */
	@PostMapping(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG)
	@Operation(summary = "Saves the reachability analysis configuration for the project", operationId = "saveReachabilityAnalysisConfiguration")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	public void saveReachabilityAnalysisConfiguration(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The reachability analysis configuration", required = true)
			@RequestBody final ReachabilityAnalysisConfig reachabilityAnalysisConfiguration) {
		validate(request);
		projectService.putConfig(projectId, ReachabilityAnalysisConfig.CONFIG_NAME, reachabilityAnalysisConfiguration);
	}

	/**
	 * Retrieves the reachability analysis configuration for the project.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block
	 * @return the {@link ReachabilityAnalysisConfig} for the project
	 */
	@GetMapping(FUNCTIONAL_BLOCK_REACHABILITY_CONFIG)
	@Operation(summary = "Retrieves the reachability analysis configuration for the project", operationId = "getReachabilityAnalysisConfiguration")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public ReachabilityAnalysisConfig getReachabilityAnalysisConfiguration(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "return the default configuration", example = "true")
			@RequestParam(required = false) final boolean reset) {
		validate(request);
		if (reset) {
			return ReachabilityAnalysisConfig.defaultConfig();
		}
		return projectService.getConfigByName(projectId, ReachabilityAnalysisConfig.class, ReachabilityAnalysisConfig.CONFIG_NAME).orElse(null);
	}
}
