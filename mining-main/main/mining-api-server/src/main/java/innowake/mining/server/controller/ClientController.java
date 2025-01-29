/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.*;
import static innowake.mining.shared.security.RoleType.*;

import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.server.ResponseStatusException;
import innowake.mining.server.config.security.ManualSecurityWithoutProjectAssociation;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.service.AuthorizationManagementService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
/**
 * Controller for the {@link ClientPojo} requests.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class ClientController extends BaseController {
	
	/**
	 * URL pattern for the collection of clients.
	 */
	public static final String CLIENT_COLLECTION_URL = "/v1/clients";

	/**
	 * URL pattern for a single client by ID.
	 */
	public static final String CLIENT_BY_ID_URL = "/v1/clients/{clientId}"; 

	@Autowired
	private AuthorizationManagementService authorizationManagementService;

	/**
	 * Lists all available {@linkplain ClientPojo clients}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @return a list of {@linkplain ClientPojo clients}
	 */
	@GetMapping(CLIENT_COLLECTION_URL)
	@Operation(summary = "List all available clients.", operationId = "findAllClients")
	@ApiResponse(responseCode = "200", description = "all clients")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({VIEWER})
	@ManualSecurityWithoutProjectAssociation
	public List<ClientPojo> findAll(final HttpServletRequest request, final HttpServletResponse response) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return clientService.find(q -> {
			if (user.isAdmin()) {
				q.withIdAbove(Long.valueOf(0L));
			} else {
				q.withIds(user.getClientIds());
			}
			q.withMarkedForDeletion(false);
			q.sortNid(SortDirection.ASCENDING);
		});
	}
	
	/**
	 * Finds a {@link ClientPojo} by an id.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param clientId the UUID or numeric ID of the client to find
	 * @return a {@link ClientPojo}
	 */
	@GetMapping(CLIENT_BY_ID_URL)
	@Operation(summary = "Finds a client by its ID.", operationId = "findClientById")
	@ApiResponse(responseCode = "200", description = "found Client")
	@ApiResponse(responseCode = "404", description = "Client not found")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role({VIEWER})
	public ClientPojo findById(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the client to find", required = true, example = "0") @PathVariable final EntityId clientId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return clientService.get(clientId, true);
	}
	
	/**
	 * Creates a new {@link ClientPojo}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param client the {@link ClientPojoPrototype} to create 
	 * @return the new {@link ClientPojo}
	 */
	@PostMapping(CLIENT_COLLECTION_URL)
	@Operation(summary = "Create a new client", operationId = "createClient")
	@ApiResponse(responseCode = "201", description = "Client created successfully")
	@ApiResponse(responseCode = "400", description = "Invalid client supplied")
	@ApiResponse(responseCode = "405", description = "If the endpoint is accessed in IAM profile")
	@Nature({MINING})
	@Role({ADMIN})
	public ClientPojo create(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The client to create", required = true) 
			@RequestBody final ClientPojoPrototype client) {
		validate(request);
		if (authorizationManagementService.isAuthorizedAccess()) {
			throw new ResponseStatusException(HttpStatus.METHOD_NOT_ALLOWED, "Please use the V2 endpoint.");
		}
		response.setStatus(HttpStatus.CREATED.value());
		return clientService.create(client);
	}
	
	/**
	 * Updates an existing {@link ClientPojo}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param clientId the id of the {@link ClientPojo} to update 
	 * @param client the {@link ClientPojo} to update 
	 * @return the updated {@link ClientPojo}
	 */
	@PutMapping(CLIENT_BY_ID_URL)
	@Operation(summary = "Update an existing client", operationId = "updateClient")
	@ApiResponse(responseCode = "200", description = "updated Client")
	@ApiResponse(responseCode = "400", description = "Invalid client supplied")
	@ApiResponse(responseCode = "404", description = "Client not found")
	@Nature({MINING})
	@Role({ADMIN})
	public ClientPojo update(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the client to update", required = true, example = "0") @PathVariable final EntityId clientId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The client with the updated values", required = true) 
			@RequestBody final ClientPojoPrototype client) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		clientId.matchOrApply(client.identityProvisional(), client::withId);
		clientService.update(client);
		return clientService.get(clientId, false);
	}
}
