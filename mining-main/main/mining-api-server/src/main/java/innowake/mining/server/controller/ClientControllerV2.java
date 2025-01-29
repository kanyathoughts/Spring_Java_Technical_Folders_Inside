/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.ADMIN;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import innowake.mining.server.config.security.ManualSecurityWithoutProjectAssociation;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.BinaryAttachmentPojo;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST Controller for {@link ClientPojo} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class ClientControllerV2 extends BaseController {

	/**
	 * URL pattern for the client collection.
	 */
	public static final String CLIENT_COLLECTION_URL = "/v2/clients";

	/**
	 * URL pattern for a single client by ID.
	 */
	public static final String CLIENT_BY_ID_URL = "/v2/clients/{clientId}";
	
	/**
	 * URL pattern for the logo of a client by ID.
	 */
	public static final String CLIENT_LOGO_URL = "/v2/clients/{clientId}/logo";
	
	private static final SortStringMapper<ClientService.ClientOrderBuilder> clientSortStringMapping = (q, field) -> {
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
	 * End-point to fetch all clients paginated based on the conditions specified.
	 *
	 * @param request Access to the request
	 * @param response The HTTP Response
	 * @param page The page number
	 * @param size The number of clients to return
	 * @param sortBy Array of sorting conditions
	 * @return A page of {@linkplain ClientPojo client}
	 */
	@GetMapping(CLIENT_COLLECTION_URL)
	@Operation(summary = "List all clients", operationId = "getAllClients")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature({MINING, DISCOVERY, DISCOVERY_LIGHT})
	@Role({VIEWER})
	@ManualSecurityWithoutProjectAssociation
	public Paged<ClientPojo> findAll(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the page number to be fetched",
							schema = @Schema(type="integer", format = "int32", defaultValue = "0"), required = false, example = "0")
			@RequestParam(value = "page", required = false, defaultValue = "0") final int page, 
			@Parameter(description = "the size of each page", example = "10",
							schema = @Schema(type="integer", format = "int32", defaultValue = "0", minimum = "1", maximum = "1073741823"),
							required = false)
			@RequestParam(value = "size", required = false, defaultValue = "0") int size,
			@Parameter(description = "the sort conditions as string array. example = new String[] {\"id;ASC\", \"name;ASC\"}", required = false)
			@RequestParam(value = "sortBy", required = false) final List<String> sortBy) {
		validate(request);
		return clientService.find(Pagination.at(page, size <= 0 ? 100 : size), q -> {
			if (user.isAdmin()) {
				q.withIdAbove(Long.valueOf(0L));
			} else {
				q.withIds(user.getClientIds());
			}
			q.withMarkedForDeletion(false);
			buildOrderFromString(q, sortBy, clientSortStringMapping);
		});
	}

	/**
	 * End-point to fetch the Client Logo as a Base64 Encoded String.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the client
	 * @return The {@link ResponseEntity Response} that contains the logo as a Base64 Encoded String if found, 404 Status Code otherwise
	 */
	@GetMapping(CLIENT_LOGO_URL)
	@Operation(summary = "Get Client Logo", operationId = "getLogo")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client or the logo does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role({VIEWER})
	public ResponseEntity<char[]> getLogo(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final EntityId clientId) {
		validate(request);
		return new ResponseEntity<>(clientService.getLogo(clientId).toDataURL().toCharArray(), HttpStatus.OK);
	}

	/**
	 * End-point to create the logo for the specified Client.
	 *
	 * @param request The HTTP Request
	 * @param response The HTTP Response
	 * @param clientId The ID of the client
	 * @param file The image to be stored
	 * @throws IOException if stream processing fails
	 */
	@PostMapping(value = CLIENT_LOGO_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Create Client Logo", operationId = "createLogo")
	@ApiResponse(responseCode = "201", description = "if the logo was saved successfully")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@Nature({MINING})
	@Role({ADMIN})
	public void createLogo(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final EntityId clientId,
			@Parameter(description = "The logo file", required = true, example = "0")
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The logo file", required = true)
			@RequestParam final MultipartFile file) throws IOException {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());
		updateLogo(clientId, file);
	}

	/**
	 * End-point to delete the logo for the specified Client.
	 *
	 * @param request access to the request
	 * @param response The HTTP Response
	 * @param clientId The ID of the Client
	 */
	@DeleteMapping(CLIENT_LOGO_URL)
	@Operation(summary = "Delete Client Logo", operationId = "deleteLogo")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given client or the logo does not exist")
	@Nature({MINING})
	@Role({ADMIN})
	public void deleteLogo(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final EntityId clientId) {
		validate(request);
		clientService.setLogo(clientId, null);
	}

	/**
	 * Endpoint to update the logo for the specified Client.
	 *
	 * @param request The HTTP Request
	 * @param response The HTTP Response
	 * @param clientId The ID of the client
	 * @param file The logo file
	 * @throws IOException if stream processing fails
	 */
	@PutMapping(value = CLIENT_LOGO_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Update Client Logo", operationId = "updateLogo")
	@ApiResponse(responseCode = "204", description = "if logo was updated successfully")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@Nature({MINING})
	@Role({ADMIN})
	public void updateLogo(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final EntityId clientId,
			@Parameter(description = "The logo file", required = true, example = "0")
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The logo file", required = true)
			@RequestParam final MultipartFile file) throws IOException {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		updateLogo(clientId, file);
	}
	
	private void updateLogo(final EntityId clientId, final MultipartFile file) throws IOException {
		final String mimeType = file.getContentType();
		if (mimeType == null || ! mimeType.startsWith("image/")) {
			throw new IllegalArgumentException("Uploaded file must be an image");
		}
		final BinaryAttachmentPojo logo = new BinaryAttachmentPojo(mimeType, file.getBytes());
		clientService.setLogo(clientId, logo);
	}

	/**
	 * Endpoint to delete a {@link ClientPojo Client} in a cascading manner, along with its associated Keycloak roles.
	 * The endpoint also deletes the entities that are associated with the given {@link ClientPojo Client}.
	 *
	 * @param request The HTTP Request
	 * @param clientId The ID of the Client
	 */
	@DeleteMapping(CLIENT_BY_ID_URL)
	@Operation(summary = "Delete specified Client in full cascading manner", operationId = "deleteClient")
	@ApiResponse(responseCode = "200", description = "if the given client was deleted successfully")
	@ApiResponse(responseCode = "404", description = "if the given client does not exist")
	@Nature({MINING})
	@Role({ADMIN})
	public void deleteClient(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the client to search", required = true, example = "0")
			@PathVariable final EntityId clientId) {
		validate(request);
		clientService.markForDeletion(clientId);
	}

	/**
	 * Endpoint to create a {@link ClientPojo Client} and its associated Keycloak roles.
	 *
	 * @param request The HTTP Request
	 * @param response The HTTP Response
	 * @param client The {@link ClientPojo Client} to be saved
	 * @return The new {@link ClientPojo Client}
	 */
	@PostMapping(CLIENT_COLLECTION_URL)
	@Operation(summary = "Create a new client and it's associated Keycloak roles", operationId = "createClientV2")
	@ApiResponse(responseCode = "201", description = "Client and it's roles were created successfully")
	@ApiResponse(responseCode = "400", description = "Invalid client supplied")
	@Nature({MINING})
	@Role({ADMIN})
	public ClientPojo create(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The client to create", required = true) 
			@RequestBody final ClientPojoPrototype client) {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());
		return clientService.create(client);
	}

}
