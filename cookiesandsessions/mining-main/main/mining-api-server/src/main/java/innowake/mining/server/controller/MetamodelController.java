/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.*;
import static innowake.mining.shared.security.RoleType.*;

import java.util.Collection;
import java.util.Optional;

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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.model.CustomPropertyMetadata;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for metamodel requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class MetamodelController extends BaseController {
	
	private static final String URI_VAR_ENTITY_NAME = "entityName";
	private static final String URI_VAR_PROPERTY_NAME = "propertyName";
	
	/**
	 * URL pattern for class metadata
	 */
	public static final String CLASS_METADATA_URL = "/v1/metamodel/{metaModelClass}";
	
	/**
	 * URL pattern for class metadata of the given property
	 */
	public static final String PROPERTY_METADATA_URL = "/v1/metamodel/{metaModelClass}/{propertyName}";
	
	/**
	 * URL pattern for refreshing a project's metamodel.
	 */
	public static final String PEFRESH_METADATA_URL = "/v1/refresh/metamodel";
	
	/**
	 * URL pattern for defining or removing properties of a project's default metamodel.
	 */
	public static final String DEFINE_METADATA_DEFAULT_PROPERTY_URL = "/v1/projects/{projectId}/metamodel/{entityName}/{propertyName}";
	
	private final ApplicationEventPublisher eventPublisher;
	
	public MetamodelController(@Autowired final ApplicationEventPublisher eventPublisher) {
		super();
		this.eventPublisher = eventPublisher;
	}
	
	/**
	 * Returns all available custom property metadata for a given class. 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param metaModelClass the name of the class for which the information should be returned
	 * @return a list of {@linkplain CustomPropertyMetadata metadata information}
	 */
	@GetMapping(value = CLASS_METADATA_URL)
	@Operation(summary = "List all custom property metadata information for a given entity class",
				operationId = "findMetaModel")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given class does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role(value = {VIEWER}, onAnyClient = true)
	public Collection<CustomPropertyMetadata> findAll(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the name of the class for which information should be returned", required = true)
			@PathVariable final String metaModelClass) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return customPropertiesService.findPropertyDefinitions(q -> q.withParent(metaModelClass));
	}
	
	/**
	 * Finds a particular {@link CustomPropertyMetadata metadata information} given a property name 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param metaModelClass the name of the class
	 * @param propertyName the name of the custom property
	 * @return the {@link CustomPropertyMetadata metadata information} for the custom property
	 */
	@GetMapping(value = PROPERTY_METADATA_URL)
	@Operation(summary = "Find particular metadata information for a custom property",
				operationId = "findCustomPropertyMetadataByName")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given class or custom property does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role(value = {VIEWER}, onAnyClient = true)
	public CustomPropertyMetadata findByPropertyName(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the name of the class for which information should be returned", required = true)
			@PathVariable final String metaModelClass, 
			@Parameter(description = "the name of the custom property for which information should be returned", required = true)
			@PathVariable final String propertyName) {
		validate(request, URI_VAR_PROPERTY_NAME);
		response.setStatus(HttpStatus.OK.value());
		return customPropertiesService.getPropertyDefinition(metaModelClass, propertyName);
	}
	
	@PostMapping(value = PEFRESH_METADATA_URL)
	@Operation(summary = "Refresh the custom property metadata for a certain project or in general",
				operationId = "refreshCustomPropertyMetadata")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role(value = {MANAGER}, onAnyClient = true)
	public void refreshCustomPropertyMetadata(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the ID of the project to refresh", required = false)
			@RequestParam final Optional<EntityId> projectId) {
		validate(request);
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(projectId));
		response.setStatus(HttpStatus.OK.value());
	}
	
	@PostMapping(value = DEFINE_METADATA_DEFAULT_PROPERTY_URL)
	@Operation(summary = "Define (create or modify) a custom property on the project specific default class for an entity",
				operationId = "defineCustomProperty")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role(value = {MANAGER}, onAnyClient = true)
	@TryLock(lockCategory = ProjectLockCategory.METAMODEL, reasonPhrase = "Applied Lock on defining Custom property")
	public void defineCustomProperty(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the name of the mining entity for which the property shall be defined", required = true)
			@PathVariable final String entityName, 
			@Parameter(description = "the name of the custom property to be defined, if the name in the metadata differs it will be renamed", required = true)
			@PathVariable final String propertyName,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "CustomPropertyMetadata", required = true) 
			@RequestBody final CustomPropertyMetadata def) {
		validate(request, URI_VAR_ENTITY_NAME, URI_VAR_PROPERTY_NAME);
		customPropertiesService.defineProperty(projectId, entityName, propertyName, def);
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
		response.setStatus(HttpStatus.OK.value());
	}

	@DeleteMapping(value = DEFINE_METADATA_DEFAULT_PROPERTY_URL)
	@Operation(summary = "Delete a custom property on the project specific default class for an entity",
				operationId = "deleteCustomProperty")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role(value = {MANAGER}, onAnyClient = true)
	@TryLock(lockCategory = ProjectLockCategory.METAMODEL, reasonPhrase = "Applied Lock on deleting Custom property")
	public void deleteCustomProperty(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the name of the mining entity for which the property shall be defined", required = true)
			@PathVariable final String entityName, 
			@Parameter(description = "the name of the custom property to be defined", required = true)
			@PathVariable final String propertyName) {
		validate(request, URI_VAR_ENTITY_NAME, URI_VAR_PROPERTY_NAME);
		customPropertiesService.deleteProperty(projectId, entityName, propertyName);
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(projectId)));
		response.setStatus(HttpStatus.OK.value());
	}

}
