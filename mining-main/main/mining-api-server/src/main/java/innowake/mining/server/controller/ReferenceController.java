/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

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

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.ModulesModifiedEvent;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipFieldName;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for {@code module_relationship} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class ReferenceController extends BaseController {

	/**
	 * URL pattern for References for a Module.
	 */
	public static final String MODULE_REFERENCES_URL = "/v1/projects/{projectId}/modules/{moduleId}/references";
	
	/**
	 * URL pattern for References for a Module.
	 */
	public static final String MODULE_REFERENCES_URL_MULTIPLE_TYPES = "/v1/projects/{projectId}/modules/{moduleId}/references-multiple-types";

	/**
	 * URL pattern for References for a Project.
	 */
	public static final String PROJECT_REFERENCES_URL = "/v1/projects/{projectId}/references";

	/**
	 * URL pattern for References by ID.
	 */
	public static final String REFERENCE_BY_ID_URL = "/v1/projects/{projectId}/modules/{moduleId}/references/{referenceId}";

	/**
	 * URL pattern for aggregated values 
	 */
	public static final String AGGREGATIONS_URL = "/v1/projects/{projectId}/references/aggregations";
	
	public static final String MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL = "/v1/projects/{projectId}/references/{moduleId}/{referencedModuleId}";
	
	/**
	 * Request parameter for {@link #getAggregatedValues(HttpServletRequest, EntityId, AggregationRequest, Optional)}
	 * to limit the aggregations to a certain relationship type.
	 */
	public static final String REQUEST_PARAM_RELATIONSHIP = "relationship";

	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	/**
	 * Returns all available {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos} for a given module.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param direction the direction of the references
	 * @param distinct flag {@link Boolean} to filter duplicate dependencies 
	 * @param relationship the relationship of the module relationship
	 * @return a list of references
	 */
	@GetMapping(value=MODULE_REFERENCES_URL)
	@Operation(summary = "Get all references for one module", operationId = "findAllReferencesForModule")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given module or project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModuleRelationshipPojo> findAllforModule(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@Parameter(description = "the direction of the references", required = false)
			@RequestParam(required=false) final String direction,
			@Parameter(description = "the relationship of the references", required = false)
			@RequestParam(required=false) final String relationship,
			@Parameter(description = "filter duplicate dependencies", required = false, example = "false")
			@RequestParam(required = false, defaultValue = "false") final boolean distinct){
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findRelationship(q -> {
				q.ofProject(projectId).ofModuleInDirection(moduleId, getDirection(direction));
				if (distinct) {
					q.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION, RelationshipField.TYPE);
				}
				if (relationship != null) {
					q.withType(RelationshipType.from(relationship));
				} else {
					q.withTypes(RelationshipType.DEPENDENCY_TYPES);
				}
		});
	}

	private static RelationshipDirection getDirection(final String direction) {
		return direction == null ? RelationshipDirection.BOTH : RelationshipDirection.valueOf(direction);
	}

	/**
	 * Returns all available {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos} for a given module.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleId the ID of the module
	 * @param direction the direction of the references
	 * @param distinct flag {@link Boolean} to filter duplicate dependencies 
	 * @param relationships the possible relationships of the module relationship
	 * @return a list of references
	 */
	@GetMapping(value=MODULE_REFERENCES_URL_MULTIPLE_TYPES)
	@Operation(summary = "Get all references for one module", operationId = "findAllforModuleMultipleTypes")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given module or project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModuleRelationshipPojo> findAllforModuleMultipleTypes(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId,
			@Parameter(description = "the direction of the references", required = false)
			@RequestParam(required=false) final String direction,
			@Parameter(description = "the possible relationships of the references", required = false)
			@RequestParam(required=false) final List<String> relationships,
			@Parameter(description = "filter duplicate dependencies", required = false, example = "false")
			@RequestParam(required = false, defaultValue = "false") final boolean distinct){
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findRelationship(q -> {
				q.ofProject(projectId)
				 .ofModuleInDirection(moduleId, getDirection(direction))
				 .distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION, RelationshipField.TYPE);
				if (relationships != null) {
					q.withTypes(relationships.stream().map(RelationshipType::from).collect(Collectors.toList()));
				} else {
					q.withTypes(RelationshipType.DEPENDENCY_TYPES);
				}
		});
	}
	
	/**
	 * Returns all available {@linkplain ModuleRelationshipPojo ModuleRelationshipPojos}.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param direction the direction of the references
	 * @param relationship the relationship of the module relationship
	 * @return a list of references
	 */
	@GetMapping(value=PROJECT_REFERENCES_URL)
	@Operation(summary = "Get all references for a given project", operationId = "findAllReferencesForProject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModuleRelationshipPojo> findAll(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the direction of the references to list", required = false)
			@RequestParam(required=false) final String direction,
			@Parameter(description = "the relationship of the references to list", required = false)
			@RequestParam(required=false) final String relationship) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		/* we don't need a direction here like in orient */
		return moduleService.findRelationship(q -> {
			q.ofProject(projectId);
			if (relationship != null) {
				q.withType(RelationshipType.from(relationship));
			} else {
				q.withTypes(RelationshipType.DEPENDENCY_TYPES);
			}
		});
	}
	
	/**
	 * Finds a module relationship by ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param referenceId the ID of the reference
	 * @return a module relationship
	 */
	@GetMapping(value=REFERENCE_BY_ID_URL)
	@Operation(summary = "Find a reference by id", operationId = "findReferenceById")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project, module or reference does not exist")
	@Parameter(name = "moduleId", description = "the ID of the module to search", required = true, schema = @Schema(type = "integer", format="int64"), 
				in = ParameterIn.PATH, example = "0")
	@Nature({MINING})
	@Role({VIEWER})
	public ModuleRelationshipPojo findById(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the reference to find", required = true, example = "0")
			@PathVariable final UUID referenceId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());

		return moduleService.findAnyRelationship(q -> q.ofProject(projectId).byId(referenceId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Relationship not found for identifier: " + referenceId + " in project: " + projectId));
	}
	
	/**
	 * Creates a reference given a module ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param reference the reference to create
	 * @return the newly created module relationship
	 */
	@PostMapping(value=MODULE_REFERENCES_URL)
	@Operation(summary = "Create a new reference", operationId = "createReference")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given reference is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Parameter(name = "moduleId", description  = "the ID of the module", required = true, schema = @Schema(type = "integer", format="int64"), 
					in = ParameterIn.PATH, example = "0")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.REFERENCE, reasonPhrase = "Applied Lock on Create a new reference")
	public ModuleRelationshipPojo create(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The reference to create", required = true) 
			@RequestBody final ModuleRelationshipPojoPrototype reference) {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());

		final UUID id = moduleService.createRelationship(reference);
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(reference.srcModule.getNonNull())));
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(reference.dstModule.getNonNull())));
		return moduleService.findAnyRelationship(q -> q.byId(id))
				.orElseThrow(() -> new MiningEntityNotFoundException("Relationship not found for identifier: " + id + " in project: " + projectId));
	}

	/**
	 * Deletes the reference with the given ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param referenceId the ID of the reference
	 */
	@DeleteMapping(value=REFERENCE_BY_ID_URL)
	@Operation(summary = "Delete a reference", operationId = "deleteReference")
	@ApiResponse(responseCode = "204", description = "always, regardless of the existence of the reference")
	@Parameter(name = "moduleId", description  = "the ID of the module", required = true, schema = @Schema(type = "integer", format="int64"), 
				in = ParameterIn.PATH, example = "0")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.REFERENCE, reasonPhrase = "Applied Lock on Delete a reference")
	public void delete(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the ID of the reference to delete", required = true, example = "0")
			@PathVariable final UUID referenceId) {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		final var reference = moduleService.getRelationship(referenceId);
		moduleService.deleteRelationship(q -> q.byId(referenceId));
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(EntityId.of(reference.getSrcModule()))));
		eventPublisher.publishEvent(new ModulesModifiedEvent(projectId, Optional.of(EntityId.of(reference.getDstModule()))));
	}
	
	/**
	 * Endpoint to retrieve aggregated values for References
	 * 
	 * @param request Access to the request for validation
	 * @param projectId The ID of the Project. The outgoing side of the Edge must be in the project.
	 * @param aggregationRequest The aggregation of requested Reference fields
	 * @param relationship limit aggregation to relationships of certain type (optional but highly recommended, will speed up the query)
	 * @return a list of aggregated value for the requested Reference fields
	 */
	@PostMapping(AGGREGATIONS_URL)
	@Operation(summary = "Get aggregated values over a number of references", operationId = "getAggregatedValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<AggregationResult<RelationshipFieldName>> getAggregatedValues(final HttpServletRequest request,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true)
			@RequestBody final AggregationRequest<RelationshipFieldName> aggregationRequest,
			@Parameter(description = "limit aggregation to relationships of certain type (optional but highly recommended, will speed up the query)", required = false)
			@RequestParam(value = REQUEST_PARAM_RELATIONSHIP, required = false) 
			final Optional<RelationshipType> relationship) {
		
		validate(request);

		/* we filter on project by defining that the outgoing side of the relation must belong to our project */
		validateAggregationRequest(aggregationRequest, RelationshipFieldName.SRC_PROJECT_ID, projectId);
		return moduleService.getRelationshipAggregations(projectId, aggregationRequest);
	}
	
	/**
	 * Endpoint to retrieve references using the from and to module Id's.
	 *
	 * @param request Access to the request for validation
	 * @param response the HTTP response
	 * @param projectId the project ID
	 * @param moduleId the from module ID
	 * @param referencedModuleId the to module ID
	 * @param relationship the relationship between modules
	 * @return list of references
	 */
	@GetMapping(value = MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL)
	@Operation(summary = "Find all references by from and to module id's", operationId = "findAllByFromAndToModuleIds")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given project or module id's are not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or module does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<ModuleRelationshipPojo> findAllByFromAndToModuleIds(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "the from ID of the module", required = true, example = "0") @PathVariable final EntityId moduleId,
			@Parameter(description = "the to ID of the module", required = true, example = "0") @PathVariable final EntityId referencedModuleId,
			@Parameter(description = "relationship of the reference", required = true) 
			@RequestParam(value = REQUEST_PARAM_RELATIONSHIP, required = true) final RelationshipType relationship) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		
		return moduleService.findRelationship(q -> q.ofProject(projectId)
													.ofSource(moduleId)
													.ofDestination(referencedModuleId)
													.withType(relationship));
	}
}
