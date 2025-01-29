/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.functionalblocks.service.AnnotationToFunctionalBlockService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.AnnotationType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for request related to Annotation to Functional Block Linkage
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class AnnotationToFunctionalBlockController extends BaseController {
	
	@Autowired
	AnnotationToFunctionalBlockService annotationToFunctionalBlockService;
	
	@Autowired
	private FunctionalBlockService functionalBlockService;
	/**
	 * URL pattern for getting the UUIDs of the functional units that represent the given Annotations
	 */
	public static final String CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION = "/v1/projects/{projectId}/annotations-to-functional-blocks/functional-units";
	
	/**
	 * URL pattern for getting FunctionalBlock (Functional Group) for List of Annotations
	 */
	public static final String FUNCTIONAL_BLOCKS_FROM_ANNOTATION = "/v1/projects/{projectId}/annotations-to-functional-blocks/functional-groups";
	
	/**
	 * URL pattern for a fetching the functional block names by annotation Id.
	 */
	public static final String FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID = "/v1/projects/{projectId}/annotations-to-functional-blocks/{annotationId}";
	
	/**
	 * URL pattern for the delete no parent 'FUNCTIONAL'type functional-units.
	 */
	public static final String DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS = "/v1/projects/{projectId}/type/{type}/no-parent-functional-units";

	/**
	 * Return the UUIDs of the functional units that represent the given Annotations. It invoking the block generation to ensure that 
	 * the functional units were created and are up-to-date.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to create
	 * @param annotationIds List of annotationIDs
	 * @return the Map of EntityID and UUID 
	 */
	@PostMapping(CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION)
	@Operation(summary = "Get the functional units which represent the given Annotations", operationId = "getFunctionalUnitsForAnnotations")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Map<Long, UUID> getFunctionalUnitsForAnnotations(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "List of annotations", required = true) 
			@RequestBody final List<EntityId> annotationIds) {
		validate(request);
		return annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(projectId, annotationIds);
	}
	
	/**
	 * Returns the functional groups (list of functional blocks {@link FunctionalBlockPojo}) that contain the given Annotations.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to create
	 * @param annotationIds List of annotationIDs
	 * @return the Map of EntityID and List of {@link FunctionalBlockPojo}.
	 */
	@PostMapping(FUNCTIONAL_BLOCKS_FROM_ANNOTATION)
	@Operation(summary = "Returns the functional groups which contain the given Annotations", operationId = "getFunctionalGroupsForAnnotations")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Map<Long, List<FunctionalBlockPojo>> getFunctionalGroupOfAnnotations(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "List of annotations", required = true) 
			@RequestBody final List<EntityId> annotationIds) {
		validate(request);
		return annotationToFunctionalBlockService.getFunctionalGroupsForAnnotations(projectId, annotationIds);
	}
	
	/**
	 * Returns the functional block names by an annotation id.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation
	 * @param annotationId the id of the annotation
	 * @return a set of functional block names
	 */
	@GetMapping(FUNCTIONAL_BLOCK_NAMES_BY_ANNOTATION_ID)
	@Operation(summary = "Returns the Functional Block Names By Annotation Id", operationId = "getFunctionalBlockNamesByAnnotationId")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given annotation or project ID does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Set<String> getFunctionalBlockNamesByAnnotationId(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final Long projectId, 
			@Parameter(description = "the annotation ID to fetch the functional blocks", required = true, example = "0")
			@PathVariable final Long annotationId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return annotationToFunctionalBlockService.getFunctionalBlockNamesByAnnotationId(annotationId);
	}
	
	/**
	 * Delete an existing {@link FunctionalBlockPojo} and corresponding annotation by type which has no parent.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the functional block to delete
	 * @param type the type of the annotation to delete
	 */
	@DeleteMapping(DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS)
	@Operation(summary = " Delete an existing functional unit and corresponding annotation which has no parent",
	operationId = "deleteEmptyAutoGeneratedFunctionalUnits")
	@ApiResponse(responseCode = "204", description = "if the functional block was successfully deleted")
	@ApiResponse(responseCode = "404", description = "if the project or functional block does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	public void deleteEmptyAutoGeneratedFunctionalUnits(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the type of the annotation to be deleted", required = true, example = "0")
			@PathVariable final AnnotationType type) {
		validate(request, "type");
		response.setStatus(HttpStatus.NO_CONTENT.value());
		
		final List<UUID> emptyFunctionalUnits = functionalBlockService.findUids(q -> {
			q.ofProject(projectId)
			.withType(FunctionalBlockType.FUNCTIONAL_UNIT)
			.withAnnotationType(type)
			.notWithParent(p -> p.withType(FunctionalBlockType.FUNCTIONAL_GROUP));
		});
		if ( ! emptyFunctionalUnits.isEmpty()) {
			annotationService.delete(query -> query.ofProject(projectId).withFunctionalBlocks(emptyFunctionalUnits)
						.withType(AnnotationType.FUNCTIONAL));
			functionalBlockService.delete(emptyFunctionalUnits);
		}
	}

}
