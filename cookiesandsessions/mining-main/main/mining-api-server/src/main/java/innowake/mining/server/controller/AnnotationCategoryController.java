/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Collections;
import java.util.List;
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
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.model.AnnotationCategory;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for the {@link AnnotationCategory} requests.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class AnnotationCategoryController extends BaseController {

	/**
	 * URL pattern for the annotation category collection.
	 */
	public static final String ANNOTATION_CATEGORY_COLLECTION_URL = "/v1/projects/{projectId}/annotation-categories";

	/**
	 * URL pattern for a single annotation category by ID.
	 */
	public static final String ANNOTATION_CATEGORY_BY_ID_URL = "/v1/projects/{projectId}/annotation-categories/{annotationCategoryId}";

	@Autowired
	private ApplicationEventPublisher eventPublisher;

	/**
	 * Lists all available {@linkplain AnnotationCategory annotation categories}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation category to find
	 * @return a list of {@linkplain AnnotationCategory annotation categories}
	 */
	@GetMapping(ANNOTATION_CATEGORY_COLLECTION_URL)
	@Operation(summary = "List all available annotation categories", operationId = "findAllAnnotationCategories")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Role({VIEWER})
	@Nature({MINING})
	public List<AnnotationCategory> findAll(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return annotationService.findCategories(q -> q.ofProjectWithDefault(projectId));
	}

	/**
	 * Finds a {@link AnnotationCategory} by an id.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation category to find
	 * @param annotationCategoryId the annotation category id of the annotation category to find
	 * @return a {@link AnnotationCategory}
	 */
	@GetMapping(ANNOTATION_CATEGORY_BY_ID_URL)
	@Operation(summary = "Get an annotation category by id", operationId = "findAnnotationCategoryById")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given annotation category or project ID does not exist")
	@Role({VIEWER})
	@Nature({MINING})
	public AnnotationCategory findById(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the annotation category ID to search for", required = true, example = "0")
			@PathVariable final Long annotationCategoryId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return annotationService.getCategory(projectId, annotationCategoryId);
	}

	/**
	 * Creates a new {@link AnnotationCategory}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation category to create
	 * @param annotationCategory the {@link AnnotationCategory} to create
	 * @return the new {@link AnnotationCategory}
	 */
	@PostMapping(ANNOTATION_CATEGORY_COLLECTION_URL)
	@Operation(summary = "Create a new annotation category", operationId = "createAnnotationCategory")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given annotation category is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Role({MANAGER})
	@Nature({MINING})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Create a new Annotation Category")
	public AnnotationCategory create(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The new annotation category", required = true) 
			@RequestBody final AnnotationCategory annotationCategory) {
		validate(request);
		projectId.matchOrApply(annotationCategory.getProjectId(), annotationCategory::setProjectId);
		response.setStatus(HttpStatus.CREATED.value());
		final long createdCategory = annotationService.createCategory(projectId, annotationCategory.getName(),
				Optional.ofNullable(annotationCategory.getTypes()).orElseGet(Collections::emptyList));
		eventPublisher.publishEvent(new AnnotationEvent(projectId));
		return annotationService.getCategory(projectId, createdCategory);
	}

	/**
	 * Updates an existing {@link AnnotationCategory}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation category to update
	 * @param annotationCategoryId the annotation category id of the annotation category to update
	 * @param annotationCategory the {@link AnnotationCategory} to update
	 * @return the updated {@link AnnotationCategory}
	 */
	@PutMapping(ANNOTATION_CATEGORY_BY_ID_URL)
	@Operation(summary = "Update an annotation category", operationId = "updateAnnotationCategory")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given annotation category is not valid")
	@ApiResponse(responseCode = "404", description = "if the given annotation category or project does not exist")
	@Role({MANAGER})
	@Nature({MINING})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Update an Annotation Category")
	public AnnotationCategory update(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the annotation category ID to update", required = true, example = "0")
			@PathVariable final Long annotationCategoryId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated annotation category", required = true) 
			@RequestBody final AnnotationCategory annotationCategory) {
		validate(request);
		projectId.matchOrApply(annotationCategory.getProjectId(), annotationCategory::setProjectId);
		response.setStatus(HttpStatus.OK.value());
		annotationService.updateCategory(projectId, annotationCategoryId,
				Optional.ofNullable(annotationCategory.getName()),
				Optional.ofNullable(annotationCategory.getTypes()));
		eventPublisher.publishEvent(new AnnotationEvent(projectId));
		return annotationService.getCategory(projectId, annotationCategoryId);
	}

	/**
	 * Delete an existing {@link AnnotationCategory}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation category to delete
	 * @param annotationCategoryId the annotation category id of the annotation category to delete
	 */
	@DeleteMapping(ANNOTATION_CATEGORY_BY_ID_URL)
	@Operation(summary = "delete an annotation category", operationId = "deleteAnnotationCategory")
	@ApiResponse(responseCode = "200", description = "if the given annotation category is deleted")
	@ApiResponse(responseCode = "400", description = "if the given annotation category is not valid")
	@ApiResponse(responseCode = "404", description = "if the given annotation category or project does not exist")
	@Role({MANAGER})
	@Nature({MINING})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Delete an Annotation Category")
	public void delete(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the annotation category ID to delete", required = true, example = "0")
			@PathVariable final Long annotationCategoryId) {
		validate(request);
		annotationService.deleteCategory(projectId, annotationCategoryId);
		eventPublisher.publishEvent(new AnnotationEvent(projectId));
	}
}
