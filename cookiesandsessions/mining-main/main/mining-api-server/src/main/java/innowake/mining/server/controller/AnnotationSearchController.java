/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.EDITOR;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.server.event.AnnotationUpdatedEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.fasterxml.jackson.core.JsonProcessingException;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationSearch;
import innowake.mining.shared.model.WorkingState;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for the Annotation requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class AnnotationSearchController extends BaseController {

	/**
	 * URL pattern for the annotation search.
	 */
	public static final String ANNOTATION_SEARCH_URL = "/v1/projects/{projectId}/annotation-search";

	/**
	 * URL pattern for a single annotation by ID.
	 */
	public static final String ANNOTATION_BY_ID_URL = "/v1/projects/{projectId}/annotation-search/{annotationId}";

	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	/**
	 * Searches annotations given a project with optional filters.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to find
	 * @param states JSON string representation of the states for the returned annotations, if {@code null} or empty, annotations for all states are returned.
	 * @param categoryIds JSON string representation of the ids of the categories for the returned annotations, if {@code null} annotations for all categories 
	 * are returned.
	 * @param modulename only annotations with corresponding modules that match this wildcard pattern of a module name are returned, if {@code null} 
	 * annotations for all modules are returned.
	 * @param modulepath only annotations with corresponding modules that equals the path are returned, if {@code null} no path restrictions apply.
	 * @param size the upper bound of the resulting elements 
	 * @return a list of annotations
	 * @throws JsonProcessingException  in case of an invalid parameter
	 */
	@GetMapping(ANNOTATION_SEARCH_URL)
	@Operation(summary = "Find all Annotations for the Annotation Search View", operationId = "findAllForAnnotationSearchView")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public AnnotationSearch findAllForAnnotationSearchView(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "JSON representation of the states for the returned annotations, omitting returns annotations for all states,"
					+ " e.g. [CANDIDATE, IN_ANALYSIS, APPROVED]", required = false)
			@RequestParam(required=false) final String states,
			@Parameter(description = "JSON representation of the category IDs for the returned annotations, omitting returns annotations for all categories "
					+ "e.g. [1, 5, 30]", required = false)
			@RequestParam(required=false) final String categoryIds,
			@Parameter(description = "restrict returned annotations to those having modules, where the module name matches this wildcard pattern, "
					+ "omitting returns annotations for all modules, e.g. mod?ule*01", required = false)
			@RequestParam(required=false) final String modulename,
			@Parameter(description = "restrict returned annotations to those having modules, where the module path equals the given path", required = false)
			@RequestParam(required=false) final String modulepath,
			@Parameter(description = "the size of the resulting list, if not specified the result size is unbounded", required = false, example = "10") 
			@RequestParam(required = false, defaultValue = "0") final int size) throws JsonProcessingException {
		validate(request);
		final List<WorkingState> statesCollection = states != null ? Arrays.asList(PojoMapper.jsonReaderFor(WorkingState[].class).readValue(states)) : null;
		final List<Long> categoryIdsCollection = categoryIds != null ? Arrays.asList(PojoMapper.jsonReaderFor(Long[].class).readValue(categoryIds)) : null;
		final Paged<AnnotationPojo> page = annotationService.find(new Pagination(0, size), q -> {
				q.ofProject(projectId);
				if (statesCollection != null) {
					q.withStates(statesCollection);
				}
				if (categoryIdsCollection != null) {
					q.withCategories(categoryIdsCollection);
				}
				if (modulename != null) {
					q.withModuleName(modulename, true);
				}
				if (modulepath != null) {
					q.withModulePath(modulepath);
				}
		});
		return new AnnotationSearch(page.getContent(), Optional.ofNullable(page.getTotalElements()).orElse(0L));
	}
	
	/**
	 * Updates the state of an existing Annotation.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of the annotation to update
	 * @param annotationId the annotation id of the annotation to update
	 * @param annotation the Annotation to update 
	 */
	@PutMapping(ANNOTATION_BY_ID_URL)
	@Operation(summary = "Update the state of an annotation", operationId = "updateAnnotationState")
	@ApiResponse(responseCode = "204", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given annotation is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project or annotation ID does not exist")
	@Nature({MINING})
	@Role({EDITOR})
	@TryLock(lockCategory = ProjectLockCategory.ANNOTATIONS, reasonPhrase = "Applied Lock on Update the State of an Annotation")
	public void update(
			final HttpServletRequest request, 
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the annotation ID of the annotation to update", required = true, example = "0")
			@PathVariable final EntityId annotationId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "the annotation for which the state will be updated", required = true) 
			@RequestBody final AnnotationPojoPrototype annotation) {
		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		annotationId.matchOrApply(annotation.identityProvisional(), annotation::withId);
		validateProject(projectId, annotation);
		annotationService.update(new AnnotationPojoPrototype()
				.withId(annotation.identityProvisional())
				.setState(annotation.state.getNonNull()));
		eventPublisher.publishEvent(new AnnotationUpdatedEvent(projectId, annotationId));
	}
	
}
