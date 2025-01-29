/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.job.TaxonomyTypeDeletionJob;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * Controller for the {@linkplain TaxonomyTypePojo} requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class TaxonomyTypeController extends BaseController {

	/**
	 * URL pattern for a collection of {@linkplain TaxonomyTypePojo Taxonomy Types}.
	 */
	public static final String TAXONOMY_TYPE_COLLECTIONS_URL = "/v1/projects/{projectId}/taxonomy-types";
	
	/**
	 * URL pattern for {@linkplain TaxonomyTypePojo} by ID.
	 */
	public static final String TAXONOMY_TYPE_BY_NAME_URL = "/v1/projects/{projectId}/taxonomy-types/{taxonomyTypeName}";
	
	@Autowired
	private JobManager jobManager;

	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	/**
	 * Returns all available {@linkplain TaxonomyTypePojo taxonomy types} for a given project.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return a list of taxonomy types
	 */
	@GetMapping(value=TAXONOMY_TYPE_COLLECTIONS_URL)
	@Operation(summary = "Get all taxonomy types for one project", operationId = "findAllTaxonomyTypes")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public List<TaxonomyTypePojo> findAll(final HttpServletRequest request, final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return taxonomyService.findTypes(q -> q.ofProject(projectId).sortName(SortDirection.ASCENDING));
	}
	
	/**
	 * Creates a new {@linkplain TaxonomyTypePojo}.
	 *
	 * @param request access to the request.
	 * @param response access to the response.
	 * @param projectId the project ID of the {@linkplain TaxonomyTypePojo} to create.
	 * @param taxonomyType the {@linkplain TaxonomyTypePojo} to create .
	 * @return the new {@linkplain TaxonomyTypePojo}.
	 */
	@PostMapping(value=TAXONOMY_TYPE_COLLECTIONS_URL)
	@Operation(summary = "Create a new taxonomy type", operationId = "createTaxonomyType")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given taxonomy type is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.TAXONOMIES, reasonPhrase = "Applied Lock on Create taxonomy type")
	public TaxonomyTypePojo create(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The taxonomy type to create", required = true) 
			@RequestBody final TaxonomyTypePojoPrototype taxonomyType) {
		validate(request);
		if (taxonomyType.name.isNullable()) {
			throw new IllegalArgumentException("Taxonomy Type name must not be null.");
		} else if (Objects.requireNonNull(taxonomyType.name.get()).isEmpty()) {
			throw new IllegalArgumentException("Taxonomy Type name must not be empty.");
		}
		final UUID type = taxonomyService.createType(taxonomyType);
		final TaxonomyTypePojo newTaxonomyType = taxonomyService.getType(type);
		response.setStatus(HttpStatus.CREATED.value());
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		return newTaxonomyType;
	}
	
	/**
	 * Updates an existing {@linkplain TaxonomyTypePojo}.
	 *
	 * @param request access to the request.
	 * @param response access to the response.
	 * @param projectId the project ID of the {@linkplain TaxonomyTypePojo} to update.
	 * @param taxonomyType the updated {@linkplain TaxonomyTypePojo}.
	 * @return the updated {@linkplain TaxonomyTypePojo}.
	 */
	@PutMapping(value=TAXONOMY_TYPE_COLLECTIONS_URL)
	@Operation(summary = "Update a taxonomy type", operationId = "updateTaxonomyType")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given taxonomy type is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	@TryLock(lockCategory = ProjectLockCategory.TAXONOMIES, reasonPhrase = "Applied Lock on Update taxonomy type")
	public TaxonomyTypePojo update(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The updated taxonomy", required = true) 
			@RequestBody final TaxonomyTypePojoPrototype taxonomyType) {
		validate(request);

		if (! projectId.equals(taxonomyType.project.orElse(projectId))) {
			throw new ConstraintViolationException("Taxonomy Type project ID does not match the projectId parameter. Reassigning a type to another project is not supported.");
		}

		taxonomyService.updateType(taxonomyType);
		final TaxonomyTypePojo changedTaxonomyType = taxonomyService.getType(taxonomyType.id.getNonNull());
		response.setStatus(HttpStatus.OK.value());
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(projectId));
		return changedTaxonomyType;
	}
	
	/**
	 * Delete an existing {@linkplain TaxonomyTypePojo} along with the taxonomies of the type.
	 *
	 * @param request access to the request.
	 * @param response access to the response.
	 * @param projectId the project ID of the {@linkplain TaxonomyTypePojo} to delete.
	 * @param taxonomyTypeName the name of the {@linkplain TaxonomyTypePojo} to delete.
	 * @return the Id of the {@link TaxonomyTypeDeletionJob}
	 */
	@DeleteMapping(value=TAXONOMY_TYPE_BY_NAME_URL)
	@Operation(summary = "Starts a job that deletes a taxonomy type along with taxonomies for the given the taxonomy type name and returns the job Id."
			+ "See the job status at '/v1/jobs/{jobId}/info", operationId = "deleteTaxonomyType")
	@ApiResponse(responseCode = "200", description = "the job has successfully been submitted")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] delete(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId, 
			@Parameter(description = "the taxonomy type name", required = true, example = "name")
			@PathVariable final String taxonomyTypeName) {
		validate(request, "taxonomyTypeName");
		final TaxonomyTypeDeletionJob job = new TaxonomyTypeDeletionJob(projectId, taxonomyTypeName);
		return jobManager.submit(job).getJobId().toCharArray();
	}
}
