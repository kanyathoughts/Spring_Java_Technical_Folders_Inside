/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;

import java.util.List;
import java.util.Map;

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

import com.innowake.innovationlab.commons.model.LegacyDatabase;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.job.DbSchemaImporterJob;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SchemaInfoPojo;
import innowake.mining.shared.security.RoleType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for managing database schema information.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class DataSchemaController extends BaseController {

	/**
	 * URL pattern to find all schemas for the specified Project.
	 */
	public static final String SCHEMAS_PER_PROJECT = "/v1/projects/{projectId}/schema";
	
	/**
	 * URL pattern to import database schemas for the specified Project.
	 */
	public static final String SCHEMA_IMPORT_URL = "/v1/projects/{projectId}/schema/import";
	
	/**
	 * URL pattern to retrieve field definitions for a Module.
	 */
	public static final String SCHEMA_FIELDS_URL = "/v1/projects/{projectId}/modules/{moduleId}/fields";

	public static final String UPDATE_SCHEMA_FIELD_URL = "/v1/projects/{projectId}/modules/{moduleId}/fields/{ordinal}";
	
	private final FieldInfoService fieldInfoService;
	private final JobManager jobManager;

	/**
	 * Creates a new controller instance.
	 * 
	 * @param jobManager the {@link JobManager} to use
	 * @param fieldInfoService the {@link FieldInfoService} to use
	 */
	public DataSchemaController(@Autowired final JobManager jobManager, @Autowired final FieldInfoService fieldInfoService) {
		this.jobManager = jobManager;
		this.fieldInfoService = fieldInfoService;
	}
	
	/**
	 * Find database schemas in a Project.
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return List of schemas with definition statistics.
	 */
	@GetMapping(value = SCHEMAS_PER_PROJECT)
	@Operation(summary = "Find database schema definitions", operationId = "findSchemas")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "If the given project does not exist")
	@Role({RoleType.MANAGER})
	@Nature({MINING})
	public List<SchemaInfoPojo> findSchemas(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId) {
		validate(request);
		return moduleService.findSchemaInfos(projectId);
	}
	
	/**
	 * Import database schema definitions.
	 *
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param dbData the database definition data
	 * @return the submitted job ID
	 */
	@PostMapping(value = SCHEMA_IMPORT_URL)
	@Operation(summary = "Import database schema definitions", operationId = "importSchemas")
	@ApiResponse(responseCode = "202", description = "The job has successfully been submitted")
	@ApiResponse(responseCode = "404", description = "If the given project does not exist")
	@Role({RoleType.MANAGER})
	@Nature({MINING})
	public char[] importSchemas(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The schema data to import", required = true)
			@RequestBody final LegacyDatabase dbData) {
		validate(request);
		final DbSchemaImporterJob job = new DbSchemaImporterJob(projectId, dbData);
		/* have to send back a JSON string value or otherwise the the JSON parsing in the client fails */
		return jobManager.submit(job).getJobId().toCharArray();
	}
	
	/**
	 * Retrieve field definitions for a Module.
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId the ID of the project the Module belongs to.
	 * @param moduleId the ID of the Module
	 * @return List of field definitions.
	 */
	@GetMapping(value = SCHEMA_FIELDS_URL)
	@Operation(summary = "Find database schema definitions", operationId = "findFieldInfos")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "If the given project does not exist")
	@Role({RoleType.VIEWER})
	@Nature({MINING})
	public List<FieldInfoPojo> findFieldInfos(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable
			final EntityId moduleId) {
		validate(request);
		return fieldInfoService.find(q -> q.ofProject(projectId)
											.ofModule(moduleId));
	}
	
	/**
	 * Updates the comment of an existing {@Link FieldInfo} based on moduleId and ordinal.
	 * 
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId the ID of the Project the Module belongs to
	 * @param moduleId the ID of the {@link Module}
	 * @param ordinal the index of the data field or table column
	 * @param properties the map containing the properties
	 */
	@PutMapping(value = UPDATE_SCHEMA_FIELD_URL)
	@Operation(summary = "Update the comment for FieldInfo based on moduleId and ordinal", operationId = "updateFieldInfoComment")
	@ApiResponse(responseCode = "404", description = "If the given project or moduleId or ordinal does not exist")
	@ApiResponse(responseCode = "200", description = "If the comment is updated successfully")
	@Role({RoleType.EDITOR})
	@Nature({MINING})
	@TryLock(lockCategory = ProjectLockCategory.DATA_SCHEMA, reasonPhrase = "Applied Lock on Update Field Info Comment")
	public void updateFieldInfo(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable
			final EntityId moduleId,
			@Parameter(description = "the index of the data field or table column", required = true, example = "0")
			@PathVariable
			final int ordinal,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The map containing the properties", required = true)
			@RequestBody final Map<String, Object> properties) {
		validate(request, "ordinal");
		if (properties.containsKey("comment")) {
			if (moduleService.countModules(q -> q.ofProject(projectId)
												 .byId(moduleId)) == 0) {
				throw new MiningEntityNotFoundException(ModulePojo.class, "No Module " + moduleId + " in Project " + projectId);
			}

			final var matches = fieldInfoService.find(q -> q.ofProject(projectId).ofModule(moduleId).withOrdinal(ordinal));
			switch (matches.size()) {
				case 0:
					throw new MiningEntityNotFoundException(FieldInfoPojo.class, "FieldInfo does not exists for ordinal: " + ordinal + ", module: " + moduleId + " and project: " + projectId);
				case 1:
					final var comment = properties.get("comment");
					fieldInfoService.update(new FieldInfoPojoPrototype()
												.setId(matches.get(0).getId())
												.setComment(comment == null ? null : comment.toString()));
					response.setStatus(HttpStatus.OK.value());
					break;
				default:
					throw new IllegalArgumentException("Found multiple FieldInfo candidates for ordinal: " + ordinal + ", module: " + moduleId + " and project: " + projectId);
			}
		}
	}

}
