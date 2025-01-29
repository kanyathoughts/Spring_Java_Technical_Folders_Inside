/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.scheduler.SchedulerEntryResolver;
import innowake.mining.server.scheduler.SchedulerImportJobProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * Controller for importing scheduler data
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class SchedulerImporterController extends BaseController {

	/**
	 * The path to create a scheduler import
	 */
	public static final String CREATE_SCHEDULER_IMPORT = "/v1/projects/{projectId}/scheduler-import";

	/**
	 * The path to get the supported importers
	 */
	public static final String GET_SUPPORTED_IMPORTERS = "/v1/projects/{projectId}/scheduler-import/importers";

	private final List<SchedulerEntryResolver> schedulerEntryResolvers;
	private final JobManager jobManager;

	@Autowired
	public SchedulerImporterController(final List<SchedulerEntryResolver> schedulerEntryResolvers, final JobManager jobManager) {
		this.schedulerEntryResolvers = schedulerEntryResolvers;
		this.jobManager = jobManager;
	}

	/**
	 * Create a new scheduler import job
	 *
	 * @param request the {@linkplain HttpServletRequest}
	 * @param response the {@linkplain HttpServletResponse}
	 * @param projectId the project ID
	 * @param prototype the request params for the import
	 * @return the job ID
	 * @throws IOException if the file cannot be read
	 */
	@PostMapping(CREATE_SCHEDULER_IMPORT)
	@Operation(summary = "Create a new scheduler import job", operationId = "createSchedulerImport")
	@ApiResponse(responseCode = "201", description = "on success")
	@ApiResponse(responseCode = "400", description = "if the given prototype is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] create(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The request params for the import", required = true)
			@RequestBody final SchedulerImportPojoPrototype prototype) throws IOException {
		validate(request);
		response.setStatus(HttpStatus.CREATED.value());
		prototype.setProject(projectId);
		final var job = new SchedulerImportJobProvider(prototype).getJob(prototype.schedulerType.getNonNull());
		return jobManager.submit(job).getJobId().toCharArray();
	}

	/**
	 * Get the supported importers
	 *
	 * @param request the {@linkplain HttpServletRequest}
	 * @param projectId the project ID
	 * @return the supported importers
	 */
	@GetMapping(GET_SUPPORTED_IMPORTERS)
	@Operation(summary = "Get the supported importers", operationId = "getSupportedImporters")
	@ApiResponse(responseCode = "200", description = "on success")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public Map<SchedulerType, List<String>> getSupportedImporters(
			final HttpServletRequest request,
			@Parameter(description = "the project ID", required = true, example = "0")
			@PathVariable final EntityId projectId) {
		validate(request);
		return schedulerEntryResolvers.stream().collect(Collectors.groupingBy(SchedulerEntryResolver::resolverSchedulerType,
				Collectors.mapping(SchedulerEntryResolver::resolverSchedulerIdentifier, Collectors.toList())));
	}
}
