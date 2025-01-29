/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.job.base.ModulesJob;
import innowake.mining.server.job.deadcode.IdentifyDeadCodeJob;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for triggering the candidate identification process.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class CandidateIdentificationController extends BaseController {
	
	/**
	 * URL pattern for identifying candidates.
	 */
	public static final String CANDIDATE_IDENTIFICATION_URL = "/v1/projects/{projectId}/identify-candidates";
	
	/**
	 * URL pattern for identifying dead code.
	 */
	public static final String DEAD_CODE_IDENTIFICATION_URL = "/v1/projects/{projectId}/identify-dead-codes";
	
	private final JobManager jobManager;

	/**
	 * Creates a new controller instance.
	 * 
	 * @param jobManager the {@link JobManager}
	 */
	@Autowired
	public CandidateIdentificationController(final JobManager jobManager) {
		this.jobManager = jobManager;
	}
	
	/**
	 * Starts a {@link IdentifyCandidatesJob} that identifies all candidates for the given modules. 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleMatcher list of id's and/or the file paths of the modules to identify the candidates for
	 * @return the Id of the {@link IdentifyCandidatesJob}
	 */
	@PostMapping(CANDIDATE_IDENTIFICATION_URL)
	@Operation(summary = "Starts a job that identifies all candidates (Annotations and Data Dictionary Entries) for the given modules and returns the job Id."
			+ " See the job status at '/v1/jobs/{jobId}/info'")
	@ApiResponse(responseCode = "202", description = "the job has successfully been submitted")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] identifyAllCandidates(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "list of id's and/or the file paths of the modules to identify the candidates "
					+ "for example = \"{1}, {src/file.cbl}\"", required = true) 
			@RequestBody final ModuleMatcher moduleMatcher) {
		final var job = new IdentifyCandidatesJob(projectId, moduleMatcher);
		return validateRequestAndSubmitJob(request, response, job);
	}
	
	/**
	 * Starts a {@link IdentifyDeadCodeJob} that identifies dead code for the given modules. 
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param moduleMatcher list of id's and/or the file paths of the modules to identify the candidates for
	 * @return the Id of the {@link IdentifyDeadCodeJob}
	 */
	@PostMapping(DEAD_CODE_IDENTIFICATION_URL)
	@Operation(summary = "Starts a job that identifies all dead code for the given modules and returns the job Id."
			+ " See the job status at '/v1/jobs/{jobId}/info'")
	@ApiResponse(responseCode = "202", description = "the job has successfully been submitted")
	@Nature({MINING})
	@Role({MANAGER})
	public char[] identifyDeadCode(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") 
			@PathVariable final Long projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "list of id's and/or the file paths of the modules to identify the candidates "
					+ "for example = \"{1}, {src/file.cbl}\"", required = true) 
			@RequestBody final ModuleMatcher moduleMatcher) {
		final var job = new IdentifyDeadCodeJob(EntityId.of(projectId), moduleMatcher);
		return validateRequestAndSubmitJob(request, response, job);
	}

	private char[] validateRequestAndSubmitJob(final HttpServletRequest request, final HttpServletResponse response, final ModulesJob job) {
		validate(request);
		response.setStatus(HttpStatus.ACCEPTED.value());
		final var jobMonitor = jobManager.submit(job);
		/* Explicitly returning the job Id as a char array, as a plain String would require a wrapper object for valid JSON. */
		return jobMonitor.getJobId().toCharArray();
	}
}
