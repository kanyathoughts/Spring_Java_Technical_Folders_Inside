/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Objects;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.AnnotationReport;
import innowake.mining.shared.model.AnnotationReportResponse;
import innowake.mining.shared.model.AnnotationReportSearchParameter;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for the {@link AnnotationReportSearchParameter} requests.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class AnnotationReportingController extends BaseController {

	/**
	 * URL pattern for the annotation reporting.
	 */
	public static final String ANNOTATION_REPORTING_URL = "/v1/projects/{projectId}/annotation-reporting";
	
	public static final int REPORT_LIMIT = 500;

	@Autowired
	UserNameUtil userNameUtil;

	/**
	 * Lists all available {@linkplain AnnotationReport annotation reports} for a project with optional filters.
	 * 
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param searchConditions {@link AnnotationReportSearchParameter} the optional filters
	 * @param projectId the ID of the project
	 * @param hasLimit if the limit has to be set on the result
	 * @return the list of {@link AnnotationReport annotation reports}
	 */
	@PostMapping(ANNOTATION_REPORTING_URL)
	@Operation(summary = "Get all available annotations for given search criteria", operationId = "getAnnotationReport")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING})
	@Role({VIEWER})
	public AnnotationReportResponse getAnnotationsForReporting(final HttpServletRequest request, final HttpServletResponse response,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "search conditions for annotation reporting", required = false)
			@RequestBody final AnnotationReportSearchParameter searchConditions,
			@Parameter(description = "the ID of the project to search", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "has limit for resultset", required = false) @RequestParam(required = false) final boolean hasLimit) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		final var annotationReports = annotationService.getReport(projectId, searchConditions, hasLimit ? REPORT_LIMIT : 0);
		/* Populates user names from user id's and sets it to the response */
		userNameUtil.fillUserNames(annotationReports.getContent());
		final Long totalElements = annotationReports.getTotalElements();
		final int numberOfRecords = totalElements == null ? annotationReports.getSize() : totalElements.intValue();
		return new AnnotationReportResponse(annotationReports.getContent(), numberOfRecords,
				annotationReports.getLastElement() != null && Objects.requireNonNull(annotationReports.getTotalElements())
					> Objects.requireNonNull(annotationReports.getLastElement()), annotationReports.getLimit());
	}
}
