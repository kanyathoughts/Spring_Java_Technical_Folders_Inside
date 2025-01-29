/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.StatementFieldName;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.ndt.core.parsing.ast.model.Statement;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for {@link Statement} requests.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class StatementController extends BaseController {

	/**
	 * URL pattern for Statement aggregations by ID.
	 */
	public static final String AGGREGATIONS_URL = "/v1/projects/{projectId}/statements/aggregations";

	/**
	 * URL pattern for SqlStatement aggregations by ID.
	 */
	public static final String SQL_AGGREGATIONS_URL = "/v1/projects/{projectId}/statements/sql-aggregations";
	
	/**
	 * Returns aggregated values for statements based on {@code AggregationRequest}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param projectId the project id of statements to fetch aggregation for
	 * @param aggregationRequest object to hold request body
	 * @return a {@link List } containing {@link AggregationResult} object representing the aggregated result
	 */
	@PostMapping(AGGREGATIONS_URL)
	@Operation(summary = "Get aggregated values over a number of statements", operationId = "getStatementAggregatedValues")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given aggregation request is invalid.")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Nature({MINING, DISCOVERY})
	@Role({VIEWER})
	public List<AggregationResult<StatementFieldName>> getStatementAggregatedValues(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId,
			@io.swagger.v3.oas.annotations.parameters.RequestBody(description = "The aggregation request", required = true)  
			@RequestBody final AggregationRequest<StatementFieldName> aggregationRequest) {
		validate(request);
		validateAggregationRequest(aggregationRequest, StatementFieldName.PROJECT_ID, projectId);
		return moduleService.getStatementAggregations(projectId, aggregationRequest);
	}
}
