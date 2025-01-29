/*
 * Copyright (c) 2022 Deloitte. All rights reserved..
 */
package innowake.mining.server.controller;

import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import innowake.mining.shared.datapoints.LabelMappings;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for label mapping requests.
 */
@MiningUnsecuredRestController
@RequestMapping(value="${routes.api}")
public class LabelMappingsController extends BaseController {
	
	/**
	 * URL pattern for retrieving label mappings.
	 */
	public static final String API_SERVER_LABEL_MAPPING_URL = "/v1/label-mappings";
	
	/**
	 * Fetch all label mappings.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @return the set of label mappings
	 */
	@GetMapping(API_SERVER_LABEL_MAPPING_URL)
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@MiningUnsecuredRestEndpoint
	@Operation(
			summary = "Get label mappings", operationId = "getLabelMapping",
			description = "The returned map contains the key 'labelMapping' and its mapped labels")
	public Map<LabelMappings.LabelType, Map<String, String>> labelMapping(
			final HttpServletRequest request, 
			final HttpServletResponse response) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		return LabelMappings.getLabelMappings();
	}
}
