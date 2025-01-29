/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.*;
import static innowake.mining.shared.security.RoleType.*;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for info requests.
 */
@MiningRestController
@RequestMapping(value="${routes.api}")
public class InfoController extends BaseController {
	
	@Autowired
	private BuildProperties buildProperties;
	
	/**
	 * URL pattern for the API info
	 */
	public static final String API_INFO_URL = "/v1/info";

	
	/**
	 * Returns the API version number and user Id.
	 * The information is returned in a map with key-value properties "api-version" and "userId".
	 * 
	 * @param request HTTP request
	 * @param response the HTTP response to use to set HTTP status and send errors with messages
	 * @return the map with the API version number and user Id
	 */
	@GetMapping(API_INFO_URL)
	@Operation(
			summary = "Common mining-api information", operationId = "getInfo",
			description = "The returned map contains the keys 'api-version' and 'userId' describing the api version and the user Id")
	@Nature(value = { DISCOVERY, DISCOVERY_LIGHT, MINING }, onAnyProject = true)
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Role(value = {VIEWER}, onAnyClient = true)
	public Map<String, String> info(final HttpServletRequest request, final HttpServletResponse response) {
		validate(request);
		response.setStatus(HttpStatus.OK.value());
		
		final Map<String, String> result = new HashMap<>();
		result.put("api-version", buildProperties.getVersion());
		result.put("userId", authentication.getUserId());
		
		return result;
	}
}
