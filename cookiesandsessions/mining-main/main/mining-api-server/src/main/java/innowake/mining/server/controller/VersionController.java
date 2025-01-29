/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller for version requests.
 */
@MiningUnsecuredRestController
@RequestMapping(value="${routes.api}")
public class VersionController extends BaseController {
	
	@Autowired
	private BuildProperties buildProperties;
	
	/**
	 * URL pattern for the API server version
	 */
	public static final String API_SERVER_VERSION_URL = "/v1/version";
	
	/**
	 * Returns the API server version number.
	 * <p>
	 * The information is returned in a map with the key "version".
	 * 
	 * @param request HTTP request
	 * @param response the HTTP response to use to set HTTP status and send errors with messages
	 * @return the map with the API server version
	 */
	@CrossOrigin
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@GetMapping(API_SERVER_VERSION_URL)
	@MiningUnsecuredRestEndpoint
	@Operation(
			summary = "API server version", operationId = "getVersion",
			description = "The returned map contains the key 'version' describing the api server version."
			)
	public Map<String, String> version(final HttpServletRequest request, final HttpServletResponse response) {
		validate(request);
		
		final Map<String, String> result = new HashMap<>();
		result.put("version", buildProperties.getVersion());
		
		return result;
	}
}
