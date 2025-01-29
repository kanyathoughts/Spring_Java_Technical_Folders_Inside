/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.DISCOVERY_LIGHT;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.ADMIN;
import static innowake.mining.shared.security.RoleType.VIEWER;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.model.Feature;
import innowake.mining.shared.model.FeatureId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for FF4J.
 */
@MiningRestController
@RequestMapping(value="${routes.api}")
public class FeatureController extends BaseController {
	
	protected static final String URI_VAR_FEATURE_ID = "featureId";

	/**
	 * URL pattern for a Feature by ID.
	 */
	public static final String FEATURE_BY_ID = "/v1/features/{featureId}";

	/**
	 * URL pattern to toggle Feature by ID.
	 */
	public static final String TOGGLE_FEATURE_BY_ID = "/v1/features/{featureId}/toggle";

	/**
	 * Parameter name for setting the state of a feature.
	 */
	public static final String STATE = "state";

	@Autowired
	private FF4j ff4j;
	
	@Override
	protected void validateUriVars(UriVarsValidation validation) {
		validation.pass(URI_VAR_FEATURE_ID);
	}

	/**
	 * Finds a {@link Feature} by an ID or {@code null} if not found.
	 * Features can be maintained with FF4J feature UI {@code <api-server-url>/feature-console/}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param featureId the feature ID of the feature to find
	 * @return the found {@link Feature} or {@code null}
	 */
	@GetMapping(path = FEATURE_BY_ID, produces = MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Find a feature by its ID. Features can be maintained with FF4J feature UI {@code <api-server-url>/feature-console/}.",
				operationId = "findFeatureById")
	@ApiResponse(responseCode = "200", description = "if operation was successful")
	@ApiResponse(responseCode = "404", description = "if the given feature does not exist")
	@Nature(value = {MINING, DISCOVERY, DISCOVERY_LIGHT}, onAnyProject = true)
	@Role(value = {VIEWER}, onAnyClient = true)
	public Feature findById(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the feature to search", required = true, example = "myFeature")
			@PathVariable final String featureId) {
		
		validate(request);
		if (ff4j.exist(featureId)) {
			response.setStatus(OK.value());
			final org.ff4j.core.Feature feature = ff4j.getFeature(featureId);
			return new Feature(FeatureId.fromId(featureId), feature.getDescription(), feature.isEnable());
		}
		response.setStatus(NOT_FOUND.value());
		return null;
	}
	
	/**
	 * Toggles a {@link Feature} with the given {@code featureId} to the given {@code state}.
	 * Features can be maintained with FF4J feature UI {@code <api-server-url>/feature-console/}.
	 *
	 * @param request access to the request
	 * @param response access to the response
	 * @param featureId the feature ID of the feature to toggle
	 * @param state the new state of the feature
	 */
	@PostMapping(value=TOGGLE_FEATURE_BY_ID, params=STATE)
	@Operation(summary = "Enables or disables a feature. Features can be maintained with FF4J feature UI {@code <api-server-url>/feature-console/}.",
				operationId = "toggleFeature")
	@ApiResponse(responseCode = "204", description = "if operation was successful")
	@ApiResponse(responseCode = "404", description = "if the given feature does not exist")
	@Nature({MINING})
	@Role({ADMIN})
	public void toggle(
			final HttpServletRequest request,
			final HttpServletResponse response, 
			@Parameter(description = "the ID of the feature to search", required = true, example = "myFeature")
			@PathVariable final String featureId,
			@Parameter(description = "the new state of the feature", required = true, example = "true")
			@RequestParam(name=STATE) final boolean state) {
		
		validate(request);
		if (ff4j.exist(featureId)) {
			if (state) {
				ff4j.enable(featureId);
			} else {
				ff4j.disable(featureId);
			}
			response.setStatus(NO_CONTENT.value());
		} else {
			response.setStatus(NOT_FOUND.value());
		}
	}
}
