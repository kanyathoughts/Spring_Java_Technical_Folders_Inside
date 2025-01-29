/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import innowake.mining.shared.extensions.CustomTableExtension;
import innowake.mining.shared.extensions.CustomTableExtensionDescription;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.extensions.MiningWebUiExtension;
import innowake.mining.shared.extensions.WebUiExtensionDescription;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller to query registered extensions for the mining-ui
 */
@MiningRestController
@RequestMapping(value="${routes.api}")
public class MiningUiExtensionsController extends BaseController {

	public static final String EXTENSION_LIST_URL = "/v1/ui/extensions";
	public static final String TABLE_EXTENSION_LIST_URL = "/v1/ui/table-extensions";

	@Autowired(required = false)
	private List<MiningWebUiExtension> webUiExtensions = Collections.emptyList();

	@Autowired(required = false)
	private List<CustomTableExtension> tableExtensions = Collections.emptyList();

	/**
	 * Returns all available web ui extensions.
	 *
	 * @param request access to the request
	 * @return list of WebUiExtensionDescription
	 */
	@GetMapping(value=EXTENSION_LIST_URL, produces=MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Gets a list of registered web ui extensions", operationId = "getWebUiExtensions")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature(value = {MINING}, onAnyProject = true)
	@Role(value = {VIEWER}, onAnyClient = true)
	public List<WebUiExtensionDescription> getWebUiExtensions(final HttpServletRequest request) {
		validate(request);
		return webUiExtensions.stream()
				.map(WebUiExtensionDescription::fromWebUiExtension)
				.collect(Collectors.toList());    
	}

	/**
	 * Returns all available custom table extensions.
	 *
	 * @param request access to the request
	 * @return list of CustomTableExtensionDescription
	 */
	@GetMapping(value=TABLE_EXTENSION_LIST_URL, produces=MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Gets a list of registered custom table extensions", operationId = "getTableExtensions")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@Nature(value = {MINING}, onAnyProject = true)
	@Role(value = {VIEWER}, onAnyClient = true)
	public List<CustomTableExtensionDescription> getTableExtensions(final HttpServletRequest request) {
		validate(request);

		return tableExtensions.stream()
				.map(CustomTableExtensionDescription::fromCustomTableExtension)
				.collect(Collectors.toList());
	}
}
