package innowake.mining.server.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for {@link HotSpot} requests.
 */
@MiningRestController
@RequestMapping(value = "${routes.api}", produces = MediaType.APPLICATION_JSON_VALUE)
public class HotSpotController extends BaseController {

	/**
	 * URL pattern for collection of Hotspots.
	 */
	public static final String HOTSPOT_COLLECTION_URL = "/v1/projects/{projectId}/hotspots/{hotspotType}";

	/**
	 * URL pattern for a limited collection of Hotspots.
	 */
	public static final String HOTSPOT_LIMITED_COLLECTION_URL = HOTSPOT_COLLECTION_URL + "/{limit}";

	/**
	 * Returns a list of {@link HotSpot} default 10 for a given project ID and hotspot type.
	 *
	 * @param request access to the request
	 * @param projectId the ID of the project
	 * @param hotspotType the type of the hotspot request 
	 * @param limit the number of records to be fetched default being 10
	 * @return a list of {@link HotSpot}
	 */
	@Operation(summary = "List all Hotspots for the given projectid and hotspot type", operationId = "getHotspots")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@GetMapping(path = {
			HOTSPOT_COLLECTION_URL,
			HOTSPOT_LIMITED_COLLECTION_URL
	})
	@Nature({MINING})
	@Role({VIEWER})
	public List<HotSpot> getHotSpots(
			final HttpServletRequest request,
			@Parameter(description = "the ID of the project to list hotspots from", required = true, example = "0") @PathVariable final EntityId projectId,
			@Parameter(description = "type of hotspot", required = true, example = "CALLS") @PathVariable final String hotspotType,
			@Parameter(description = "the number of hotspots to be returned default being 10", required = false, example = "0") 
			@PathVariable(required = false) final Integer limit) {
		validate(request, "hotspotType", "limit");
		return moduleService.findHotSpots(projectId, FilterType.valueOf(hotspotType), limit);
	}
}
