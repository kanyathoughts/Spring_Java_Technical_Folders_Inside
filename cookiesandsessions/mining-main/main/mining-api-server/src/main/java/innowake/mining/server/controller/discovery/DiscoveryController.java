/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.discovery;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;
import static innowake.mining.shared.security.RoleType.EDITOR;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.BufferedReader;

import java.net.URL;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.shared.discovery.config.ConfigResources;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import innowake.mining.server.util.DiscoveryConfigSynchUtil;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.io.discovery.config.DiscoveryConfigurationImportService;
import innowake.mining.data.model.discovery.dna.ModelBelongsToClusters;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.server.Logging;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.controller.BaseController;
import innowake.mining.server.controller.MiningRestController;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.dna.FindCommunitiesJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.service.DnaModelService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * REST controller for Discovery.
 */
@MiningRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class DiscoveryController extends BaseController {

	/**
	 * URL pattern for Discovery configuration.
	 */
	public static final String DISCOVERY_CONFIGURATION_URL = "/v1/projects/{projectId}/discovery/config";

	/**
	 * URL pattern for Discovery DNA.
	 */
	public static final String DISCOVER_DNA_URL = "/v1/projects/{projectId}/discovery/find-communities";

	/**
	 * URL pattern for Discovery Metrics.
	 */
	public static final String DISCOVER_METRICS_URL = "/v1/projects/{projectId}/discovery/discover-metrics";

	/**
	 * URL pattern for Discovery Code.
	 */
	public static final String DISCOVER_CODE_URL = "/v1/projects/{projectId}/discovery/discover-code";

	/**
	 * URL pattern for list of DNA Snapshots.
	 */
	private static final String LIST_OF_DNA_SNAPSHOT_URL = "/v1/projects/{projectId}/discovery/dna-snapshots";

	/**
	 * URL pattern for DNA snapshot for selected timestamp.
	 */
	private static final String DNA_SNAPSHOT_FOR_SELECTED_TIMESTAMP_URL = "/v1/projects/{projectId}/discovery/dna-snapshots/{timestamp}";

	/**
	 * URL pattern for DNA snapshot for latest timestamp.
	 */
	public static final String DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL = "/v1/projects/{projectId}/discovery/dna-snapshots/latest";

	/**
	 * URL pattern for Belongs to DNA cluster.
	 */
	public static final String BELONGS_TO_CLUSTER_URL = "/v1/projects/{projectId}/discovery/dna-clusters/modules/{moduleId}";
	
	/**
	 * URL pattern to update the title and description of DNA Community.
	 */
	public static final String DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL = "/v1/projects/{projectId}/discovery/dna-community/{uuid}";

	private static final String SEARCH_ORDER = "discovery-search-order.xml";

	private static final String SYNC_CF_REPO_WITH_PD_UTILITIES = "/v1/projects/discovery/sync-utilities";

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);

	@Autowired
	private DnaModelService dnaModelService;
	@Autowired
	private JobManager jobManager;

	@Nullable
	@Value("${configuration.external-utility-repo-url}")
	private String cfRepoLink;

	@Autowired
	private DiscoveryConfigurationImportService discoveryConfigurationImportService;


	/**
	 * Return an xml of the updated PD Utility.xml as a File
	 *
	 * @return an xml as a String of the updated utilities.xml
	 */
	@GetMapping(value = SYNC_CF_REPO_WITH_PD_UTILITIES)
	@Operation(summary = "Sync CF Repo with PD Repo", operationId = "syncing utilities")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "error syncing utilities")
	@Role({VIEWER})
	@Nature({DISCOVERY})
	public ResponseEntity<byte[]> synchronizeUtilities() {
		final HttpHeaders headers = new HttpHeaders();
		headers.add("Content-Disposition", "attachment; filename=utilities.xml");
		try {
			final BufferedReader pdRepoReader = DiscoveryConfigSynchUtil.getUtilitiesReaderLocal();
			final BufferedReader remoteRepoReader = DiscoveryConfigSynchUtil.getUtilitiesReaderRemote(new URL(cfRepoLink));
			return ResponseEntity.ok()
					.headers(headers)
					.body(UtilityList.serializeUtilityList(DiscoveryConfigSynchUtil.readAndParseUtilities(pdRepoReader, remoteRepoReader)).getBytes());
		} catch (final Exception e) {
			final String message = String.format("Error while accessing multipart file %s: %s", cfRepoLink, e.getMessage());
			LOG.error(message, e);
			return ResponseEntity.badRequest().body(message.getBytes());
		}
	}

	/**
	 * Submits a Discovery Code job for the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return the ID of the submitted job
	 * @throws IOException in case of an error
	 */
	@PostMapping(DISCOVER_CODE_URL)
	@Operation(summary = "Executes a Discovery Code job for the given project ID.", operationId = "discoverCode")
	@ApiResponse(responseCode = "202", description = "if the request was successfully committed")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public char[] discoverCode(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws IOException {
		validate(request);

		response.setStatus(HttpStatus.ACCEPTED.value());
		return jobManager.submit(new DiscoverCodeJob(projectId)).getJobId().toCharArray();
	}

	/**
	 * Submits a Discovery Metrics job for the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param incremental {@code true} for incremental discovery or {@code false} to enforce full discovery. Default is {@code true}
	 * @return the ID of the submitted job
	 * @throws IOException in case of an error
	 */
	@PostMapping(DISCOVER_METRICS_URL)
	@Operation(summary = "Executes a Discovery Metrics job for the given project ID.", operationId = "discoverMetrics")
	@ApiResponse(responseCode = "202", description = "if the request was successfully committed")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public char[] discoverMetrics(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "whether to do an incremental or full discover metrics scan", required = false, example = "true")
			@RequestParam(required = false, defaultValue = "true") final boolean incremental) throws IOException {
		validate(request);

		final DiscoverMetricsJob job = new DiscoverMetricsJob(projectId, incremental);

		response.setStatus(HttpStatus.ACCEPTED.value());
		return jobManager.submit(job).getJobId().toCharArray();
	}

	/**
	 * Submits a Discovery DNA job for the given project ID. Calls cache-evict on project-DNA-cache.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return the ID of the submitted job
	 * @throws IOException in case of an error
	 */
	@PostMapping(value = DISCOVER_DNA_URL, produces = MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Executes a Discovery DNA job for the given project ID.", operationId = "discoverDNA")
	@ApiResponse(responseCode = "202", description = "if the request was successfully committed")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public char[] discoverDna(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws IOException {
		validate(request);

		response.setStatus(HttpStatus.ACCEPTED.value());
		return jobManager.submit(new FindCommunitiesJob(projectId)).getJobId().toCharArray();
	}

	/**
	 * Return configuration and {@link SearchOrder} for the given Project id.
	 *
	 * @param request access to the request
	 * @param response The HTTP response.
	 * @param projectId The ID of the project.
	 * @return The ID of the submitted job.
	 * @throws Exception In case of an error.
	 */
	@GetMapping(value = DISCOVERY_CONFIGURATION_URL, produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
	@Operation(summary = "Downloads Discovery configuration.", operationId = "downloadDiscoveryConfiguration")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({VIEWER})
	@Nature({DISCOVERY, MINING})
	public @Nullable ResponseEntity<Resource> downloadConfig(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws Exception {
		validate(request);
		try {
			final Resource resource = new ByteArrayResource(getConfigurations(projectId));
			return ResponseEntity.ok()
					.header(HttpHeaders.CONTENT_DISPOSITION)
					.body(resource);
		} catch (final Exception e) {
			final String message = String.format("Error occured while downloading configurations for the project %s : %s", projectId, e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), message);
			return null;
		}
	}

	/**
	 * Imports configuration and {@link SearchOrder} for the given Project id.
	 *
	 * @param request access to the request
	 * @param response The HTTP response.
	 * @param projectId The ID of the project.
	 * @param configZipFile The zip file to be imported.
	 * @throws Exception In case of an error.
	 */
	@PostMapping(value = DISCOVERY_CONFIGURATION_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Uploads Discovery configuration.", operationId = "uploadDiscoveryConfiguration")
	@ApiResponse(responseCode = "204", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public void uploadConfiguration(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@RequestBody(description = "The Zip file to import", required = true)
			@RequestParam(name = "file", required = true)
			final MultipartFile configZipFile) throws Exception {
		validate(request);
		@Nullable
		final String filename = configZipFile.getOriginalFilename();
		final String fileId = filename != null ? filename : String.format("<unavailable file ID [%d]>", Long.valueOf(System.currentTimeMillis()));

		try {
			discoveryConfigurationImportService.importConfigurations(projectId, fileId, configZipFile.getInputStream());
		} catch (final IOException e) {
			final String message = String.format("Error while accessing multipart file %s: %s", fileId, e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.BAD_REQUEST.value(), message);
		}
	}

	/**
	 * Returns the list of DNA snapshots for the given Project ID.
	 *
	 * @param request access to the request
	 * @param response The HTTP response
	 * @param projectId The ID of the project
	 * @return Lists the timestamps of DNA snapshots
	 * @throws IOException In case of an error
	 */
	@GetMapping(value = LIST_OF_DNA_SNAPSHOT_URL, produces = MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Lists the timestamps of the Discovery DNA snapshots", operationId = "list the timestamps of DNA snapshots")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Role({VIEWER})
	@Nature({DISCOVERY})
	public  ResponseEntity<List<String>> fetchListOfDnaSnapshots(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws IOException {
		validate(request);
		return ResponseEntity.ok().body(dnaModelService.getListOfDnaSnapshots(projectId));
	}

	/**
	 * Return  {@link ModelDna } for the given Project id and the given timestamp.
	 *
	 * @param request access to the request
	 * @param response The HTTP response
	 * @param projectId The ID of the project
	 * @param timestamp The timestamp of the DNA snapshot
	 * @return ModelDna of the project
	 * @throws IOException In case of an error
	 */
	@GetMapping(value = DNA_SNAPSHOT_FOR_SELECTED_TIMESTAMP_URL, produces = MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Fetch Model DNA For a selected DNA snapshot's timestamp.", operationId = "model DNA for selected timestamp")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project/DNA snapshot does not exist")
	@Role({VIEWER})
	@Nature({DISCOVERY})
	public  ResponseEntity<ModelDna> fetchModelDnaForSelectedDnaSnapshotTimestamp(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the timestamp of a DNA snapshot", required = true)
	        @PathVariable final String timestamp) throws IOException {
		validate(request, "timestamp");
		final Optional<ModelDna> optionalModelDna = dnaModelService.getDnaForSelectedTimestamp(projectId, timestamp);
		return optionalModelDna.isPresent() ? ResponseEntity.ok().body(optionalModelDna.get()) : ResponseEntity.notFound().build();
	}





	/**
	 * Return latest {@link ModelDna } for the given Project id. Results get cached.
	 *
	 * @param request access to the request
	 * @param response The HTTP response
	 * @param projectId The ID of the project
	 * @return ModelDna of the project
	 * @throws IOException In case of an error
	 */
	@GetMapping(value = DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL, produces = MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Fetch Model DNA for latest discovery DNA snapshot", operationId = "model DNA for latest timestamp")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project/DNA snapshot does not exist")
	@Role({VIEWER})
	@Nature({DISCOVERY})
	public  ResponseEntity<ModelDna> fetchLatestDnaSnapshot(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws IOException {
		validate(request);
		final Optional<ModelDna> optionalModelDna = dnaModelService.getLatestDna(projectId);
		return optionalModelDna.isPresent() ? ResponseEntity.ok().body(optionalModelDna.get()) : ResponseEntity.notFound().build();
	}

	/**
	 * Return the {@link ModelBelongsToClusters} for the given {@code module} id.
	 * It returns based on the latest DNA Snapshot of the given {@code project} id.
	 *
	 * @param request access to the request
	 * @param response The HTTP response
	 * @param projectId the id of Project
	 * @param moduleId the id of {@link Module}
	 * @return ModelBelongsToClusters for the given project and moduleId
	 * @throws IOException In case of an error
	 */
	@GetMapping(value = BELONGS_TO_CLUSTER_URL)
	@Operation(summary = "Returns the list of clusters for the given module", operationId = "Belongs to cluster")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project/module/DNA snapshot does not exist or if the given module is not present in any cluster")
	@Role({VIEWER})
	@Nature({DISCOVERY})
	public  ResponseEntity<ModelBelongsToClusters> fetchBelongsToCluster(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the module", required = true, example = "0")
			@PathVariable final EntityId moduleId) throws IOException {
		validate(request);
		final Optional<ModelBelongsToClusters> modelBelongsToClusters = dnaModelService.getModelBelongsToCluster(projectId, moduleId);
		return modelBelongsToClusters.isPresent() ? ResponseEntity.ok().body(modelBelongsToClusters.get()) : ResponseEntity.notFound().build();
	}

	/**
	 * Requests to update the title and description {@link DnaCommunityPojo}.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the {@link ProjectPojo}
	 * @param uuid the ID of the {@link DnaCommunityPojo}
	 * @param title the title of the {@link DnaCommunityPojo}
	 * @param description of the {@link DnaCommunityPojo}
	 * 
	 * @return UUID of updated DNA Community
	 */
	@PutMapping(value = DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL)
	@Operation(summary = "Update the Title and Description for DnaCommunity based on UUID", method= "updateDnaCommunity")
	@ApiResponse(responseCode = "404", description = "If the given UUID and projectId does not exist")
	@ApiResponse(responseCode = "200", description = "If the title and description is updated successfully")
	@Role({EDITOR})
	@Nature({DISCOVERY, MINING})
	public UUID updateDnaCommunity(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true)
			@PathVariable final EntityId projectId,
			@Parameter(description = "the UUID of Dna Community", required = true)
			@PathVariable final String uuid,
			@Parameter(description = "the title of Dna Community", required = true)
			@RequestParam final String title,
			@Parameter(description = "the description of Dna Community", required = false)
			@RequestParam final Optional<String> description) {
		validate(request, "uuid");
		return dnaModelService.updateDnaCommunityTitleDescription(uuid, title, description);
	}

	private byte[] getConfigurations(final EntityId projectId) throws Exception {
		final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
		final ProjectPojo project = projectService.get(projectId);
		final Map<String, String> configurations = projectService.getXmlConfigs(projectId, Arrays.stream(ConfigResources.values())
				.map(ConfigResources::getResourceName).toList());
		final SearchOrders searchOrders = new SearchOrders(project.getSearchOrders());
		configurations.put(SEARCH_ORDER, searchOrders.serializeSearchOrders());

		try (final ZipOutputStream zipOut = new ZipOutputStream(byteOut)) {
			configurations.entrySet().stream().forEach(configuration -> {
				final String fileName = configuration.getKey();
				final String fileContent = configuration.getValue();

				final ZipEntry zipEntry = new ZipEntry(fileName);
				try {
					zipOut.putNextEntry(zipEntry);
					zipOut.write(fileContent.getBytes(StandardCharsets.UTF_8));
				} catch (final IOException e) {
					final String message = String.format("Error while accessing file %s: %s", fileName, e.getMessage());
					LOG.error(message, e);
					throw new IllegalStateException(message, e);
				}
			});
		}
		return byteOut.toByteArray();
	}
}
