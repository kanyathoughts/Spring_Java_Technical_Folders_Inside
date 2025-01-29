/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.io.DiscoveryCsvExportService;
import innowake.mining.data.io.DiscoveryExcelExportService;
import innowake.mining.data.io.DiscoveryExportOptions;
import innowake.mining.data.io.ExcelImportService;
import innowake.mining.data.io.sourceobject.SourceObjectExportService;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.server.Logging;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.importer.csv.CSVImportService;
import innowake.mining.server.importer.sources.SourceObjectImportJob;
import innowake.mining.shared.access.EffortSummaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.extensions.MiningExportExtension.ExportException;
import innowake.mining.shared.extensions.MiningExportExtension.ExportValue;
import innowake.mining.shared.io.ExportFormatDescription;
import innowake.mining.shared.io.ExportFormatDescription.ExtensionType;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.io.UploadDescription;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import innowake.mining.shared.service.UserRoleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.Explode;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.MANAGER;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * REST controller for importing and exporting Mining data.
 */
@MiningRestController
@RequestMapping(value="${routes.api}")
public class IoController extends BaseController {
	
	private static final String URI_VAR_EXPORT_FORMAT = "exportFormat";

	/**
	 * URL pattern for Project as CSV.
	 */
	public static final String PROJECT_AS_CSV_URL = "/v1/projects/{projectId}/csv";

	/**
	 * URL pattern for Project as EXCEL.
	 */
	public static final String PROJECT_AS_EXCEL_URL = "/v1/projects/{projectId}/excel";

	/**
	 * URL pattern for Project as FILE.
	 */
	public static final String PROJECT_AS_FILE_URL = "/v1/projects/{projectId}/file";

	/**
	 * URL pattern for Source Object collections for Project.
	 */
	public static final String SOURCE_OBJECTS_URL = "/v1/projects/{projectId}/source-objects";

	/**
	 * URL pattern for Export Format collections.
	 */
	public static final String EXPORT_FORMAT_COLLECTIONS_URL = "/v1/projects/{projectId}/export-formats";

	/**
	 * URL pattern for Export Format by type.
	 */
	public static final String EXPORT_FORMAT_BY_TYPE_URL = "/v1/projects/{projectId}/export/{exportFormat}";

	/**
	 * URL pattern for Effort Summary for a Project.
	 */
	public static final String EFFORT_SUMMARY_URL = "/v1/projects/{projectId}/effort-summary-excel";

	/**
	 * Parameter name for importing the file.
	 */
	public static final String FILE = "file";

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONTROLLER);

	@Autowired
	private CSVImportService csvImportService;
	@Autowired
	private ExcelImportService excelImportService;
	@Autowired
	private DiscoveryExcelExportService excelExportService;
	@Autowired
	private DiscoveryCsvExportService csvExportService;
	@Autowired
	private SourceObjectExportService sourceCodeExportService;
	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private EffortSummaryService effortSummaryService;
	
	@Autowired(required = false)
	private List<MiningJobExtension<?>> jobExtensions = Collections.emptyList();
	
	@Autowired(required = false)
	private List<MiningExportExtension> exportExtensions = Collections.emptyList();
	
	@Autowired
	private JobManager jobManager;

	@Autowired
	protected JobConfigurationProperties jobConfigurationProperties;

	/**
	 * Imports a CSV file into the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param file the CSV file to import
	 * @throws IOException in case of an error
	 */
	@PostMapping(value=PROJECT_AS_CSV_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Imports a CSV file into a Mining project")
	@ApiResponse(responseCode = "204", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({MINING})
	public void importCSV(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@RequestBody(description = "the CSV file to import", required = true)
			@RequestParam(name = FILE, required = true)
	        final MultipartFile file) throws IOException {

		validate(request);

		@Nullable final String filename = file.getOriginalFilename();
		final String fileId = filename != null ? filename : String.format("<unavailable file ID [%d]>", Long.valueOf(System.currentTimeMillis()));
		try (final InputStream inputStream = file.getInputStream()) {
			csvImportService.importCsv(projectId, fileId, inputStream);
			response.setStatus(HttpStatus.NO_CONTENT.value());
		} catch (final IOException e) {
			final String message = String.format("Error while parsing file: %s", e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.BAD_REQUEST.value(), message);
		}
	}

	/**
	 * Imports a Discovery Excel file into the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param file the Excel file to import
	 * @throws IOException in case of an error
	 */
	@PostMapping(value=PROJECT_AS_EXCEL_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Imports an Excel file into a Mining project")
	@ApiResponse(responseCode = "204", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")	
	@Role({MANAGER})
	@Nature({MINING})
	public void importExcel(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@RequestBody(description = "the Excel file to import", required = true)
			@RequestParam(name = FILE, required = true)
			final MultipartFile file) throws IOException {

		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());

		@Nullable final String filename = file.getOriginalFilename();
		final String fileId = filename != null ? filename : String.format("<unavailable file ID [%d]>", Long.valueOf(System.currentTimeMillis()));

		try (final InputStream inputStream = file.getInputStream()) {
			excelImportService.importExcel(projectId, fileId, inputStream);
		} catch (final IOException e) {
			final String message = String.format("Error while parsing file %s: %s", fileId, e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.BAD_REQUEST.value(), message);
		}
	}

	/**
	 * Exports a Discovery Excel workbook from the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 *  whether the export should be made in a format that is compatible with legacy discovery-ui
	 * @return response entity with a resource body
	 * @throws IOException in case of an error
	 */
	@GetMapping(value=PROJECT_AS_EXCEL_URL, produces=MediaType.APPLICATION_OCTET_STREAM_VALUE)
	@Operation(summary = "Exports an Excel file from a Mining project")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "500", description = "if an internal error occurs")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public void exportExcel(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws IOException {

		validate(request);
		response.setStatus(HttpStatus.OK.value());	

		try {
			final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
			final String workbookId = String.format("discovery_%d_%s.xlsx", projectService.getNid(projectId), timestamp);
			response.setStatus(HttpStatus.OK.value());
			response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
			response.setHeader(HttpHeaders.CONTENT_DISPOSITION, String.format("attachment; filename=%s", workbookId));
			excelExportService.exportExcel(projectId, response.getOutputStream(), new DiscoveryExportOptions());
		} catch (final IOException e) {
			final String message = String.format("Error while exporting project %s: %s", projectId, e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), message);
		}
	}

	/**
	 * Exports a Discovery CSV file from the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return response entity with a resource body
	 * @throws IOException in case of an error
	 */
	@GetMapping(value=PROJECT_AS_CSV_URL, produces=MediaType.APPLICATION_OCTET_STREAM_VALUE)
	@Operation(summary = "Exports a CSV file from a Mining project", description = "exportCsv")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "500", description = "if an internal error occurs")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public void exportCsv(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable
			final EntityId projectId) throws IOException {

		validate(request);
		response.setStatus(HttpStatus.OK.value());

		try {
			final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
			final String fileId = String.format("discovery_%d_%s.csv", projectService.getNid(projectId), timestamp);
			response.setStatus(HttpStatus.OK.value());
			response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
			response.setHeader(HttpHeaders.CONTENT_DISPOSITION, String.format("attachment; filename=%s", fileId));
			csvExportService.exportCsv(projectId, response.getOutputStream(), new DiscoveryExportOptions());
		} catch (final IOException e) {
			final String message = String.format("Error while exporting project %s: %s", projectId, e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), message);
		}
	}

	/**
	 * Imports source objects from the entries of the given Zip file into the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param file the Zip file to import
	 * @return {@link MiningFileIndex}
	 * @throws IOException in case of an error
	 */
	@PostMapping(value=SOURCE_OBJECTS_URL, produces=MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Imports source objects from the entries of a Zip file into a Mining project")
	@ApiResponse(responseCode = "204", description = "if the request was successful")
	@ApiResponse(responseCode = "400", description = "if the given file is not valid")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "500", description = "if the source objects are not imported successfully")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public char[] importSourceObjects(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@RequestBody(description = "The Zip file to import", required = true)
			@RequestParam(name = FILE, required = true)
			final MultipartFile file) throws IOException {

		validate(request);

		try {
			final SourceObjectImportJob importJob = new SourceObjectImportJob(projectId, file.getInputStream(), jobConfigurationProperties);
			response.setStatus(HttpStatus.ACCEPTED.value());
			/* Executed locally as we have to write the source object zip into the file system of the local machine */
			return jobManager.submitLocal(importJob).getJobId().toCharArray();
		} catch (final IOException exception) {
			LOG.error("SourcePojo import failed due to I/O exception", exception);
			response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), "SourcePojo import failed due to I/O exception");
			return new char[0];
		}
	}

	/**
	 * Writes source content and {@link MiningFileIndex} for the given Project ID into {@link ServletOutputStream} of {@link HttpServletResponse}.
	 *
	 * @param request access to the request
	 * @param response The HTTP response.
	 * @param projectId The ID of the project.
	 * @param baseRevision The baseRevision, its an optional parameter.
	 * @throws IOException In case of an error.
	 */
	@GetMapping(value=SOURCE_OBJECTS_URL)
	@Operation(summary = "Downloads Source Objects", operationId = "downloadSourceObject")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({VIEWER})
	@Nature({DISCOVERY, MINING})
	public void downloadSourceObjects(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "Base revision (Optional)", required = false, example = "0")
			@RequestParam(required=false) final Long baseRevision) throws IOException {
		validate(request);
		try {
			response.setContentType(MediaType.APPLICATION_OCTET_STREAM_VALUE);
			sourceCodeExportService.exportSourceObjects(projectId, baseRevision, response.getOutputStream());
		} catch (final Exception e) {
			final String message = String.format("Error occured while downloading source objects for the project %s : %s", projectId.toString(), e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), message);
		}
	}

	/**
	 * Returns a list of supported export formats.
	 * The formats in the list can be passed to {@link #exportToFormat(HttpServletRequest, HttpServletResponse, EntityId, String, MultiValueMap)}.
	 *
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId The ID of the project.
	 * @return a list of supported export formats
	 * @throws IOException when sending the response fails
	 */
	@GetMapping(value=EXPORT_FORMAT_COLLECTIONS_URL, produces=MediaType.APPLICATION_JSON_VALUE)
	@Operation(summary = "Gets a list of supported export formats", operationId = "getExportFormats")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Role(value = {VIEWER}, onAnyClient = true)
	@Nature(value = {MINING, DISCOVERY}, onAnyProject = true) 
	public @Nullable List<ExportFormatDescription> getExportFormats(final HttpServletRequest request, final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0") @PathVariable final EntityId projectId) throws IOException {
		validate(request);
		
		final var projectNid = projectService.getNid(projectId);
		final List<ExportFormatDescription> exportFormats = new ArrayList<>();
		/* Running this check only once per request significantly improves the performance as the method call queries the database */
		final boolean isClientAdmin = userRoleService.isClientAdmin(projectNid);
		exportExtensions.stream().filter(ext -> checkExtensionRoles(projectNid, ext.getRequiredRole(), isClientAdmin))
				.forEach(extSupportingCurrentUser -> exportFormats
						.add(new ExportFormatDescription(extSupportingCurrentUser.getFormatIdentifier(), 
								extSupportingCurrentUser.getDescription(),
								ExtensionType.EXPORT_EXTENSION, 
								extSupportingCurrentUser.getRequiredRole(), 
								extSupportingCurrentUser.getRequiredNature(),
								extSupportingCurrentUser.getParameterDescriptions(), 
								extSupportingCurrentUser.getShowOnExportPage(),
								UploadDescription.notSupported())));
		jobExtensions.stream().filter(ext -> checkExtensionRoles(projectNid, ext.getRequiredRole(), isClientAdmin))
				.forEach(extSupportingCurrentUser -> exportFormats
						.add(new ExportFormatDescription(extSupportingCurrentUser.getIdentifier(), 
								extSupportingCurrentUser.getDescription(),
								ExtensionType.JOB_EXTENSION, 
								extSupportingCurrentUser.getRequiredRole(), 
								extSupportingCurrentUser.getRequiredNature(),
								extSupportingCurrentUser.getParameterDescriptions(), 
								extSupportingCurrentUser.getShowOnExportPage(),
								extSupportingCurrentUser.getUploadDescription())));
		return exportFormats;
	}
	
	/**
	 * Exports data from the given project into the requested format.
	 * Returns a response entity that contains the exported data as an attachment.
	 * <p>
	 * Supported formats can be queried via {@link #getExportFormats(HttpServletRequest, HttpServletResponse, EntityId)}.
	 *
	 * @param request the HTTP request
	 * @param response the HTTP response
	 * @param projectId the ID of the project from which to export data
	 * @param exportFormat the identifier of the requested export format
	 * @param queryParams the query parameters of the request
	 * @return a list of supported export formats
	 * @throws IOException when sending the response fails
	 */
	@GetMapping(value=EXPORT_FORMAT_BY_TYPE_URL, produces=MediaType.APPLICATION_OCTET_STREAM_VALUE)
	@Operation(summary = "Export Mining data to specific format", operationId = "exportToFormat")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled or the export format is not supported")
	@Role({VIEWER})
	@Nature({MINING, DISCOVERY})
	public @Nullable ResponseEntity<Resource> exportToFormat(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the export format", required = true)
			@PathVariable final String exportFormat,
			@Parameter(description = "parameters for the export", required = false, explode = Explode.TRUE, schema = @Schema(type = "object"))
			@RequestParam final MultiValueMap<String, String> queryParams) throws IOException
	{

		validate(request, URI_VAR_EXPORT_FORMAT);

		final var projectNid = projectService.getNid(projectId);
		final Optional<MiningExportExtension> exportExtension = exportExtensions.stream()
				.filter(extension -> extension.getFormatIdentifier().equals(exportFormat))
				.findFirst();

		if ( ! exportExtension.isPresent()) {
			response.sendError(HttpStatus.NOT_IMPLEMENTED.value(), "Export Format " + exportFormat + " not supported");
			return ResponseEntity.noContent().build();
		}
		
		final NatureType requiredNature = exportExtension.get().getRequiredNature();
		final RoleType requiredRole = exportExtension.get().getRequiredRole();

		if ( ! userRoleService.isAdmin()
				&& ( ! userRoleService.getNaturesOnProject(projectNid).contains(requiredNature) || ! userRoleService.hasRequiredRole(projectNid, requiredRole))
				&& ! userRoleService.isClientAdmin(projectNid)) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN, "this action requires Nature " + requiredNature + " and Role" + requiredRole);
		}

		try {
			final ExportValue exportValue = exportExtension.get().export(projectId, queryParams);
			final Resource resource = new InputStreamResource(exportValue.getInputStream());
			response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
			return ResponseEntity.ok()
					.header(HttpHeaders.CONTENT_DISPOSITION, String.format("attachment; filename=%s", exportValue.getFileName()))
					.header(HttpHeaders.CONTENT_TYPE, exportValue.getContentType())
					.body(resource);
		} catch (final ExportException e) {
			final String message = String.format("Error occured while exporting data from project %s to format %s: %s",
					projectId, exportFormat, e.getMessage());
			LOG.error(message, e);
			response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), message);
			return null;
		}
	}

	/**
	 * Exports a Discovery Effort Summary Excel workbook from the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @return response entity with a resource body
	 * @throws IOException in case of an error
	 */
	@GetMapping(value=EFFORT_SUMMARY_URL, produces=MediaType.APPLICATION_OCTET_STREAM_VALUE)
	@Operation(summary = "Exports an Effort Summary Excel file from a Mining project")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@ApiResponse(responseCode = "500", description = "if an internal error occurs")
	@ApiResponse(responseCode = "501", description = "if this feature is not enabled")
	@Role({MANAGER})
	@Nature({DISCOVERY, MINING})
	public void exportEffortSummaryExcel(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId) throws IOException {
		validate(request);

		final var summary = effortSummaryService.findAny(q -> q.ofProject(projectId));
		if (summary.isEmpty()) {
			throw new ResponseStatusException(HttpStatus.NOT_FOUND, "No effort summary data available for project " + projectId);
		}

		final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime());
		final String workbookId = String.format("effort-summary_%d_%s.xlsx", projectService.getNid(projectId), timestamp);
		response.setStatus(HttpStatus.OK.value());
		response.setHeader("Access-Control-Expose-Headers", "Content-Disposition");
		response.setHeader(HttpHeaders.CONTENT_DISPOSITION, String.format("attachment; filename=%s", workbookId));
		response.setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_OCTET_STREAM_VALUE);
		excelExportService.exportEffortSummaryExcel(projectId, response.getOutputStream());
	}
	
	private boolean checkExtensionRoles(final Long projectId, final RoleType requiredRole, final boolean isClientAdmin) {
		return isClientAdmin || userRoleService.isAdmin() || userRoleService.hasRequiredRole(projectId, requiredRole);
    }

	/**
	 * Imports a file into the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 * @param file the file to import
	 */
	@PostMapping(value=PROJECT_AS_FILE_URL, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	@Operation(summary = "Imports a file into a Mining project")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project does not exist")
	@Role({MANAGER})
	@Nature({MINING})
	public char[] uploadFile(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@RequestBody(description = "the file to import", required = true)
			@org.springframework.web.bind.annotation.RequestBody final MultipartFile file) throws IOException {

		validate(request);
		return sourceService.put(projectId, new BinaryString(file.getBytes())).toString().toCharArray();
	}

	/**
	 * Deletes a file from the given project ID.
	 *
	 * @param request access to the request
	 * @param response the HTTP response
	 * @param projectId the ID of the project
	 */
	@DeleteMapping(value=PROJECT_AS_FILE_URL)
	@Operation(summary = "Deletes a file from a Mining project")
	@ApiResponse(responseCode = "204", description = "if the request was successful")
	@ApiResponse(responseCode = "404", description = "if the given project or file does not exist")
	@Role({MANAGER})
	@Nature({MINING})
	public void deleteFile(
			final HttpServletRequest request,
			final HttpServletResponse response,
			@Parameter(description = "the ID of the project", required = true, example = "0")
			@PathVariable final EntityId projectId,
			@Parameter(description = "the ID of the file", required = true)
			@RequestParam final String fileId) throws IOException {

		validate(request);
		response.setStatus(HttpStatus.NO_CONTENT.value());
		sourceService.remove(EntityId.of(fileId), projectId);
	}

}
