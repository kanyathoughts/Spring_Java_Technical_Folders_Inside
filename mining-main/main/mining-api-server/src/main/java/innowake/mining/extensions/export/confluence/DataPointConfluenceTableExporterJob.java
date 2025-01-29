/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.extensions.export.confluence;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.data.datapoints.DataPointQueryResult;
import innowake.mining.data.datapoints.DataPointQueryService;
import innowake.mining.data.datapoints.DataPointSelection;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.extensions.export.table.DataPointTableExportHelper;
import innowake.mining.extensions.export.table.LineBuilderFactory;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.util.ExporterUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.extensions.MiningExportExtension.ExportValue;

/**
 * Job that exports the results of a {@linkplain DataPointQueryService#queryDataPoints(Optional, String, Map, List) data point query} as Confluence.
 */
public class DataPointConfluenceTableExporterJob extends MiningJob<FileSystemResult> {

	private static final String MODULE_NAME_DISPLAY_NAME = "Module Name";
	private static final String MODULE_LINKHASH_KEYWORD = "${linkHash}";
	private static final String PROJECT_ID_KEYWORD = "${$projectId}";

	/* For inserting links in Confluence. Parameters: module name, base url, link pattern we get from datapoint with inserted ids */
	private static final String CONFLUENCE_LINK_PATTERN = "[%s|%s/#%s]";
	private static final String VIEW_MODE_PATH = "general.viewMode";
	private static final String LINK_TEMPLATE_KEY = "linkTemplate";

	private final String baseUrl;
	private final Map<String, List<String>> parameters;
	
	/* the SecurityContext of the user that submitted the Job */
	private final SecurityContext securityContext;

	@Autowired
	private transient DataPointQueryService queryService;

	@Autowired
	private transient DataPointRegistry registry;

	/**
	 * Creates a new Job. The {@code SecurityContext} is required so that the Job inherits the permissions of the user that submitted the Job.
	 * This determines what data points the user can access.
	 *
	 * @param projectId the id of the project from which to export data points
	 * @param parameters the export parameters - see the documentation of the {@link DataPointConfluenceTableExporterJob} class for details
	 * @param baseUrl the base url to append in module name to provide links to the Module 
	 * @param securityContext the {@code SecurityContext} that was active when the Job was created
	 */
	public DataPointConfluenceTableExporterJob(final EntityId projectId, final Map<String, List<String>> parameters, final String baseUrl,
			final SecurityContext securityContext) {
		super(projectId);
		this.projectId = projectId;
		this.parameters = parameters;
		this.baseUrl = baseUrl;
		this.securityContext = securityContext;
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		final SecurityContext previousContext = SecurityContextHolder.getContext();
		SecurityContextHolder.setContext(securityContext);
		try {
			return runInternal(progressMonitor);
		} finally {
			SecurityContextHolder.setContext(previousContext);
		}
	}

	private Result<FileSystemResult> runInternal(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Confluence Data Export");
		progressMonitor.setStepDescription("Exporting data ...");

		final String queryName = DataPointTableExportHelper.extractQueryName(parameters);
		final List<String> columns = DataPointTableExportHelper.extractColumns(parameters);

		final DataPointQueryResult queryResult = DataPointTableExportHelper.obtainResult(projectId.getNid(), parameters, queryService, columns);

		final List<DataPointSelection> selections = columns.stream().map(queryResult::getDataPoint).collect(Collectors.toList());
		final int rows = Collections.max(
				selections.stream().map(sel -> sel.getDimensions() > 0 ? ((List<?>) assertNotNull(sel.getValue())).size() : 0).collect(Collectors.toList()));

		final String queryType = assertNotNull(registry.getQueryDefinitions().get(queryName).getReferenceTypeName());
		final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(Optional.of(projectId).map(EntityId::getNid), queryType);

		/* Obtain Link Template */
		final List<MiningDataPointDefinitionWithPath> dataPointsWithLink = dataPoints.stream()
				.filter(dp -> dp.getUsageAttributes().containsKey(VIEW_MODE_PATH) && dp.getUsageAttributes().get(VIEW_MODE_PATH).containsKey(LINK_TEMPLATE_KEY))
				.collect(Collectors.toList());
		//TODO: This is a hack, we need to find a better way to get the correct data point: https://iriseu.deloitte.com/browse/WMIN-14350
		final MiningDataPointDefinitionWithPath dataPoint = dataPointsWithLink.get(1);
		final String moduleLinkPattern = dataPoint.getUsageAttributes().get(VIEW_MODE_PATH).get(LINK_TEMPLATE_KEY);
		/* Obtain module IDs */
		final String path = dataPoint.getPath();
		final String idPath = path.substring(0, path.lastIndexOf('.') + 1) + dataPoint.getUsageAttributes().get(VIEW_MODE_PATH).get("togetherWith");
		final DataPointSelection idSelection = queryResult.getDataPoint(idPath);

		final List<String> columnHeaders = DataPointTableExportHelper.obtainColumnHeaders(Optional.of(projectId).map(EntityId::getNid), registry, queryName, columns);
		final int moduleIndex = columnHeaders.indexOf(MODULE_NAME_DISPLAY_NAME);
		/* We need a module name column to replace the name with a link */
		final boolean linkable = moduleIndex != -1;

		/* Create easily exportable List of Lists containing the table data with links already inserted */
		final List<List<String>> columnsAndRows = new ArrayList<>();
		for (int i = 0; i < rows; i++) {
			final int index = i;
			final List<String> columnValues = DataPointTableExportHelper.obtainColumnValues(selections, index);
			if (linkable) {
				/* Replace module name value with module link displayed as module name */
				String moduleLink = StringUtils.replace(moduleLinkPattern, MODULE_LINKHASH_KEYWORD, assertNotNull(idSelection.getValue(i)).toString());
				moduleLink = StringUtils.replace(moduleLink, PROJECT_ID_KEYWORD, projectId.getNid().toString());
				columnValues.set(moduleIndex, String.format(CONFLUENCE_LINK_PATTERN, columnValues.get(moduleIndex), baseUrl, moduleLink));
			}
			columnsAndRows.add(columnValues);
		}

		final LineBuilderFactory factory = new ConfluenceLineBuilderFactory();
		
		final ExportValue exportFormat = DataPointTableExportHelper.buildExportValue(queryName, projectId.getNid(), ExporterUtil.CONTENT_TYPE_PLAIN, factory,
				columnsAndRows, columnHeaders);
		try (OutputStream out = createResultFile()) {
			IOUtils.copy(exportFormat.getInputStream(), out);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}

		return new Result<>(new FileSystemResult("text/plain", exportFormat.getFileName()));
	}
}
