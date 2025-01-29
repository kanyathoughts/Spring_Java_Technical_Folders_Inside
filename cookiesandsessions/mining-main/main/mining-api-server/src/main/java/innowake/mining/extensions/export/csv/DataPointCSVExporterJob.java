/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.datapoints.DataPointQueryResult;
import innowake.mining.data.datapoints.DataPointQueryService;
import innowake.mining.data.datapoints.DataPointSelection;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.extensions.export.table.DataPointTableExportHelper;
import innowake.mining.extensions.export.table.LineBuilderFactory;
import innowake.mining.extensions.export.table.TableLineBuilder;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;

/**
 * Job that exports the results of a {@linkplain DataPointQueryService#queryDataPoints(Optional, String, Map, List) data point query} as CSV.
 */
public class DataPointCSVExporterJob extends MiningJob<FileSystemResult> {

	private static class PagedResult {
		final int totalElements;
		final List<List<String>> columnsAndRows;

		public PagedResult(final int totalElements, final List<List<String>> columnsAndRows) {
			this.totalElements = totalElements;
			this.columnsAndRows = columnsAndRows;
		}
	}

	/* path to the data point on paged results that indicates the total number of elements */
	private static final String TOTAL_ELEMENTS_DATAPOINT = "totalElements";
	/* parameter to set the file name which was passed via parameter*/
	private static final String FILE_NAME_PARAMETER = "$fileName";
	/* parameter for GraphQL queries that holds the page size */
	private static final String SIZE_PARAMETER = "size";
	/* parameter for GraphQL queries that holds the page number */
	private static final String PAGE_PARAMETER = "page";
	private static final int PAGE_SIZE = 100000;

	@Nullable
	private Long projectNid = null;
	private final Map<String, List<String>> parameters;
	/* the SecurityContext of the user that submitted the Job */
	private final SecurityContext securityContext;

	@Autowired
	private transient DataPointQueryService queryService;

	@Autowired
	private transient DataPointRegistry registry;

	@Autowired
	private transient ProjectService projectService;

	/**
	 * Creates a new Job. The {@code SecurityContext} is required so that the Job inherits the permissions of the user that submitted the Job.
	 * This determines what data points the user can access.
	 *
	 * @param projectId the id of the project from which to export data points
	 * @param parameters the export parameters - see the documentation of the {@link DataPointCSVExporter} class for details
	 * @param securityContext the {@code SecurityContext} that was active when the Job was created
	 */
	public DataPointCSVExporterJob(final EntityId projectId, final Map<String, List<String>> parameters, final SecurityContext securityContext) {
		super(projectId);
		this.parameters = parameters;
		this.securityContext = securityContext;
	}
	
	private Long getProjectNid() {
		if (projectNid == null) {
			projectNid = projectService.getNid(projectId);
		}
		return Assert.assertNotNull(projectNid);
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
		progressMonitor.setJobDescription("CSV Data Export");
		progressMonitor.setStepDescription("exporting data ...");
		
		final String queryName = DataPointTableExportHelper.extractQueryName(parameters);
		final MiningDataPointDefinition queryDefinition = registry.getQueryDefinitions().get(queryName);
		if (queryDefinition == null) {
			throw new IllegalArgumentException("The query " + queryName + " does not exist.");
		}
		final String queryType = assertNotNull(queryDefinition.getReferenceTypeName());
		final List<String> columns = DataPointTableExportHelper.extractColumns(parameters);


		final List<MiningDataPointDefinitionWithPath> dataPoints = new ArrayList<>(columns.size());
		for (final String col : columns) {
			final Optional<MiningDataPointDefinitionWithPath> dp = registry.getDataPointAtPath(Optional.of(getProjectNid()), queryType, col);
			if (dp.isPresent()) {
				dataPoints.add(dp.get());
			} else {
				throw new IllegalArgumentException("The data point " + col + " does not exist for the query " + queryName);
			}
		}

		/* check if the query supports paging by checking for presence of the "totalElements" data point (not 100% accurate, but should work) */
		final boolean isPaged = registry.getDataPointAtPath(Optional.of(getProjectNid()), queryType, TOTAL_ELEMENTS_DATAPOINT).isPresent();

		/* check if explicit size was specified via parameters */
		final Optional<Integer> specifiedSize = Optional.ofNullable(parameters.get(SIZE_PARAMETER))
				.flatMap(param -> param.stream().map(Integer::valueOf).findAny());

		final List<String> columnHeaders = DataPointTableExportHelper.obtainColumnHeaders(Optional.of(getProjectNid()), registry, queryName, columns);
		final LineBuilderFactory factory = new CSVLineBuilderFactory();
		final int pageSize;
		if (specifiedSize.isPresent()) {
			pageSize = specifiedSize.get();
		} else if (isPaged) {
			pageSize = PAGE_SIZE;
		} else {
			pageSize = -1;
		}

		try (final OutputStream out = createResultFile();
			 final TableLineBuilder lineBuilder = factory.getTableLineBuilder(out)) {

			lineBuilder.buildHeaderLine(columnHeaders);
			int currentPage = 0;
			int processedRows = 0;

			PagedResult result = getPagedResult(dataPoints, currentPage, pageSize);
			progressMonitor.begin(specifiedSize.isPresent() ? specifiedSize.get() : result.totalElements);
			for (final List<String> columnsAndRow : result.columnsAndRows) {
				lineBuilder.buildStandardLine(columnsAndRow);
			}
			processedRows += result.columnsAndRows.size();
			progressMonitor.worked(result.columnsAndRows.size());

			/* if size was specified via parameters, we stop after fetching one page - otherwise we fetch all results */
			while ( ! specifiedSize.isPresent() && processedRows < result.totalElements) {
				currentPage++;
				result = getPagedResult(dataPoints, currentPage, pageSize);
				for (final List<String> columnsAndRow : result.columnsAndRows) {
					lineBuilder.buildStandardLine(columnsAndRow);
				}
				processedRows += result.columnsAndRows.size();
				progressMonitor.worked(result.columnsAndRows.size());
				progressMonitor.setStepDescription(String.format("exporting data (%d/%d rows) ...", processedRows, result.totalElements));
			}

		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		final String fileNamePrefix = Optional.ofNullable(parameters.get(FILE_NAME_PARAMETER)).map(p -> p.get(0)).orElse(queryName);
		final String fileName = String.format("%s_%s_%s.csv", fileNamePrefix, projectService.getNid(projectId), new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()));

		return new Result<>(new FileSystemResult("text/csv", fileName));
	}

	private PagedResult getPagedResult(final List<MiningDataPointDefinitionWithPath> dataPoints, final int page, final int size) {
		final Map<String, List<String>> actualParameters = new HashMap<>(parameters);
		actualParameters.remove(FILE_NAME_PARAMETER);
		final List<String> actualColumns = new ArrayList<>(dataPoints.size() + 1);
		dataPoints.stream().map(MiningDataPointDefinitionWithPath::getPath).forEachOrdered(actualColumns::add);

		if (size >= 0) {
			/* add the pagination attributes */
			actualParameters.put(PAGE_PARAMETER, Collections.singletonList(String.valueOf(page)));
			actualParameters.put(SIZE_PARAMETER, Collections.singletonList(String.valueOf(size)));

			/* additionally select the "totalElements" data point */
			actualColumns.add(TOTAL_ELEMENTS_DATAPOINT);
		}

		final DataPointQueryResult queryResult = DataPointTableExportHelper.obtainResult(getProjectNid(), actualParameters, queryService, actualColumns);
		final List<DataPointSelection> selections = dataPoints.stream()
				.map(dp -> queryResult.getDataPoint(dp.getPath(), dp.isAlias() ? assertNotNull(dp.getAliasFor()).getJsonPath() : ""))
				.collect(Collectors.toList());

		/* Create easily exportable List of Lists containing the table data */
		final List<List<String>> columnsAndRows;
		if (DataPointTableExportHelper.hasUnrollParameter(parameters)) {
			/* export "flat" table with no nested arrays */
			columnsAndRows = DataPointTableExportHelper.convertSelectionsToTable(selections);
		} else {
			/* keep nested arrays as-is */
			final int rows = Collections.max(selections.stream().map(sel -> sel.getDimensions() > 0 ? ((List<?>) assertNotNull(sel.getValue())).size() : 0).collect(Collectors.toList()));
			columnsAndRows = new ArrayList<>(rows);
			for (int i = 0; i < rows; i++) {
				final List<String> columnValues = DataPointTableExportHelper.obtainColumnValues(selections, i);
				columnsAndRows.add(columnValues);
			}
		}

		final Integer totalElements = queryResult.getDataPoint(TOTAL_ELEMENTS_DATAPOINT).getValue();

		return new PagedResult(totalElements == null ? columnsAndRows.size() : totalElements, columnsAndRows);
	}
}
