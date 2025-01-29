/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.BinaryString;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.data.core.storeast.DefaultStoreAstExecutor;
import innowake.mining.data.datapoints.DataPointQueryResult;
import innowake.mining.data.datapoints.DataPointQueryService;
import innowake.mining.data.datapoints.DataPointSelection;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.extensions.export.table.DataPointTableExportHelper;
import innowake.mining.extensions.export.table.LineBuilderFactory;
import innowake.mining.extensions.export.table.TableLineBuilder;
import innowake.mining.server.job.MiningJob;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.server.util.OrderedAnnotationRuleFinderUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;

/**
 * Job that exports the results of a Ordered Annotation Rules as CSV.
 */
public class OrderedAnnotationRuleCSVExporterJob extends MiningJob<FileSystemResult> {

	private static final Logger LOG = LoggerFactory.getLogger(OrderedAnnotationRuleCSVExporterJob.class);

	private static class PagedResult {
		final int totalElements;
		final List<List<String>> columnsAndRows;

		public PagedResult(final int totalElements, final List<List<String>> columnsAndRows) {
			this.totalElements = totalElements;
			this.columnsAndRows = columnsAndRows;
		}
	}
	
	private final Map<String, List<String>> parameters;
	
	/* the SecurityContext of the user that submitted the Job */
	private final SecurityContext securityContext;
	
	private static final String EXECUTION_ORDER_COLUMN = "Execution Order";
	/* path to the data point on paged results that indicates the total number of elements */
	private static final String TOTAL_ELEMENTS_DATAPOINT = "totalElements";
	/* parameter for GraphQL queries that holds the page size */
	private static final String SIZE_PARAMETER = "size";
	/* parameter for GraphQL queries that holds the page number */
	private static final String PAGE_PARAMETER = "page";
	private static final int PAGE_SIZE = 100000;
	
	@Autowired
	private transient MiningDataCoreService core;
	
	@Autowired
	private transient AstService astService;
	
	@Autowired
	private transient DataPointRegistry registry;
	
	@Autowired
	private transient DataPointQueryService queryService;
	
	/**
	 * Creates a new Job. The {@code SecurityContext} is required so that the Job inherits the permissions of the user that submitted the Job.
	 * This determines what data points the user can access.
	 *
	 * @param projectId the id of the project from which to export data points
	 * @param parameters the export parameters - see the documentation of the {@link OrderedAnnotationRuleCSVExporter} class for details
	 * @param securityContext the {@code SecurityContext} that was active when the Job was created
	 */
	public OrderedAnnotationRuleCSVExporterJob(final EntityId projectId, final Map<String, List<String>> parameters, final SecurityContext securityContext) {
		super(projectId);
		this.projectId = projectId;
		this.parameters = parameters;
		this.securityContext = securityContext;
	}
	
	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Ordered Annotation Rule CSV Data Export");
		progressMonitor.setStepDescription("Calculating Control Flow ...");
		final SecurityContext previousContext = SecurityContextHolder.getContext();
		SecurityContextHolder.setContext(securityContext);
		final String queryName = DataPointTableExportHelper.extractQueryName(parameters);
		final List<MiningDataPointDefinitionWithPath> dataPoints = new ArrayList<>();
		final MiningDataPointDefinition queryDefinition = registry.getQueryDefinitions().get(queryName);
		if (queryDefinition == null) {
			throw new IllegalArgumentException("The query " + queryName + " does not exist.");
		}
		final String queryType = Optional.ofNullable(queryDefinition.getReferenceTypeName())
				.orElseThrow(() -> new IllegalStateException("ReferenceTypeName is null"));
		final List<String> columns = DataPointTableExportHelper.extractColumns(parameters);
		for (final String col : columns) {
			final Optional<MiningDataPointDefinitionWithPath> dp = registry.getDataPointAtPath(Optional.of(projectId).map(EntityId::getNid), queryType, col);
			dp.ifPresentOrElse(dataPoints::add, () -> {
				throw new IllegalArgumentException("The data point " + col + " does not exist for the query " + queryName);
			});
		}
		try {
			final List<String> moduleIds = Optional.ofNullable(parameters.get("moduleId")).orElse(Collections.emptyList());
			final EntityId moduleId = moduleIds.isEmpty() ? null : EntityId.of(moduleIds.get(0));
			if (moduleId != null) {
				final List<AnnotationPojo> annotations = core.annotationService.find(q -> q.ofModule(moduleId));
				if ( ! annotations.isEmpty()) {
					ControlFlowGraph graph = astService.getControlFlow(moduleId, null);
					if (graph.nodes.isEmpty() || graph.edges.isEmpty()) {
						innowake.mining.data.core.controlflow.ControlFlowGraph.calculate(moduleId, new DefaultStoreAstExecutor(), core);
						graph = astService.getControlFlow(moduleId, null);
					}
					final Map<Integer, AnnotationPojo> orderedMap = new OrderedAnnotationRuleFinderUtil(graph, annotations).findAnnotationRulesOrder();
					return runInternal(progressMonitor, orderedMap, queryType, dataPoints, columns);
				} else {
					throw new IllegalArgumentException("No Annotations are present for this Module with ID :"+ moduleId);
				}
			} else {
				throw new IllegalArgumentException("Module Id is null");
			}
		} catch (final Exception e) {
			LOG.error(() -> e.getMessage(), e);
			return new Result<>(new Status(e));
		} finally {
			SecurityContextHolder.setContext(previousContext);
		}
	}

	private Result<FileSystemResult> runInternal(final ProgressMonitor progressMonitor, final Map<Integer, AnnotationPojo> orderedMap, final String queryType,
			final List<MiningDataPointDefinitionWithPath> dataPoints, final List<String> columns) {
		progressMonitor.setStepDescription("Exporting data for Ordered Annotation Rule export");
		
		final boolean isPaged = registry.getDataPointAtPath(Optional.of(projectId).map(EntityId::getNid), queryType, TOTAL_ELEMENTS_DATAPOINT).isPresent();

		/* check if explicit size was specified via parameters */
		final Optional<Integer> specifiedSize = Optional.ofNullable(parameters.get(SIZE_PARAMETER))
				.flatMap(param -> param.stream().map(Integer::valueOf).findAny());
		
		final List<String> columnHeaders = DataPointTableExportHelper.obtainColumnHeaders(Optional.of(projectId).map(EntityId::getNid), registry, "annotations", columns);
		columnHeaders.add(0, EXECUTION_ORDER_COLUMN);
		final LineBuilderFactory factory = new CSVLineBuilderFactory();
		final int pageSize;
		pageSize = specifiedSize.orElse(isPaged ? PAGE_SIZE : -1);
		
		try (final OutputStream out = createResultFile();
				final TableLineBuilder lineBuilder = factory.getTableLineBuilder(out)) {
			lineBuilder.buildHeaderLine(columnHeaders);
			int currentPage = 0;
			int processedRows = 0;

			PagedResult result = getPagedResult(dataPoints, currentPage, pageSize);

			final List<List<String>> filteredColumnsAndRows = getOrderRuleDatapoints(orderedMap, result);

			progressMonitor.begin(specifiedSize.isPresent() ? specifiedSize.get() : filteredColumnsAndRows.size());

			for (final List<String> columnsAndRow : filteredColumnsAndRows) {
				lineBuilder.buildStandardLine(columnsAndRow);
			}
			processedRows += filteredColumnsAndRows.size();
			progressMonitor.worked(filteredColumnsAndRows.size());

			/* if size was specified via parameters, we stop after fetching one page - otherwise we fetch all results */
			while (!specifiedSize.isPresent() && processedRows < filteredColumnsAndRows.size()) {
				currentPage++;
				result = getPagedResult(dataPoints, currentPage, pageSize);
				for (final List<String> columnsAndRow : filteredColumnsAndRows) {
					lineBuilder.buildStandardLine(columnsAndRow);
				}
				processedRows += filteredColumnsAndRows.size();
				progressMonitor.worked(filteredColumnsAndRows.size());
				progressMonitor.setStepDescription(String.format("exporting data (%d/%d rows) ...", processedRows, result.totalElements));
			}

		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		final String fileName = String.format("orderd_annotation_rules_%s_%s.csv", projectId.getNid(), new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()));

		return new Result<>(new FileSystemResult("text/csv", fileName));
	}


	private List<List<String>> getOrderRuleDatapoints(final Map<Integer, AnnotationPojo> orderedMap, final PagedResult result) {
		final Set<String> sourceAttachments = new HashSet<>();
		final Map<Integer, String> controlFlowOrder = new LinkedHashMap<>();
		final Map<String, List<String>> sourceAttachmentsAndRow = new LinkedHashMap<>();

		for (final Map.Entry<Integer, AnnotationPojo> entry : orderedMap.entrySet()) {
			final AnnotationPojo annotation = entry.getValue();
			sourceAttachments.add(annotation.getSourceAttachment().orElse(BinaryString.EMPTY).toString());
			controlFlowOrder.put(entry.getKey(), annotation.getSourceAttachment().orElse(BinaryString.EMPTY).toString());
		}
		for (final List<String> row : result.columnsAndRows) {
			for (final String cell : row) {
				if (sourceAttachments.contains(cell)) {
					sourceAttachmentsAndRow.put(cell, row);
				}
			}
		}

		/* Filter sourceAttachment values in List<List<String>> */
		final List<List<String>> filteredColumnsAndRows = new ArrayList<>();
		for (final Entry<Integer, String> entry : controlFlowOrder.entrySet()) {
			final List<String> row = sourceAttachmentsAndRow.get(entry.getValue());
			final List<String> filteredRow = new ArrayList<>(row.size() + 1);
			filteredRow.add(String.valueOf(entry.getKey()));
			filteredRow.addAll(row);
			filteredColumnsAndRows.add(filteredRow);
		}
		return filteredColumnsAndRows;
	}

	private PagedResult getPagedResult(final List<MiningDataPointDefinitionWithPath> dataPoints, final int page, final int size) {
		final Map<String, List<String>> actualParameters = new HashMap<>();
		actualParameters.put("$query", parameters.get("$query"));
		actualParameters.put("$columns", parameters.get("$columns"));
		final List<String> actualColumns = new ArrayList<>(dataPoints.size() + 1);
		dataPoints.stream().map(MiningDataPointDefinitionWithPath::getPath).forEachOrdered(actualColumns::add);

		if (size >= 0) {
			/* add the pagination attributes */
			actualParameters.put(PAGE_PARAMETER, Collections.singletonList(String.valueOf(page)));
			actualParameters.put(SIZE_PARAMETER, Collections.singletonList(String.valueOf(size)));

			/* additionally select the "totalElements" data point */
			actualColumns.add(TOTAL_ELEMENTS_DATAPOINT);
		}

		final DataPointQueryResult queryResult = DataPointTableExportHelper.obtainResult(projectId.getNid(), actualParameters, queryService, actualColumns);
		final List<DataPointSelection> selections = dataPoints.stream()
				.map(dp -> queryResult.getDataPoint(dp.getPath(), dp.isAlias() ? Optional.ofNullable(dp.getAliasFor())
				.orElseThrow(() -> new IllegalStateException("AliasFor is null")).getJsonPath() : ""))
				.collect(Collectors.toList());

		/* Create easily exportable List of Lists containing the table data */
		final List<List<String>> columnsAndRows;
		if (DataPointTableExportHelper.hasUnrollParameter(parameters)) {
			/* export "flat" table with no nested arrays */
			columnsAndRows = DataPointTableExportHelper.convertSelectionsToTable(selections);
		} else {
			/* keep nested arrays as-is */
			final int rows = Collections.max(selections.stream().map(sel -> sel.getDimensions() > 0 ? ((List<?>) Optional.ofNullable(sel.getValue())
					.orElseThrow(() -> new IllegalStateException("value is null"))).size() : 0)
					.collect(Collectors.toList()));
			
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
