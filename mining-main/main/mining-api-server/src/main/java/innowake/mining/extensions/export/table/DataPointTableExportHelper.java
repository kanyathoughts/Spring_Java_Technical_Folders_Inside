package innowake.mining.extensions.export.table;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.datapoints.DataPointQueryResult;
import innowake.mining.data.datapoints.DataPointQueryService;
import innowake.mining.data.datapoints.DataPointSelection;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.util.ExporterUtil;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.extensions.MiningExportExtension.ExportValue;
import innowake.mining.shared.model.aggregations.AggregationResult;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Class used to run common tasks of the DataPoint-based Mining Table Exporters
 * (DataPointConfluenceExporter, DataPointCSVExporter)
 * 
 * @author jsimianer
 *
 */
public class DataPointTableExportHelper {
	
	private static final String QUERY_NAME_PARAMETER = "$query";
	private static final String COLUMNS_PARAMETER = "$columns";
	private static final String UNROLL_PARAMETER = "$unroll";
	
	private static final Logger LOG = LoggerFactory.getLogger(DataPointTableExportHelper.class);

	private DataPointTableExportHelper() {
		/* this class only has static methods */
	}

	/**
	 * Writes table data to a stream using a TableWriter and creates an ExportValue containing the formatted table.
	 *
	 * @param queryName name of graphql query, used for the file name
	 * @param projectId id of project the data is exported from, used for file name
	 * @param contentType content type of the exported content
	 * @param factory the line builder factory used to obtain table line builders to build the lines of the table
	 * @param columnsAndRows contains table data
	 * @param columnHeaders contains table header data
	 * @return ExportValue containing the formatted table that is supposed to be exported
	 */
	public static ExportValue buildExportValue(final String queryName, final long projectId, final String contentType, final LineBuilderFactory factory, final List<List<String>> columnsAndRows, final List<String> columnHeaders) {
		final PipedInputStream in = new PipedInputStream();			
		final CountDownLatch latchExport = new CountDownLatch(1);
		final Thread export = new Thread(() -> {
			try (
				final PipedOutputStream out = new PipedOutputStream(in);
				final TableLineBuilder lineBuilder = factory.getTableLineBuilder(out)
				){
				latchExport.countDown();
				lineBuilder.buildHeaderLine(columnHeaders);
				for (final List<String> columnValues : columnsAndRows) {
					lineBuilder.buildStandardLine(columnValues);
				}
			} catch (final IOException e) {
				LOG.error("IOException writing table to output stream", e);
			}
		});
		export.start();
		
		try {
			latchExport.await();
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		final String fileEnding;
		switch (contentType) {
		case ExporterUtil.CONTENT_TYPE_CSV:
			fileEnding = "csv";
			break;
		case ExporterUtil.CONTENT_TYPE_XML:
			fileEnding = "xml";
			break;
		case ExporterUtil.CONTENT_TYPE_PLAIN:
		default:
			fileEnding = "txt";
			break;
		}
		return ExporterUtil.createExportValue(in, 
				String.format("%s_%s_%s.%s", queryName, projectId,
						new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()), fileEnding), contentType);
	}
	
	/**
	 * Runs a datapoint query and returns the result.
	 * 
	 * @param projectId id of the project
	 * @param parameters query parameters
	 * @param queryService used to run the query
	 * @param columns datapoint columns. Used to run the query
	 * 
	 * @return result of the query
	 * @throws IllegalStateException if query fails
	 */
	public static DataPointQueryResult obtainResult(final Long projectId, final Map<String, List<String>> parameters, final DataPointQueryService queryService, final List<String> columns) {
		final String queryName = Optional.ofNullable(parameters.get(QUERY_NAME_PARAMETER)).map(p -> p.get(0))
				.orElseThrow(() -> new IllegalArgumentException("The " + QUERY_NAME_PARAMETER + " parameter is mandatory."));
		
		final Map<String, Object> queryParameters = obtainQueryParameters(parameters);
		queryParameters.put("projectId", projectId);
		
		final DataPointQueryResult queryResult = queryService.queryDataPointsByPath(Optional.of(projectId), queryName, queryParameters, columns);
		if ( ! queryResult.getErrors().isEmpty()) {
			throw new IllegalStateException("The query for data has failed: " + queryResult.getErrors());
		}
		return queryResult;
	}
	
	private static Map<String, Object> obtainQueryParameters(final Map<String, List<String>> parameters) {
		final Map<String, Object> queryParameters = new HashMap<>();
		for (final Map.Entry<String, List<String>> param : parameters.entrySet()) {
			/* Pass all remaining parameters expect $query and $columns on to the actual query.
			 * As a quirk we only pass parameters as list if the list contains more than 1 entry.
			 * The GraphQl execution will figure out what we mean (I hope). */
			if (QUERY_NAME_PARAMETER.equals(param.getKey()) || COLUMNS_PARAMETER.equals(param.getKey()) || UNROLL_PARAMETER.equals(param.getKey())) {
				continue;
			}
			
			if (param.getValue().size() == 1) {
				queryParameters.put(param.getKey(), param.getValue().get(0));
			} else {
				queryParameters.put(param.getKey(), param.getValue());
			}
		}
		return queryParameters;
	}
	
	/**
	 * Obtains the columnHeaders for a specified list of columns.
	 * @param projectId optionally include project specific datapoints
	 * @param registry used to get the datapoints
	 * @param queryName name of the datapoint query
	 * @param columns will be mapped to first datapoints display name
	 * @return list of columnHeaders (DataPoint display names) of the specified columns
	 */
	public static List<String> obtainColumnHeaders(final Optional<Long> projectId, final DataPointRegistry registry, final String queryName, final List<String> columns) {
		/* first row of the table will contain the columnHeaders, for these, we need to look up the data point definitions: */
		final String queryType = assertNotNull(registry.getQueryDefinitions().get(queryName).getReferenceTypeName());

		final List<String> columnHeaders = new ArrayList<>();
		for (final String col: columns) {
			final Optional<MiningDataPointDefinitionWithPath> dp = registry.getDataPointAtPath(projectId, queryType, col);
			if (dp.isPresent()) {
				columnHeaders.add(dp.get().getDisplayName());
			} else {
				throw new IllegalArgumentException("" + col + " is not a valid column name for this query");
			}
		}

		return columnHeaders;
	}

	/**
	 * Retrieves the values of the given data point selection at the given indexes as strings.
	 * The returned list of values will have the same length as the provided {@code selections}.
	 * <p>
	 * If the resolved value is {@code null} then empty string will be returned instead. If the selection has fewer dimensions
	 * than are provided, the additional indexes are ignored.
	 *
	 * @param selections a list of data point selections
	 * @param indexes a list of indexes to index the selections
	 * @return the list of resolved values as strings; list has the same length as {@code selections}
	 */
	public static List<String> obtainColumnValues(final List<DataPointSelection> selections, final int... indexes) {
		return selections.stream().map(sel -> {
			final Object value;
			if (sel.getDimensions() < indexes.length) {
				/* this data point has fewer dimensions - we will use the same value for all remaining inner indexes */
				if (sel.getDimensions() == 0) {
					value = sel.getValue();
				} else {
					value = sel.getValue(Arrays.copyOfRange(indexes, 0, sel.getDimensions()));
				}
			} else {
				value = sel.getValue(indexes);
			}
			if (value == null) {
				return "";
			} else if (value instanceof List<?>) {
				return ((List<?>) value).stream().map(Objects::toString).collect(Collectors.joining(", "));
			} else {
				return value.toString();
			}
		}).collect(Collectors.toList());
	}

	/**
	 * Convert a list of selections to a flat two-dimensional table.
	 * <p>
	 * This method will "unroll" nested dimensions in order to end up with a flat table structure. For example, if you have this table data
	 * <pre>
	 *     [
	 *     		["Foo", ["Bar", "Baz"]],
	 *     		["Qux", ["Quux"]]
	 *     ]
	 * </pre>
	 * Then the resulting table will look like
	 * <pre>
	 *     [
	 * 	   		["Foo", "Bar"],
	 * 	   		["Foo", "Baz"],
	 * 	   		["Qux", "Quux"]
	 * 	   ]
	 * </pre>
	 * @param selections a list of {@code DataPointSelections} that will make up the columns of the created table
	 * @return a nested list of rows and columns
	 */
	public static List<List<String>> convertSelectionsToTable(final List<DataPointSelection> selections) {
		final List<List<String>> table = new ArrayList<>();
		convertSelectionsToTableRec(table, selections, new int[0]);
		return table;
	}

	private static void convertSelectionsToTableRec(final List<List<String>> table, final List<DataPointSelection> selections, final int[] indexes) {
		final int rows = selections.stream()
				.filter(sel -> sel.getDimensions() > indexes.length)
				.map(sel -> {
					final Object value = sel.getValue(indexes);
					return value instanceof List<?> ? ((List<?>) value).size() : 0;
				})
				.max(Integer::compare)
				.orElse(0);
		if (rows > 0) {
			for (int i = 0; i < rows; i++) {
				final int[] indexesCopy = Arrays.copyOf(indexes, indexes.length + 1);
				indexesCopy[indexesCopy.length - 1] = i;
				convertSelectionsToTableRec(table, selections, indexesCopy);
			}
		} else {
			table.add(obtainColumnValues(selections, indexes));
		}
	}
	
	/**
	 * Obtains the query name parameter from a map of parameters.
	 * 
	 * @param parameters needs to contain the query name parameter
	 * @return query name
	 */
	public static String extractQueryName(final Map<String, List<String>> parameters) {
		return Optional.ofNullable(parameters.get(QUERY_NAME_PARAMETER)).map(p -> p.get(0))
				.orElseThrow(() -> new IllegalArgumentException("The " + QUERY_NAME_PARAMETER + " parameter is mandatory."));
	}
	
	/**
	 * Extracts the columns parameter from a map of parameters.
	 * 
	 * @param parameters needs to contain the columns parameter
	 * @return list of columns
	 */
	public static List<String> extractColumns(final Map<String, List<String>> parameters) {
		final List<String> columns = new ArrayList<>(Optional.ofNullable(parameters.get(COLUMNS_PARAMETER)).orElse(Collections.emptyList()));
		if (columns.isEmpty()) {
			throw new IllegalArgumentException("At least one column is required for table export. Specify columns using the " + COLUMNS_PARAMETER + " parameter");
		}
		if (columns.size() == 1) {
			/* workaround for mining-cli (and possibly other openapi clients) which incorrectly passes the columns as a single comma-separated string */
			return Arrays.asList(columns.get(0).split(","));
		}
		return columns;
	}

	/**
	 * Returns whether {@code $unroll=true} parameter is present.
	 *
	 * @param parameters the query parameters
	 * @return whether the {@code $unroll} option is enabled
	 */
	public static boolean hasUnrollParameter(final Map<String, List<String>> parameters) {
		return Optional.ofNullable(parameters.get(UNROLL_PARAMETER)).map(p -> p.get(0)).map(Boolean::valueOf).orElse(false);
	}

	/**
	 * Converts a list of {@link AggregationResult} to a flat table. The table will have one column for each field listed in {@code groupFields}
	 * followed by one column for each field listed in {@code fields}.
	 *
	 * @param aggregationResults the list of aggregation results
	 * @param groupFields the list of group fields of the aggregation result that shall be included in the table
	 * @param fields the list of fields that are to be included in the table
	 * @param <T> the field type of the {@code AggregationResult}
	 * @return the aggregation result converted to a flat table
	 */
	public static <T> List<List<String>> convertAggregationResultToTable(final List<AggregationResult<T>> aggregationResults,
			final List<T> groupFields, final List<T> fields) {
		if (aggregationResults.isEmpty()) {
			return Collections.emptyList();
		}

		/* convert AggregationResult to GraphQlQueryResult-like structure (nested maps, top level must be a map, not list),
		 * so that we can use convertSelectionToTable() */
		final List<Map<String, Object>> results = new ArrayList<>(aggregationResults.size());
		for (final AggregationResult<?> aggregationResult : aggregationResults) {
			final Map<String, Object> result = new HashMap<>();
			result.put("group",
					aggregationResult.getGroup().entrySet().stream().collect(Collectors.toMap(entry -> entry.getKey().toString(), Map.Entry::getValue)));
			result.put("fields",
					aggregationResult.getFields().entrySet().stream().collect(Collectors.toMap(entry -> entry.getKey().toString(), Map.Entry::getValue)));
			results.add(result);
		}
		final Map<String, Object> data = new HashMap<>();
		data.put("data", results);

		final DataPointQueryResult queryResult = new DataPointQueryResult(Collections.emptyList(), data);
		final List<DataPointSelection> selections = Stream.concat(
				groupFields.stream().map(field -> "data.group." + field),
				fields.stream().map(field -> "data.fields." + field)
		).map(queryResult::getDataPoint).collect(Collectors.toList());

		return convertSelectionsToTable(selections);
	}
}
