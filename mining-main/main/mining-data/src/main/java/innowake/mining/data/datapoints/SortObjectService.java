/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.MiningDataPointSortCallback;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Service for handling sort objects. Sort Objects are passed to GraphQL queries in order to determine the ordering of results.
 * <p>
 * This class has two main responsibilities:
 * <ul>
 *     <li>create a list of {@link Sort.Order} objects from a list of sort objects</li>
 *     <li>define the schema for sort objects that can be passed to each GraphQL query</li>
 * </ul>
 * The latter is done by inspecting the usage attributes of usage {@value Usages#GRAPHQL_QUERY_PREFIX} for the data points of each GraphQL query.
 */
@Service
public class SortObjectService {

	private final DataPointRegistry dataPointRegistry;

	/**
	 * Convert a list of strings of the form
	 * <pre>["field1;ASC", "field2;DESC"]</pre>
	 * to a list of sort objects of the form
	 * <pre>[{"field1": "ASC"}, {"field2": "DESC"}]</pre>
	 *
	 * @param sortBy the list of sort strings
	 * @return the list of corresponding sort objects (as Maps)
	 */
	public static List<Map<String, String>> convertLegacySortByToSortObject(final List<String> sortBy) {
		final List<Map<String, String>> sorts = new ArrayList<>();
		for (final String sortString: sortBy) {
			final String[] sortArgs = sortString.split(";");
			final String fieldName = sortArgs[0];
			final Sort.Direction direction = sortArgs.length == 2 ? Sort.Direction.fromString(sortArgs[1]) : Sort.Direction.ASC;
			sorts.add(Map.of(fieldName, direction.name()));
		}
		return sorts;
	}

	/**
	 * Constructs a new sort object service that uses the given data point registry.
	 * The registry is used to query the data points (recursively) that can be provided by a GraphQL query and to find the
	 * {@link SortByAttributes#SQL_FRAGMENT_ORDER_BY} attributes.
	 *
	 * @param dataPointRegistry the data point registry
	 * @see SortByAttributes
	 */
	@Autowired
	public SortObjectService(final DataPointRegistry dataPointRegistry) {
		this.dataPointRegistry = dataPointRegistry;
	}

	/**
	 * Parses the given sort object for the GraphQL query with the given name in the given project.
	 * Returns the list of {@link Sort.Order} objects that need to be used to be passed to the DB adapter to sort the query results.
	 * <p>
	 * This method parses the sortObject in strict mode by default.
	 *
	 * @param projectId id of the project for which the query is made
	 * @param queryName name of the GraphQL query that is executed (used to query the available data points for the query)
	 * @param sortObject the sort object describing the sort fields and direction
	 * @return a list of {@link Sort.Order} objects describing the sort fields
	 */
	public List<Sort.Order> parseSortObject(final Long projectId, final String queryName, final List<Map<String, String>> sortObject) {
		return parseSortObject(projectId, queryName, sortObject, true);
	}

	/**
	 * Parses the given sort object for the GraphQL query with the given name in the given project.
	 * Returns the list of {@link Sort.Order} objects that need to be used to be passed to the DB adapter to sort the query results.
	 *
	 * @param projectId id of the project for which the query is made
	 * @param queryName name of the GraphQL query that is executed (used to query the available data points for the query)
	 * @param sortObject the sort object describing the sort fields and direction
	 * @param strict when {@code true} an exception is thrown when the sortObject contains an unknown datapoint
	 * @return a list of {@link Sort.Order} objects describing the sort fields
	 */
	public List<Sort.Order> parseSortObject(final Long projectId, final String queryName, final List<Map<String, String>> sortObject, final boolean strict) {
		final String queryType = dataPointRegistry.getQueryDefinitions().get(queryName).getReferenceTypeName();
		final List<MiningDataPointDefinitionWithPath> dataPoints = dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(Optional.of(projectId), queryType,
				Collections.singleton(Usages.GRAPHQL_QUERY_PREFIX + queryName));

		return parseSortObject(queryName, dataPoints, sortObject, strict);
	}

	/**
	 * Parses the given sort object for the GraphQL query with the given name.
	 * This is essentially the same as {@link #parseSortObject(Long, String, List, boolean)} but the list of data points can be passed in.
	 *
	 * @param queryName name of the GraphQL query that is executed
	 * @param dataPoints list of filterable data points for the query
	 * @param sortObject the sort object describing the sort fields and direction
	 * @param strict when {@code true} an exception is thrown when the sortObject contains an unknown datapoint
	 * @return a list of {@link Sort.Order} objects describing the sort fields
	 */
	public List<Sort.Order> parseSortObject(final String queryName, final List<MiningDataPointDefinitionWithPath> dataPoints,
			final List<Map<String, String>> sortObject, final boolean strict) {
		final Map<String, String> dataPointsSqlFragmentMap = new HashMap<>();
		for (final MiningDataPointDefinitionWithPath dp : dataPoints) {
			final Map<String, String> attributes = dp.getUsageAttributes().get(Usages.GRAPHQL_QUERY_PREFIX + queryName);
			
			if (attributes == null || attributes.isEmpty()) {
				continue;
			}

			if (attributes.containsKey(SortByAttributes.SQL_FRAGMENT_ORDER_BY)) {
				dataPointsSqlFragmentMap.put(dp.getPath().replace('.', '_'), attributes.get(SortByAttributes.SQL_FRAGMENT_ORDER_BY));
			}
		}
		return parseSortObject(dataPointsSqlFragmentMap, sortObject, strict);
	}

	private List<Sort.Order> parseSortObject(final Map<String, String> sqlFragmentMap, final List<Map<String, String>> sortObject, final boolean strict) {
		final List<Sort.Order> sortOrders = new ArrayList<>();
		for (final Map<String, String> sortSpec : sortObject) {
			for (final Map.Entry<String, String> sortEntry : sortSpec.entrySet()) {
				if ( ! sqlFragmentMap.containsKey(sortEntry.getKey())) {
					if (strict) {
						throw new IllegalArgumentException("While constructing order by clause: Unable to filter by "
								+ sortEntry.getKey() + ": unknown data point or data point is not sortable");
					} else {
						/* In "non-strict" mode, if the key does not name a known datapoint, we assume that the key is a database field name
						 * and pass it through as-is. This is for backwards compatibility with old saved searches where the sort order is stored
						 * using database field names. */
						sortOrders.add(new Sort.Order(Sort.Direction.valueOf(sortEntry.getValue()), sortEntry.getKey()));
					}
				} else {
					sortOrders.add(new Sort.Order(Sort.Direction.valueOf(sortEntry.getValue()), sqlFragmentMap.get(sortEntry.getKey())));
				}
			}
		}
		return sortOrders;
	}

	/**
	 * Applies the sort order defined by the given {@code sortObject} by calling the corresponding sort callback defined on the data point
	 * on the given {@code orderBuilder} instance.
	 *
	 * @param projectId id of the project on which the query is made (for sorting by project-specific data points)
	 * @param queryName name of the GraphQL query that is being executed
	 * @param sortObject the sort object passed to the GraphQL query
	 * @param orderBuilder an order builder (or other object) on which the sort callback defined for the data point will be applied
	 * @param <B> the type of the builder
	 */
	public <B> void applySortObject(final Long projectId, final String queryName, final List<Map<String, String>> sortObject, final B orderBuilder) {
		final String rootTypeName = dataPointRegistry.getQueryDefinitions().get(queryName).getReferenceTypeName();
		for (final Map<String, String> sortSpec : sortObject) {
			for (final Map.Entry<String, String> sortEntry : sortSpec.entrySet()) {
				final Optional<MiningDataPointDefinitionWithPath> dataPoint = dataPointRegistry.getDataPointAtPath(Optional.of(projectId), rootTypeName,
						sortEntry.getKey().replace('_', '.'));
				if (dataPoint.isPresent()) {
					final Optional<MiningDataPointSortCallback<Object>> sortCallback = dataPoint.get().getSortCallback(queryName);
					if ( ! sortCallback.isPresent()) {
						return;
					}
					sortCallback.get().apply(orderBuilder, SortDirection.of(sortEntry.getValue()));
				}
			}
		}
	}

	/**
	 * For each {@linkplain DataPointRegistry#getQueryDefinitions() query definition} inspect all data points recursively and collect data points
	 * that have {@link SortByAttributes#SQL_FRAGMENT_ORDER_BY} defined for the query
	 * (we're looking for usage attributes for usage {@value Usages#GRAPHQL_QUERY_PREFIX} plus the query name).
	 * <p>
	 * Constructs the GraphQL schema for possible sort objects that can be passed to the query. This method returns a map where the map keys
	 * are the names of the types that need to be added to the GraphQL schema as input types. The values of the Map are again Maps where each key
	 * is a sortable data point and the value is the sort direction.
	 * <p>
	 * Note: as a side-effect this method currently mutates the query definition and adds a parameter "sortObject" using the generated schema type.
	 * This is not clean, but there was no suitable other place to put this. For this reason, this method must be synchronized.
	 *
	 * @param projectId id of the project for which the schema is built
	 * @return a Map ( type name -> ( field name -> SortDirection ) )
	 */
	public synchronized Map<String, Map<String, String>> buildSortObjectsSchema(final Optional<Long> projectId) {
		final Map<String, Map<String, String>> inputTypes = new HashMap<>();
		for (final MiningDataPointDefinition queryDef : dataPointRegistry.getQueryDefinitions().values()) {
			final Map<String, String> querySortObject = new HashMap<>();
			final String usageName = Usages.GRAPHQL_QUERY_PREFIX + queryDef.getName();
			final String queryType = queryDef.getReferenceTypeName();
			final List<MiningDataPointDefinitionWithPath> sortableDataPoints = dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(projectId,
					queryType, Collections.singleton(usageName));

			for (final MiningDataPointDefinitionWithPath dp : sortableDataPoints) {
				final Map<String, String> attributes = dp.getUsageAttributes().get(usageName);
				
				if (dp.getSortCallback(queryDef.getName()).isPresent() || attributes != null && attributes.containsKey(SortByAttributes.SQL_FRAGMENT_ORDER_BY)) {
					querySortObject.put(dp.getPath().replace('.', '_'), "SortDirection");
				}
			}
			if ( ! querySortObject.isEmpty()) {
				final String querySortObjectName = "SortObject_" + queryDef.getName();
				inputTypes.put(querySortObjectName, querySortObject);
				queryDef.getParameters().stream()
						.filter(dp -> dp.getName().equals("sortObject"))
						.findFirst()
						.ifPresent(miningDataPointDefinition -> queryDef.getParameters().remove(miningDataPointDefinition));
				queryDef.addParameter(new MiningDataPointDefinition("sortObject", "", querySortObjectName, true, true));
			}
		}

		return inputTypes;
	}
}
