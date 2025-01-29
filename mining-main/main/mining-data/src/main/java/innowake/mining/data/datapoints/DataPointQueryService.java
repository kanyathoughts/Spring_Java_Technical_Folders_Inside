/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import graphql.ExecutionResult;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.data.graphql.ProjectAwareGraphQlSource;
import innowake.mining.shared.datapoints.definition.AliasDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.ExecutionGraphQlRequest;
import org.springframework.graphql.ExecutionGraphQlService;
import org.springframework.graphql.support.DefaultExecutionGraphQlRequest;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Allows to programmatically issue queries based on datapoints.
 * <p>
 * Basically this service provides programmatic access to the data that can otherwise be retrieved via the GraphQl endpoint.
 * In most cases it is probably more convenient to use the DAOs or Repositories for data retrieval instead. However, this service
 * is useful for querying data based on a list of data point definitions.
 */
@Service
public class DataPointQueryService {
	
	/**
	 * Internal class for turning list of datapoints into GraphQL query.
	 */
	private static class Selection {
		private final String field;
		private final Map<String, Object> parameters = new HashMap<>();
		private final List<Selection> subSelections = new ArrayList<>();
		
		@Nullable
		private final AliasDefinition aliasDefinition;

		public Selection(final String field) {
			this.field = field;
			this.aliasDefinition = null;
		}
		
		public Selection(final String field, @Nullable final AliasDefinition aliasDefinition) {
			this.field = field;
			this.aliasDefinition = aliasDefinition;
		}
	}
	
	/**
	 * Class encapsulating a parameter variable. This is used internally for building the GraphQL query and will be rendered as {@code $name}.
	 */
	private static class ParameterVariable {
		private final String name;

		public ParameterVariable(final String name) {
			this.name = name;
		}
		
		@Override
		public String toString() {
			return "$" + name;
		}
	}

	/**
	 * Internal class for managing parameters passed to data points.
	 * <p>
	 * Unfortunately the parameter handling is quite complex. Let's say the {@code parameters} map
	 * passed to the constructor contains the parameter {@code foo.bar.baz: "a string value"} and
	 * {@code queryName} is "modules". In that case the {@link #getGraphQlQuery(String, QueryParameters, List)}
	 * method would need to build the following construct:
	 * <pre>
	 *     query($foo_bar_baz: String!) {
	 *         modules {
	 *             foo {
	 *                 bar(baz: $foo_bar_baz)
	 *             }
	 *         }
	 *     }
	 * </pre>
	 * So there are a couple of things that need to be taken care of:
	 * <ul>
	 *     <li>we need to build a variable name for the parameter ({@code $foo_bar_baz} in the example)</li>
	 *     <li>we need to determine the parameter type, because that is required for some reason</li>
	 *     <li>we need to parse the "parameter path" ({@code foo.bar}) and parameter name {@code baz} and put it at the correct location in the query</li>
	 * </ul>
	 */
	private class QueryParameters {

		private final String queryName;
		private final MiningDataPointDefinition queryDefinition;
		private final Map<String, Object> parameters;

		/**
		 * Create new QueryParameters object.
		 * @param queryName the name of the root query, e.g. "modules"
		 * @param parameters map of parameters to be passed to the query
		 */
		public QueryParameters(final String queryName, final Map<String, Object> parameters) {
			this.queryName = queryName;
			this.queryDefinition = registry.getQueryDefinitions().get(queryName);
			if (this.queryDefinition == null) {
				throw new IllegalArgumentException("The query " + queryName + " does not exist.");
			}
			this.parameters = parameters;
		}

		/**
		 * Returns all parameter keys that are in the parameters map that was passed to the constructor.
		 * @return the set of parameters
		 */
		private Set<String> getKeys() {
			return parameters.keySet();
		}

		/**
		 * Returns the parameter keys that are applicable to the data point at {@code path}.
		 *
		 * @param path the data point path for which to return the parameter keys
		 * @return a (possibly empty) list of parameter keys
		 */
		private List<String> getKeysForPath(final String path) {
			final List<String> pathSegments = StringUtils.isEmpty(path) ? Collections.emptyList() : Arrays.asList(path.split("\\."));
			return parameters.keySet().stream()
					.filter(key -> {
						final List<String> segments = Arrays.asList(key.split("\\."));
						return (segments.size() <= 1 ? Collections.emptyList() : segments.subList(0, segments.size() - 1)).equals(pathSegments);
					})
					.collect(Collectors.toList());
		}

		/**
		 * Gets the field name of the parameter with the given key. The field name is the last segment of the key.
		 * <p>
		 * Example: calling the method with key {@code "foo.bar.baz"} returns "baz".
		 * @param key the parameter key
		 * @return the field name portion of the key
		 */
		private String getFieldName(final String key) {
			final String[] pathSegments = key.split("\\.");
			return pathSegments[pathSegments.length - 1];
		}

		/**
		 * Returns a {@link ParameterVariable} object that can be used as a placeholder for the parameter inside the query.
		 * <p>
		 * Example: calling the method with key {@code "foo.bar.baz"} will return a {@code ParameterVariable} with name {@code $foo_bar_baz}.
		 * @param key the parameter key
		 * @return a {@link ParameterVariable} object that can be used as a placeholder for the key
		 */
		private ParameterVariable getVariable(final String key) {
			return new ParameterVariable(key.replace('.', '_'));
		}

		private MiningDataPointDefinition getParameterDefinition(final String key) {
			final List<String> pathSegments = Arrays.asList(key.split("\\."));
			final String parameterName = pathSegments.get(pathSegments.size() - 1);
			final String dataPointName;
			final MiningDataPointDefinition dataPoint;
			if (pathSegments.size() > 1) {
				dataPointName = String.join(".", pathSegments.subList(0, pathSegments.size() - 1));
				dataPoint = registry.getDataPointAtPath(Optional.ofNullable(parameters.get("projectId")).map(p -> Long.valueOf(p.toString())),
								queryDefinition.getReferenceTypeName(), dataPointName)
						.orElseThrow(() -> new IllegalArgumentException("Found parameter " + parameterName + " for non-existing data point " + dataPointName));
			} else {
				dataPointName = queryName;
				dataPoint = queryDefinition;
			}

			return dataPoint.getParameters().stream().filter(dp -> dp.getName().equals(parameterName)).findFirst()
					.orElseThrow(() -> new IllegalArgumentException("The parameter '" + parameterName + "' does not exist on the data point '" + dataPointName + "'"));
		}

		/**
		 * Gets the type of the given parameter as it needs to be represented in the schema.
		 *
		 * @param parameterDefinition the {@link MiningDataPointDefinition} of the parameter.
		 * @return the type of the parameter
		 */
		private String getType(final MiningDataPointDefinition parameterDefinition) {
			String typeName = Optional.ofNullable(parameterDefinition.getScalarType()).map(Object::toString)
					.orElse(parameterDefinition.getReferenceTypeName());
			if (parameterDefinition.isArray()) {
				typeName = "[" + typeName + "]";
			}
			if ( ! parameterDefinition.isNullable()) {
				typeName = typeName + "!";
			}
			return typeName;
		}

		/**
		 * Return whether the parameter represented by the given {@link MiningDataPointDefinition} is a complex input object.
		 * <p>
		 * It is a complex input object when it is not a scalar value (except JSON scalar, which is also a complex object) and not an enum.
		 *
		 * @param parameterDefinition the data point definition for the parameter
		 * @return whether the parameter is a complex input object
		 */
		private boolean isInputObject(final MiningDataPointDefinition parameterDefinition) {
			if (parameterDefinition.getReferenceTypeName() != null) {
				return ! registry.getEnumDefinitions().containsKey(parameterDefinition.getReferenceTypeName());
			} else if (parameterDefinition.getScalarType() != null) {
				return parameterDefinition.getScalarType() == MiningDataPointDefinition.ScalarType.JSON;
			}
			return false;
		}

		/**
		 * Returns a map of the query parameters that can be passed to
		 * {@link DefaultExecutionGraphQlRequest#DefaultExecutionGraphQlRequest(String, String, Map, Map, String, Locale)}.
		 * This is the same {@code parameters} map that passed to the constructor,
		 * but the parameter names are replaced with the variable names used inside the query.
		 * <p>
		 * Example: if the original parameter map contained key {@code "foo.bar.baz"} the returned map will contain key {@code "foo_bar_baz"}.
		 * @return parameter map that can be used as request input for
		 * {@link DefaultExecutionGraphQlRequest#DefaultExecutionGraphQlRequest(String, String, Map, Map, String, Locale)}
		 */
		private Map<String, Object> toParameterMap() {
			return parameters.entrySet().stream().collect(Collectors.toMap(entry -> entry.getKey().replace(".", "_"), entry -> {
				final MiningDataPointDefinition parameterDefinition = getParameterDefinition(entry.getKey());
				if (isInputObject(parameterDefinition)) {
					/* parameter is a complex object that needs to be represented as Map */
					try {
						if (entry.getValue() instanceof Map) {
							return entry.getValue();
						} else {
							return objectMapper.readValue(entry.getValue().toString(), Map.class);
						}
					} catch (final JsonProcessingException e) {
						throw new IllegalArgumentException("Invalid value for parameter " + entry.getKey() + ": expected a JSON object", e);
					}
				}
				return entry.getValue();
			}));
		}
	}

	private final ProjectAwareGraphQlSource graphQlSource;
	private final ExecutionGraphQlService graphQlService;
	private final DataPointRegistry registry;
	private final ObjectMapper objectMapper;
	
	@Autowired
	public DataPointQueryService(final ProjectAwareGraphQlSource graphQlSource, final ExecutionGraphQlService graphQlService,
			final DataPointRegistry registry, final ObjectMapper objectMapper) {
		this.graphQlSource = graphQlSource;
		this.graphQlService = graphQlService;
		this.registry = registry;
		this.objectMapper = objectMapper;
	}
	
	/**
	 * Issue a query retrieving data points.
	 * <p>
	 * Valid names of queries and parameters can be retrieved via {@link DataPointRegistry#getQueryDefinitions()}.
	 * <p>
	 * Valid data points for the query can be retrieved with {@link DataPointRegistry#getDataPointsForTypeRecursively(String)}, passing in the return
	 * type of the query.
	 * <p>
	 * Example:
	 * <pre>
	 * // you can get valid query names with registry.getQueryDefinitions().keySet() - here we choose to query "modules"
	 * final String queryName = "modules"
	 * // get the query definition
	 * final MiningDataPointDefinition queryDefinition = registry.getQueryDefinitions().get(queryName);
	 * // get list of valid parameters for the query
	 * final List<MiningDataPointDefinition> parameters = queryDefinition.getParameters();
	 * // get the list of data points that can be retrieved from this query
	 * final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(queryDefinition.getReferenceTypeName());
	 * </pre>
	 *
	 * @param queryName the name of the query - one of the keys of {@link DataPointRegistry#getQueryDefinitions()}
	 * @param parameters parameters and values for the query
	 * @param dataPoints the list of data points to query
	 * @return the result of the query
	 */
	public DataPointQueryResult queryDataPoints(final Optional<Long> projectId, final String queryName,
			final Map<String, Object> parameters, final List<MiningDataPointDefinitionWithPath> dataPoints) {
		/* Ensure the GraphQlSource has been initialized before building the RequestInput -- This is a hack to ensure that all FilterObjects have
		* been properly added to the schema, as they are being added to the schema when the schema is built for the first time, yet their definitions
		* are required to build the RequestInput. */
		graphQlSource.graphQl();

		projectId.ifPresent(graphQlSource::setThreadLocalProjectId);
		try {
			final QueryParameters queryParameters = new QueryParameters(queryName, parameters);
			final ExecutionGraphQlRequest request = new DefaultExecutionGraphQlRequest(getGraphQlQuery(queryName, queryParameters, dataPoints),
					null, queryParameters.toParameterMap(),
					null, UUID.randomUUID().toString(), null);
			final ExecutionResult result = assertNotNull(graphQlService.execute(request).block()).getExecutionResult();

			final Map<String, Map<String, Object>> data = result.getData();
			return new DataPointQueryResult(result.getErrors(), data == null ? Collections.emptyMap() : data.get(queryName));
		} finally {
			graphQlSource.removeThreadLocalProjectId();
		}
	}
	
	public DataPointQueryResult queryDataPointsById(final Optional<Long> projectId, final String queryName, final Map<String, Object> parameters, final List<String> dataPointIds) {
		final String queryType = assertNotNull(registry.getQueryDefinitions().get(queryName).getReferenceTypeName());
		final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(queryType).stream()
				.filter(dp -> dataPointIds.contains(dp.getId()))
				.collect(Collectors.toList());
		
		return queryDataPoints(projectId, queryName, parameters, dataPoints);
	}
	
	public DataPointQueryResult queryDataPointsByPath(final String queryName, final Map<String, Object> parameters, final List<String> dataPointPaths) {
		return queryDataPointsByPath(Optional.empty(), queryName, parameters, dataPointPaths);
	}
	
	public DataPointQueryResult queryDataPointsByPath(final Optional<Long> projectId, final String queryName, final Map<String, Object> parameters, final List<String> dataPointPaths) {
		final List<String> additionalPaths = new ArrayList<>();
		final List<MiningDataPointDefinitionWithPath> dataPoints = new ArrayList<>();
		final String queryType = assertNotNull(registry.getQueryDefinitions().get(queryName).getReferenceTypeName());
		/* Add togetherWith paths to list of dataPoints */
		for (final String path : dataPointPaths) {
			registry.getDataPointAtPath(projectId, queryType, path).ifPresent(dp -> {
				dataPoints.add(dp);
				final Map<String, String> viewModeAttributes = dp.getUsageAttributes().get(Usages.VIEW_MODE);
				if(viewModeAttributes != null && viewModeAttributes.containsKey(ViewModeAttributes.TOGETHER_WITH)) {
					final String parentPath = path.substring(0, path.lastIndexOf('.') + 1); /* We want the dot to be included */
					final String togetherWithPath = parentPath + viewModeAttributes.get(ViewModeAttributes.TOGETHER_WITH);
					additionalPaths.add(togetherWithPath);
				}
			});
		}
		/* Add togetherWith paths to list of dataPoints */
		for (final String path : additionalPaths) {
			registry.getDataPointAtPath(projectId, queryType, path).ifPresent(dataPoints::add);
		}
		return queryDataPoints(projectId, queryName, parameters, dataPoints);
	}
	
	/**
	 * Returns a GraphQL query as string, which is build from the provided query name, parameters and list of data points.
	 *
	 * @param queryName the name of the query, i.e. the name of the property on the top-level Query type in GraphQL
	 * @param parameters the parameters that are to be passed to the query
	 * @param dataPoints the datapoints to select from the query
	 * @return the GraphQL query
	 */
	public String getGraphQlQuery(final String queryName, final QueryParameters parameters, final List<MiningDataPointDefinitionWithPath> dataPoints) {
		final Selection root = new Selection(queryName);
		root.parameters.putAll(parameters.getKeysForPath("").stream().collect(Collectors.toMap(parameters::getFieldName, parameters::getVariable)));
		
		for (final MiningDataPointDefinitionWithPath dataPoint : dataPoints) {
			final List<String> pathSegments = Arrays.asList(dataPoint.getPath().split("\\."));
			final Selection selection = new Selection(dataPoint.getName(), dataPoint.getAliasFor());
			selection.parameters.putAll(parameters.getKeysForPath(dataPoint.getPath()).stream().collect(Collectors.toMap(parameters::getFieldName, parameters::getVariable)));
			putSelectionAtPath(root, selection, Collections.emptyList(), pathSegments, parameters);
		}
		
		final String query = selectionToString(root);
		
		if (parameters.getKeys().isEmpty()) {
			return "query {" + query + "}";
		} else {
			return "query(" + parameterNamesToString(parameters) + ") {" + query + "}";
		}
		
	}
	
	private void putSelectionAtPath(final Selection parent, final Selection selection, final List<String> pathFromRoot, final List<String> path, final QueryParameters parameters) {
		final AliasDefinition aliasDefinition = selection.aliasDefinition;
		/*
		 * If there is only 1 remaining path segment: Put the selection at the current path. The remaining path segment is just the name of the field,
		 * but there are no further sub-selections.
		 * 
		 * If there are more path segments and the selection has an alias definition and the remaining path segments equal
		 * the sub-selection of the alias definition: Put the selection at the current path, the remaining path segments for the alias
		 * sub-selection will be added later by aliasToString().
		 */
		if (path.size() == 1 || aliasDefinition != null && selection.field.equals(path.get(0)) 
				&& aliasDefinition.getSubSelection().equals(String.join(".", path.subList(1, path.size())))) {
			parent.subSelections.add(selection);
		} else {
			final List<String> newPathFromRoot = new ArrayList<>(pathFromRoot);
			newPathFromRoot.add(path.get(0));
			for (final Selection subSelection : parent.subSelections) {
				if (subSelection.field.equals(path.get(0))) {
					putSelectionAtPath(subSelection, selection, newPathFromRoot, path.subList(1, path.size()), parameters);
					return;
				}
			}
			final Selection subSelection = new Selection(path.get(0));
			subSelection.parameters.putAll(parameters.getKeysForPath(String.join(".", newPathFromRoot)).stream().collect(Collectors.toMap(parameters::getFieldName, parameters::getVariable)));
			parent.subSelections.add(subSelection);
			putSelectionAtPath(subSelection, selection, newPathFromRoot, path.subList(1, path.size()), parameters);
		}
	}
	
	private String selectionToString(final Selection selection) {
		final StringBuilder sb = new StringBuilder();
		if (selection.aliasDefinition != null) {
			sb.append(aliasToString(selection.field, selection.aliasDefinition));
		} else {
			sb.append(selection.field);
			sb.append(parametersToString(selection.parameters));
			if ( ! selection.subSelections.isEmpty()) {
				sb.append(" { ");
				sb.append(selection.subSelections.stream().map(this::selectionToString).collect(Collectors.joining(" ")));
				sb.append(" } ");
			}
		}
		return sb.toString();
	}
	
	private String aliasToString(final String field, final AliasDefinition aliasDefinition) {
		final StringBuilder sb = new StringBuilder();
		
		sb.append(field);
		sb.append(": ");
		
		sb.append(aliasDefinition.getAliasFor());
		if ( ! aliasDefinition.getParameters().isEmpty()) {
			sb.append("(");
			sb.append(String.join(", ", aliasDefinition.getParameters()));
			sb.append(")");
		}
		
		final List<String> subSelectionPath = Arrays.asList(aliasDefinition.getSubSelection().split("\\."));
		if ( ! subSelectionPath.isEmpty() && ! subSelectionPath.get(0).isEmpty()) {
			final int openBraces = subSelectionPath.size();
			sb.append(" { ");
			sb.append(String.join(" { ", subSelectionPath));
			for (int i = 0; i < openBraces; i++) {
				sb.append(" }");
			}
		}
		
		return sb.toString();
	}
	
	private String parametersToString(final Map<String, Object> parameters) {
		if (parameters.isEmpty()) {
			return "";
		}
		
		final List<String> paramStrings = new ArrayList<>();
		for (final Map.Entry<String, Object> entry : parameters.entrySet()) {
			if (entry.getValue() instanceof String) {
				/* needs quote */
				paramStrings.add(entry.getKey() + ": \"" + entry.getValue() + "\"");
			} else {
				paramStrings.add(entry.getKey() + ": " + entry.getValue());
			}
		}
		return "(" + String.join(", ", paramStrings) + ")";
	}
	
	private String parameterNamesToString(final QueryParameters parameters) {
		return parameters.getKeys().stream()
				.map(key -> parameters.getVariable(key) + ":" + parameters.getType(parameters.getParameterDefinition(key)))
				.collect(Collectors.joining(","));
	}
}
