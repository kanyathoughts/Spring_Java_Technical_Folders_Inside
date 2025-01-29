/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.datapoints.definition.filters.FilterOperators.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallback;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallbacks;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.lucene.LuceneUtil;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;

/**
 * Service for handling filter objects. Filter Objects are passed to GraphQL queries in order to perform searching and filtering.
 * <p>
 * This class has two main responsibilities:
 * <ul>
 *     <li>create OrientClause (WHERE condition) from a filter object</li>
 *     <li>define the schema for filter objects that can be passed to each GraphQL query</li>
 * </ul>
 * The latter is done by inspecting the usage attributes of usage {@value Usages#GRAPHQL_QUERY_PREFIX} for the data points of each GraphQL query.
 */
@Service
public class FilterObjectService {

	private static final String OPERATOR_EQ = "eq";
	private static final String OPERATOR_NOT_EQ = "notEq";
	private static final String OPERATOR_IS = "is";
	private static final String OPERATOR_IS_TRUE = "isTrue";
	private static final String OPERATOR_IS_FALSE = "isFalse";
	private static final String OPERATOR_GTE = "gte";
	private static final String OPERATOR_LTE = "lte";
	private static final String OPERATOR_IN = "in";
	private static final String OPERATOR_NOT_IN = "notIn";

	private static final String AND_CONJUNCTION = "_and";
	private static final String OR_CONJUNCTION = "_or";
	private static final String NOT_CONJUNCTION = "_not";

	private final DataPointRegistry dataPointRegistry;

	/**
	 * Constructs a new filter object service that uses the given data point registry.
	 * The registry is used to query the data points (recursively) that can be provided by a GraphQL query and to find the
	 * various SQL_FRAGMENT attributes.
	 *
	 * @param dataPointRegistry the data point registry
	 * @see SearchFilterAttributes
	 */
	@Autowired
	public FilterObjectService(final DataPointRegistry dataPointRegistry) {
		this.dataPointRegistry = dataPointRegistry;
	}

	/**
	 * Parses the given filter object for the GraphQL query with the given name in the given project.
	 * Returns a tuple containing the filter clause that needs to be used in a WHERE clause to apply the filter, as well as a list of values
	 * that are required by the clause and need to be passed to the database query.
	 *
	 * @param projectId id of the project for which the query is made
	 * @param queryName name of the GraphQL query that is executed (used to query the available data points for the query)
	 * @param filterObject the filter object describing the filters that shall be applied to the query
	 * @return a {@link OrientClause} implementing the requested filters as well as a list of values required by the clause
	 */
	public Pair<OrientClause, List<Object>> parseFilterObject(final Long projectId, final String queryName, final Map<String, Object> filterObject) {
		final String queryType = dataPointRegistry.getQueryDefinitions().get(queryName).getReferenceTypeName();
		final List<MiningDataPointDefinitionWithPath> dataPoints = dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(Optional.of(projectId), queryType,
				Collections.singleton(Usages.GRAPHQL_QUERY_PREFIX + queryName));

		return parseFilterObject(queryName, dataPoints, filterObject);
	}

	/**
	 * Parses the given filter object for the GraphQL query with the given name. This is essentially the same as {@link #parseFilterObject(Long, String, Map)}
	 * but the list of data points can be passed in.
	 *
	 * @param queryName name of the GraphQL query that is executed
	 * @param dataPoints list of filterable data points for the query
	 * @param filterObject the filter object describing the filters that shall be applied to the query
	 * @return a {@link OrientClause} implementing the requested filters as well as a list of values required by the clause
	 */
	public Pair<OrientClause, List<Object>> parseFilterObject(final String queryName, final List<MiningDataPointDefinitionWithPath> dataPoints,
			final Map<String, Object> filterObject) {
		final Map<String, Map<String, String>> dataPointsSqlFragmentsMap = new HashMap<>();
		for (final MiningDataPointDefinitionWithPath dp : dataPoints) {
			final Map<String, String> attributes = dp.getUsageAttributes().get(Usages.GRAPHQL_QUERY_PREFIX + queryName);
			final Map<String, String> sqlFragmentMap = new HashMap<>();

			if (attributes == null || attributes.isEmpty()) {
				continue;
			}
			mapSqlFragmentsToOperators(attributes, sqlFragmentMap);
			dataPointsSqlFragmentsMap.put(dp.getPath().replace('.', '_'), sqlFragmentMap);
		}
		return parseFilterObject(dataPointsSqlFragmentsMap, filterObject);
	}

	private void mapSqlFragmentsToOperators(final Map<String, String> attributes, final Map<String, String> sqlFragmentMap) {
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_EQ)) {
			sqlFragmentMap.put(OPERATOR_EQ, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_EQ));
			sqlFragmentMap.put(OPERATOR_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_TRUE)) {
			sqlFragmentMap.put(OPERATOR_IS_TRUE, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_TRUE));
			sqlFragmentMap.put(OPERATOR_IS_TRUE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_TRUE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_FALSE)) {
			sqlFragmentMap.put(OPERATOR_IS_FALSE, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_FALSE));
			sqlFragmentMap.put(OPERATOR_IS_FALSE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_FALSE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_GTE)) {
			sqlFragmentMap.put(OPERATOR_GTE, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_GTE));
			sqlFragmentMap.put(OPERATOR_GTE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_GTE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_GT)) {
			sqlFragmentMap.put(OPERATOR_GT, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_GT));
			sqlFragmentMap.put(OPERATOR_GT + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_GT + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_LTE)) {
			sqlFragmentMap.put(OPERATOR_LTE, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_LTE));
			sqlFragmentMap.put(OPERATOR_LTE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_LTE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_LT)) {
			sqlFragmentMap.put(OPERATOR_LT, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_LT));
			sqlFragmentMap.put(OPERATOR_LT + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_LT + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_IN)) {
			sqlFragmentMap.put(OPERATOR_IN, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_IN));
			sqlFragmentMap.put(OPERATOR_IN + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_IN + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_NONE)) {
			sqlFragmentMap.put(OPERATOR_IS, attributes.get(SearchFilterAttributes.SQL_FRAGMENT_NONE));
			sqlFragmentMap.put(OPERATOR_IS + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
					attributes.get(SearchFilterAttributes.SQL_FRAGMENT_NONE + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX));
		}
	}

	@SuppressWarnings("unchecked")
	private Pair<OrientClause, List<Object>> parseFilterObject(final Map<String, Map<String, String>> sqlFragmentMap, final Map<String, Object> filterObject) {
		final List<OrientClause> clauses = new ArrayList<>();
		final List<Object> queryArguments = new ArrayList<>();
		for (final Map.Entry<String, Object> filterObjectEntry : filterObject.entrySet()) {
			if (AND_CONJUNCTION.equals(filterObjectEntry.getKey())) {
				/* handle "{ _and: [ filterObjects... ] }" */
				handleAnd(sqlFragmentMap, (List<Map<String, Object>>) filterObjectEntry.getValue(), clauses, queryArguments);
			} else if (OR_CONJUNCTION.equals(filterObjectEntry.getKey())) {
				/* handle "{ _or: [ filterObjects... ] }" */
				handleOr(sqlFragmentMap, (List<Map<String, Object>>) filterObjectEntry.getValue(), clauses, queryArguments);
			} else if (NOT_CONJUNCTION.equals(filterObjectEntry.getKey())) {
				/* handle "{ _not: filterObject }" */
				handleNot(sqlFragmentMap, (Map<String, Object>) filterObjectEntry.getValue(), clauses, queryArguments);
			} else if (sqlFragmentMap.containsKey(filterObjectEntry.getKey())) {
				/* handle "{ dataPointName: { operator: value } }" */
				handleExpression(sqlFragmentMap.get(filterObjectEntry.getKey()), filterObjectEntry.getKey(),
						(Map<String, Object>) filterObjectEntry.getValue(), clauses, queryArguments);
			}
		}
		
		return Pair.of(OrientClause.and(clauses.toArray(new OrientClause[0])), queryArguments);
	}

	private void handleAnd(final Map<String, Map<String, String>> sqlFragmentMap, final List<Map<String, Object>> operands,
				final List<OrientClause> clauses, final List<Object> queryArguments) {
		final List<Pair<OrientClause, List<Object>>> clausesAndArguments = operands.stream()
				.map(op -> parseFilterObject(sqlFragmentMap, op)).collect(Collectors.toList());
		clauses.add(OrientClause.and(clausesAndArguments.stream().map(Pair::getLeft).toArray(OrientClause[]::new)));
		clausesAndArguments.stream().map(Pair::getRight).forEach(queryArguments::addAll);
	}

	private void handleOr(final Map<String, Map<String, String>> sqlFragmentMap, final List<Map<String, Object>> operands,
						final List<OrientClause> clauses, final List<Object> queryArguments) {
		final List<Pair<OrientClause, List<Object>>> clausesAndArguments = operands.stream()
				.map(op -> parseFilterObject(sqlFragmentMap, op)).collect(Collectors.toList());
		clauses.add(OrientClause.or(clausesAndArguments.stream().map(Pair::getLeft).toArray(OrientClause[]::new)));
		clausesAndArguments.stream().map(Pair::getRight).forEach(queryArguments::addAll);
	}

	private void handleNot(final Map<String, Map<String, String>> sqlFragmentMap, final Map<String, Object> operand,
							final List<OrientClause> clauses, final List<Object> queryArguments) {
		final Pair<OrientClause, List<Object>> clauseAndArguments = parseFilterObject(sqlFragmentMap, operand);
		clauses.add(OrientClause.not(clauseAndArguments.getLeft()));
		queryArguments.addAll(clauseAndArguments.getRight());
	}

	private void handleExpression(final Map<String, String> operatorToSqlFragmentMap, final String dataPointName, final Map<String, Object> operatorArgumentMap,
								final List<OrientClause> clauses, final List<Object> queryArguments) {
		for (final Map.Entry<String, Object> operatorArgumentEntry : operatorArgumentMap.entrySet()) {
			if (OPERATOR_IS.equals(operatorArgumentEntry.getKey())) {
				/* special handling: for { is: null }, { is: false }, { is: true } we do not pass the filter argument value to the query,
				 * because anyway separate SQL fragments are used for each case */
				handleBooleanOrAbsentOperator(operatorToSqlFragmentMap, dataPointName, operatorArgumentEntry.getValue(), clauses);
			} else {
				handleOperator(operatorToSqlFragmentMap, operatorArgumentEntry.getKey(), operatorArgumentEntry.getValue(), clauses, queryArguments);
			}
		}
	}

	private void handleBooleanOrAbsentOperator(final Map<String, String> operatorToSqlFragmentMap, final String dataPointName, @Nullable final Object value,
											   final List<OrientClause> clauses) {
		final String realOperatorName;
		if (value == null) {
			/* value == null means we must use SQL_FRAGMENT_NONE for filtering.
			 * SQL_FRAGMENT_NONE is mapped to OPERATOR_IS in mapSqlFragmentsToOperators() */
			realOperatorName = OPERATOR_IS;
		} else {
			final boolean booleanValue = value instanceof Boolean ? ((Boolean) value).booleanValue() : Boolean.parseBoolean(value.toString());
			realOperatorName = booleanValue ? OPERATOR_IS_TRUE : OPERATOR_IS_FALSE;
		}
		if (operatorToSqlFragmentMap.containsKey(realOperatorName)) {
			clauses.add(OrientClause.custom(operatorToSqlFragmentMap.get(realOperatorName)));
		} else {
			throw new IllegalArgumentException("While constructing filter clause for " + dataPointName + ": Operator { is: "
					+ value + " } not supported." + " Use one of: " + operatorToSqlFragmentMap.keySet());
		}
	}

	@SuppressWarnings("null")
	private void handleOperator(final Map<String, String> operatorToSqlFragmentMap, final String operator, final Object value,
								final List<OrientClause> clauses, final List<Object> queryArguments) {
		if (operatorToSqlFragmentMap.containsKey(operator) &&  ! operatorToSqlFragmentMap.get(operator).isEmpty()) {
			
			if (operator.equals(OPERATOR_IN) && hasNull(value)) {
				clauses.add(OrientClause.or(OrientClause.custom(operatorToSqlFragmentMap.get(operator)),
						OrientClause.custom(operatorToSqlFragmentMap.get(OPERATOR_IS))));
			} else {
				clauses.add(OrientClause.custom(operatorToSqlFragmentMap.get(operator)));
			}
			final String flags = operatorToSqlFragmentMap.get(operator + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX);
			if ( ! (operator.equals(OPERATOR_IS) && value == null)) {
				queryArguments.add(handleFlags(value, flags));
			}
		}
	}

	private Object handleFlags(final Object value, @Nullable final String flags) {
		if (flags == null) {
			return value;
		}
		Object ret = value;
		if (flags.contains(SearchFilterAttributes.SQL_FRAGMENT_FLAG_TO_LOWERCASE)) {
			ret = ret.toString().trim().toLowerCase();
		}
		if (flags.contains(SearchFilterAttributes.SQL_FRAGMENT_FLAG_BEGINS_WITH)) {
			if (flags.contains(SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE)) {
				/* for LUCENE searches, '*' is used instead of '%' as the wildcard */
				ret = ret.toString().trim() + '*';
			} else {
				ret = ret.toString().trim() + '%';
			}
		}
		if (flags.contains(SearchFilterAttributes.SQL_FRAGMENT_FLAG_ENDS_WITH)) {
			if (flags.contains(SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE)) {
				/* for LUCENE searches, '*' is used instead of '%' as the wildcard */
				ret = '*' + ret.toString().trim();
			} else {
				ret = '%' + ret.toString().trim();
			}
		}
		if (flags.contains(SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE)) {
			ret = LuceneUtil.escapeSearchTerm(ret.toString().trim(), false);
		}
		return ret;
	}
	
	/**
	 * Applies the filters specified in the given {@code filterObject} by calling the corresponding filter callbacks defined on the data point
	 * on the given {@code inquiryBuilder} instance.
	 *
	 * @param projectId id of the project on which the query is made (for filtering project-specific data points)
	 * @param queryName name of the GraphQL query that is being executed
	 * @param filterObject the filter object passed to the GraphQL query
	 * @param inquiryBuilder an inquiry builder (or other object) on which the filter callbacks defined for the data point will be applied
	 * @param <B> the type of the builder
	 */
	@SuppressWarnings("unchecked")
	public <B> void applyFilterObject(@Nullable final Long projectId, final String queryName, final Map<String, Object> filterObject, final B inquiryBuilder) {
		final String rootTypeName = dataPointRegistry.getQueryDefinitions().get(queryName).getReferenceTypeName();
		for (final Map.Entry<String, Object> filterExpression : filterObject.entrySet()) {
			final Optional<MiningDataPointDefinitionWithPath> dataPoint = dataPointRegistry.getDataPointAtPath(Optional.ofNullable(projectId), rootTypeName,
					filterExpression.getKey().replace('_', '.'));
			if ( ! dataPoint.isPresent() || ! (filterExpression.getValue() instanceof Map)) {
				throw new IllegalArgumentException(
						"While applying filters: Unable to filter by " + filterExpression.getKey() + ": unknown data point or data point is not filterable");
			}
			final MiningDataPointFilterCallbacks filterCallbacks = dataPoint.get().getFilterCallbacks(queryName);
			final Map<String, Object> operatorArgumentMap = (Map<String, Object>) filterExpression.getValue();
			for (final Map.Entry<String, Object> operatorAndArgument : operatorArgumentMap.entrySet()) {
				final Object argument = operatorAndArgument.getValue();
				final Object convertedValue = convertEnumArgument(argument, dataPoint.get());
				switch (operatorAndArgument.getKey()) {
					case OPERATOR_EQ:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getEq());
						break;
					case OPERATOR_NOT_EQ:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getNotEq());
						break;
					case OPERATOR_GTE:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getGte());
						break;
					case OPERATOR_GT:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getGt());
						break;
					case OPERATOR_LTE:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getLte());
						break;
					case OPERATOR_LT:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getLt());
						break;
					case OPERATOR_IN:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getIn());
						break;
					case OPERATOR_NOT_IN:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getNotIn());
						break;
					case OPERATOR_IS:
						invokeCallbackIsVariable(inquiryBuilder, convertedValue, filterCallbacks);
						break;
					case OPERATOR_IS_FALSE:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getIsFalse());
						break;
					case OPERATOR_IS_TRUE:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getIsTrue());
						break;
					case OPERATOR_IS_ABSENT:
						invokeCallback(inquiryBuilder, convertedValue, filterCallbacks.getIsAbsent());
						break;
					default:
						throw new IllegalArgumentException(
								"While applying filters on " + filterExpression.getKey() + ": operator " + operatorAndArgument.getKey() + " is not supported");
				}
			}
		}
	}

	@SuppressWarnings({ "rawtypes" })
	private Object convertEnumArgument(final Object argument,
			final MiningDataPointDefinitionWithPath miningDataPointDefinitionWithPath) {
		final MiningEnumDefinition miningEnumDefinition = dataPointRegistry.getEnumDefinitions()
				.get(miningDataPointDefinitionWithPath.getReferenceTypeName());
		if (miningEnumDefinition == null) {
			//Not an Enum Type or it's not defined
			return argument;
		}
		final Class<Enum> enumClass = Objects.requireNonNull(miningEnumDefinition.getRepresentedBy(), "Enum class in MiningEnumDefinition must not be null");
		if (argument instanceof String) {
			return enumValueOf(enumClass, (String) argument);
		}
		if (argument instanceof Collection) {
			final List<Enum> result = new LinkedList<>();
			for (final Object o : ((Collection) argument)) {
				if (o instanceof String) {
					result.add(enumValueOf(enumClass, (String) o));
				} else {
					throw new IllegalStateException("Unhandled value / type: " + o + " for enum class: " + enumClass + " on enumDefinition: " + miningEnumDefinition);
				}
			}
			return result;
		}

		throw new IllegalStateException("Undefined argument " + argument + " or datapoint: " + miningEnumDefinition);
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	private static Enum enumValueOf(final Class<Enum> enumClass, final String value) {
		try {
			return Enum.valueOf(enumClass, value);
		} catch (final IllegalArgumentException e) {
			/* Fallback for DefinedLocation.COPYBOOK("Copybook") => return COPYBOOK for "Copybook" */
			try {
				return Enum.valueOf(enumClass, value.toUpperCase());
			} catch (final IllegalArgumentException e2) {
				throw e;
			}
		}
	}

	/**
	 * For each {@linkplain DataPointRegistry#getQueryDefinitions() query definition} inspect all data points recursively and collect data points
	 * that have SQL_FRAGMENTS defined for the query (we're looking for usage attributes for usage {@value Usages#GRAPHQL_QUERY_PREFIX} plus the query name).
	 * <p>
	 * Constructs the GraphQL schema for possible filter objects that can be passed to the query, taking into account all filterable data points,
	 * the SQL_FRAGMENTS that are available for the data point and the data point's type. This method returns a map where the map keys
	 * are the names of the types that need to be added to the GraphQL schema as input types. The values of the Map are again Maps describing each field
	 * of each input type and its type.
	 * <p>
	 * Note: as a side-effect this method currently mutates the query definition and adds a parameter "filterObject" using the generated schema type.
	 * This is not clean, but there was no suitable other place to put this. For this reason, this method must be synchronized.
	 *
	 * @param projectId id of the project for which the schema is built
	 * @return a Map ( type name -> ( field name -> field type ) )
	 */
	public synchronized Map<String, Map<String, String>> buildFilterObjectsSchema(final Optional<Long> projectId) {
		final Map<String, Map<String, String>> inputTypes = new HashMap<>();
		for (final MiningDataPointDefinition queryDef : dataPointRegistry.getQueryDefinitions().values()) {
			final Map<String, String> queryFilterObject = new HashMap<>();
			final String usageName = Usages.GRAPHQL_QUERY_PREFIX + queryDef.getName();
			final String queryType = queryDef.getReferenceTypeName();
			final List<MiningDataPointDefinitionWithPath> filterableDataPoints = dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(projectId,
					queryType, Collections.singleton(usageName));

			/* might be expensive, might want to only with debug mode (maybe logging flag?) */
			verifyAllDatapointUsagesAreInEffect(projectId, usageName, filterableDataPoints, queryType);

			for (final MiningDataPointDefinitionWithPath dp : filterableDataPoints) {
				generateFilterObjectSchema(dp, usageName, queryFilterObject, queryDef, inputTypes);
			}
			if ( ! queryFilterObject.isEmpty()) {
				final String queryFilterObjectName = "FilterObject_" + queryDef.getName();
				queryFilterObject.put(AND_CONJUNCTION, "[" + queryFilterObjectName + "]");
				queryFilterObject.put(OR_CONJUNCTION, "[" + queryFilterObjectName + "]");
				queryFilterObject.put(NOT_CONJUNCTION, queryFilterObjectName);
				inputTypes.put(queryFilterObjectName, queryFilterObject);
				queryDef.getParameters().stream()
						.filter(dp -> "filterObject".equals(dp.getName()))
						.findFirst()
						.ifPresent(miningDataPointDefinition -> queryDef.getParameters().remove(miningDataPointDefinition));
				queryDef.addParameter(new MiningDataPointDefinition("filterObject", "", queryFilterObjectName, false, true));
			}
		}
		return inputTypes;
	}

	private void verifyAllDatapointUsagesAreInEffect(final Optional<Long> projectId, final String usageName, final List<MiningDataPointDefinitionWithPath> filterableDataPoints, @Nullable final String queryType) {
		final Map<String, Map<String, MiningDataPointDefinition>> filterableDataPointsPerType = dataPointRegistry.getDataPointDefinitionsWithUsage(projectId, Collections.singleton(usageName));
		final long datapointsWithThatUsage = filterableDataPointsPerType.values().stream().map(Map::values).flatMap(Collection::stream).count();
		if (filterableDataPoints.size() != datapointsWithThatUsage) {
			for (MiningDataPointDefinitionWithPath dp : filterableDataPoints) {
				final var parentTypeName = dp.getParentTypeName();
				final Map<String, MiningDataPointDefinition> parentTypeMap = filterableDataPointsPerType.get(parentTypeName);
				if (parentTypeMap == null) {
					throw new IllegalStateException("I don't know how this could happen, your lucky day, you can analyze it!");
				}
				final var dpd = parentTypeMap.get(dp.getName());
				if (dpd == null) {
					throw new IllegalStateException("I don't know how this could happen, your lucky day, you can analyze it!");
				}
				parentTypeMap.remove(dp.getName());
				if (parentTypeMap.isEmpty()) {
					filterableDataPointsPerType.remove(parentTypeName);
				}
			}

			var ls = System.lineSeparator();
			throw new IllegalStateException("Datapoint not registered correctly. You won't be able to use: " + filterableDataPointsPerType + ". " + ls
					+ "    Make sure to register the foreign data point accordingly. " + ls
					+ "    Your extension of another source's DataPoint won't occur in the GraphQl Schema, " + ls
					+ "    because theres no link from " + queryType + " to " + filterableDataPointsPerType + ". " + ls
					+ "    This can have two causes:" + ls
					+ "    1. " + ls
					+ "        You did not register the link as @SchemaMapping method." + ls
					+ "        If e.g. StatementDataPointSource wants to link to module.name, the StatementGraphQlController " + ls
					+ "        has to map a StatementPojo to a ModulePojo. (done via @SchemaMapping... public ModulePojo module(StatementPojo...)" + ls
					+ "    or 2. " + ls
					+ "        You linked and registered a datapoint with the same name, " + ls
					+ "        e.g. builder.extend(MiningEnitityNames.STATEMENT, \"module\") " + ls
					+ "        and defined a Method " + ls
					+ "        @SchemaMapping... public ModulePojo module(StatementPojo... " + ls
					+ "        Please use the id of the extended entity directly, " + ls
					+ "        e.g. builder.extend(MiningEnitityNames.MODULE, \"id\")");
		}
	}

	private void generateFilterObjectSchema(final MiningDataPointDefinitionWithPath filterableDataPoint, final String usageName,
			final Map<String, String> queryFilterObject, final MiningDataPointDefinition queryDef, final Map<String, Map<String, String>> inputTypes) {
		final String typeName = getFilterArgumentTypeName(filterableDataPoint, queryDef.getName());
		final Map<String, String> filterObject = new HashMap<>();
		final Map<String, String> attributes = Optional.ofNullable(filterableDataPoint.getUsageAttributes().get(usageName)).orElse(Collections.emptyMap());
		final MiningDataPointFilterCallbacks filterCallbacks = filterableDataPoint.getFilterCallbacks(queryDef.getName());
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_EQ) || filterCallbacks.getEq().isPresent()) {
			filterObject.put(OPERATOR_EQ, typeName);
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_TRUE) || attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_FALSE) ||
				attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_NONE) || filterCallbacks.getIsFalse().isPresent()
				|| filterCallbacks.getIsTrue().isPresent() || filterCallbacks.getIsAbsent().isPresent()) {
			filterObject.put(OPERATOR_IS, typeName);
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_GTE) || filterCallbacks.getGte().isPresent()) {
			filterObject.put(OPERATOR_GTE, typeName);
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_GT) || filterCallbacks.getGt().isPresent()) {
			filterObject.put(OPERATOR_GT, typeName);
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_LTE) || filterCallbacks.getLte().isPresent()) {
			filterObject.put(OPERATOR_LTE, typeName);
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_LT) || filterCallbacks.getLt().isPresent()) {
			filterObject.put(OPERATOR_LT, typeName);
		}
		if (attributes.containsKey(SearchFilterAttributes.SQL_FRAGMENT_IN) || filterCallbacks.getIn().isPresent()) {
			filterObject.put(OPERATOR_IN,  "[" + typeName + "]");
		}
		if (filterCallbacks.getNotEq().isPresent()) {
			filterObject.put(OPERATOR_NOT_EQ, typeName);
		}
		if (filterCallbacks.getNotIn().isPresent()) {
			filterObject.put(OPERATOR_NOT_IN, "[" + typeName + "]");
		}

		if ( ! filterObject.isEmpty()) {
			final String filterObjectName = "FilterObject_" + queryDef.getName() + "_" + filterableDataPoint.getPath().replace('.', '_');
			inputTypes.put(filterObjectName, filterObject);
			queryFilterObject.put(filterableDataPoint.getPath().replace('.', '_'), filterObjectName);
		}
	}

	private String getFilterArgumentTypeName(final MiningDataPointDefinitionWithPath filterableDataPoint, final String queryName) {
		final MiningDataPointFilterCallbacks filterCallbacks = filterableDataPoint.getFilterCallbacks(queryName);
		final ScalarType scalarTypeOverride = filterCallbacks.getScalarType();
		final String referenceTypeNameOverride = filterCallbacks.getReferenceTypeName();
		final ScalarType scalarType = filterableDataPoint.getScalarType();
		if (scalarTypeOverride != null) {
			return scalarTypeOverride.toString();
		} else if (referenceTypeNameOverride != null) {
			return referenceTypeNameOverride;
		} else if (scalarType != null) {
			return scalarType.toString();
		} else {
			return assertNotNull(filterableDataPoint.getReferenceTypeName());
		}
	}

	private <B> void invokeCallback(final B inquiryBuilder,
			final Object argument,
			final Optional<MiningDataPointFilterCallback<B, Object>> callback) {
		if (callback.isEmpty()) {
			return;
		}
		callback.get().apply(inquiryBuilder, argument);
	}

	@SuppressWarnings({"null", "unused"})
	private <B> void invokeCallbackIsVariable(final B inquiryBuilder, final Object argument, final MiningDataPointFilterCallbacks filterCallbacks) {
		if (argument == null) {
			invokeCallback(inquiryBuilder, argument, filterCallbacks.getIsAbsent());
		} else if (argument.equals(Boolean.FALSE)) {
			invokeCallback(inquiryBuilder, argument, filterCallbacks.getIsFalse());
		} else if (argument.equals(Boolean.TRUE)) {
			invokeCallback(inquiryBuilder, argument, filterCallbacks.getIsTrue());
		} else if(argument.toString().isEmpty()) {
			invokeCallback(inquiryBuilder, argument, filterCallbacks.getEq());
		}
	}
	
	private boolean hasNull(final Object value) {
		if (value instanceof ArrayList) {
			final ArrayList<?> arrayList = (ArrayList<?>) value;
			if (arrayList.contains(null)) {
				return true;
			}
		}
		return false;
	}
}
