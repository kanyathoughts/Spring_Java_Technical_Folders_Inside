/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.mining.server.cache.MiningCacheConfig.PROJECT_KEY_GENERATOR;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.UtilityEntityWithIdAndProperties;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.server.cache.MiningCacheConfig;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Service for aggregating utilities for a given project.
 */
@Service
public class UtilityAggregationService {

	private static final Logger LOG = LoggerFactory.getLogger(UtilityAggregationService.class);

	private final ModuleService moduleService;
	private final ProjectService projectService;

	@Autowired
	public UtilityAggregationService(final ModuleService moduleService, final ProjectService projectService) {
		this.moduleService = moduleService;
		this.projectService = projectService;
	}

	/**
	 * Method to retrieve aggregated values for Utility fields.
	 * 
	 * @param projectId The ID of the Project
	 * @param aggregationRequest The aggregation of requested Utility fields
	 * @return {List<AggregationResult<ModuleFieldName>>} Returns a list of aggregated value for the requested Utility fields and Project Id
	 */
	/* project is required for caching */
	@Cacheable(cacheNames =MiningCacheConfig.UTILITY_AGGREGATION_CACHE, cacheResolver = "cacheResolver", keyGenerator = PROJECT_KEY_GENERATOR)
	public List<AggregationResult<ModuleFieldName>> getAggregatedUtilityValues(@ProjectIdArgument final EntityId projectId, final AggregationRequest<ModuleFieldName> aggregationRequest) {
		final List<AggregationResult<ModuleFieldName>> results = new ArrayList<>();
		final List<UtilityEntityWithIdAndProperties> utilities = getUtilityInvocations(projectId, aggregationRequest);

		/* perform the grouping */
		final Map<Map<ModuleFieldName, Object>, List<UtilityEntityWithIdAndProperties>> groups = utilities.stream().collect(Collectors.groupingBy(utility -> {
			final Map<ModuleFieldName, Object> group = new HashMap<>();

			for (final ModuleFieldName field : aggregationRequest.getGroupBy()) {
				group.put(field, utility.getFieldValue(field));
			}

			return group;
		}));
		
		/* perform the aggregation*/
		for (final Map.Entry<Map<ModuleFieldName, Object>, List<UtilityEntityWithIdAndProperties>> group : groups.entrySet()) {

			final EnumMap<ModuleFieldName, Object> fields = new EnumMap<>(ModuleFieldName.class);
			for (final Map.Entry<ModuleFieldName, AggregationOperator> aggregation : aggregationRequest.getFields().entrySet()) {

				final Object aggregationResult;
				switch (aggregation.getValue()) {
					case COUNT:
						/* in case of count, we disregard the field and just count the number of utilities in the group */
						aggregationResult = Long.valueOf(group.getValue().size());
						break;
					case COUNT_DISTINCT:
						aggregationResult = Long.valueOf(group.getValue().stream()
							.map(utility -> utility.getFieldValue(aggregation.getKey()))
							.distinct()
							.count());
						break;
					case SUM:
						aggregationResult = group.getValue().stream()
							.map(utility -> utility.getFieldValue(aggregation.getKey()))
							.map(Object::toString)
							.filter(StringUtils::isNumeric)
							.collect(Collectors.summingLong(Long::parseLong));
						break;
					default:
						throw new IllegalArgumentException("The aggregation operator " + aggregation.getValue() + " is currently not implemented");
				}
				fields.put(aggregation.getKey(), aggregationResult);
			}

			final AggregationResult<ModuleFieldName> result = new AggregationResult<>();
			result.setGroup(group.getKey());
			result.setFields(fields);
			results.add(result);
		}
		
		return results;
	}

	/**
	 * Returns list of {@link UtilityEntityWithIdAndProperties}.
	 *
	 * @param projectId project id of the module
	 * @param filterObject The filter object
	 * @return list of UtilityEntityWithIdAndProperties dependent on given project
	 */
	private List<UtilityEntityWithIdAndProperties> getUtilityInvocations(final EntityId projectId, final AggregationRequest<ModuleFieldName> aggregationRequest) {
		final UtilityList utilityList;
		try {
			utilityList = UtilityList.loadUtilityList(projectService, projectId);
		} catch (final DiscoveryException e) {
			LOG.error("Unable to load utility definitions for project", e);
			return Collections.emptyList();
		}

		final Map<ModuleFieldName, Map<String, Object>> filterObject = aggregationRequest.getFilterObject();
		filterObject.put(ModuleFieldName.PROJECT_ID, Map.of(FilterOperators.OPERATOR_EQ, projectId));
		filterObject.put(ModuleFieldName.ORIGIN, Map.of(FilterOperators.OPERATOR_EQ, Origin.ENVIRONMENT));

		final Optional<Table> invocations = moduleService.findRelationshipInvocations(filterObject);
		return invocations.map(table -> table.stream().map(row -> {
			final String name = (String) Objects.requireNonNull(row.get("name"));
			final long id = (Long) Objects.requireNonNull(row.get("nid"));
			@SuppressWarnings("unchecked")
			final Map<String, Object> properties = (Map<String, Object>) row.get("properties");

			final Optional<UtilityEntity> entity = utilityList.findUtility(name);

			return entity.map(utilityEntity -> new UtilityEntityWithIdAndProperties(utilityEntity, id, properties))
					.orElseGet(() -> new UtilityEntityWithIdAndProperties(name, id));
		}).collect(Collectors.toList())).orElse(Collections.emptyList());
	}
}
