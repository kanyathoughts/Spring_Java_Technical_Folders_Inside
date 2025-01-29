/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.mining.server.cache.MiningCacheConfig.PROJECT_KEY_GENERATOR;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import com.google.gson.reflect.TypeToken;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.SavedSearchPgDao;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SavedSearchService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SavedSearchPojo;
import innowake.mining.shared.entities.SavedSearchPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.SavedSearchCountResponse;
import innowake.mining.shared.model.ScopeEnum;

/**
 * Central point for accessing and modifying saved search entities.
 */
@Service
public class SavedSearchServiceImpl implements SavedSearchService {

	private static final List<String> SAVED_SEARCHES = Arrays.asList("Missing Source Files", "No Taxonomies Assigned", "Not Referenced", "Requires Review", 
			"Taxonomy Assignments", "Business Related DD Entries", "Copybook Defined DD Entries", "Not Referenced DD entries",  "Business Rule Candidates",
			"Database Candidates");
	private static final String ADD_ALL_TAXONOMIES = "ADD_ALL_TAXONOMIES";

	
	private static final String KEY_IN_FILTER = "key";
	private static final String VALUE_IN_FILTER = "value";
	private static final String CONTENT_QUERY_PREFIX = "content_";
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ProjectService projectService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private FilterObjectService filterObjectService;
	@Autowired
	private DataPointRegistry dataPointRegistry;

	private final SavedSearchPgDao dao;

	@Autowired
	public SavedSearchServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		dao = new SavedSearchPgDao(jdbcTemplate);
	}

	@Override
	public Long create(final SavedSearchPojoPrototype savedSearch) {
		final var scope = savedSearch.scope.getNonNull();
		final var usage = savedSearch.usage.getNonNull();
		final var name = savedSearch.name.getNonNull();
		/* project must be present when creating new saved searches */
		final var project = savedSearch.project.getNonNull();
		/* For some reason the old SavedSearchDao never uses the client set in the prototype but loads it for the project */
		final EntityId client = scope == ScopeEnum.CLIENT ? getClientFromProject(project) : null;
		final Optional<SavedSearchPojo> existing = findAny(query -> {
			query.withName(name)
				 .withUsage(usage)
				 .withScope(scope);

			if (scope == ScopeEnum.CLIENT) {
				query.ofClient(client);
			} else {
				query.ofProject(project);
			}
		});

		/* For now we have no unique constraints in Postgres fo this check */
		if (existing.isPresent()) {
			throw new ConstraintViolationException(String.format("Saved search with name %s, scope %s and usage %s already exists", name, scope, usage));
		}

		/* client id must be set only when scope is CLIENT. In that case project id must not be set */
		if (scope == ScopeEnum.CLIENT) {
			savedSearch.setClient(client);
			savedSearch.setProject(null);
		} else {
			savedSearch.setClient(null);
		}

		return dao.put(savedSearch, true);
	}

	@Override
	public void update(final EntityId project, final SavedSearchPojoPrototype savedSearch) {
		final SavedSearchPojo existing = dao.findAny(q -> q.byId(savedSearch.id.getNonNull()))
											.orElseThrow(() -> new MiningEntityNotFoundException(SavedSearchPojo.class, savedSearch.id.getNonNull().toString()));

		final var scope = savedSearch.scope.orElse(existing.getScope());
		if (scope == ScopeEnum.CLIENT) {
			savedSearch.setProject(null);
			savedSearch.setClient(getClientFromProject(savedSearch.project.getNonNull()));
		} else {
			savedSearch.setClient(null);
			if (scope != ScopeEnum.GLOBAL) {
				savedSearch.setProject(project);
			}
		}

		dao.put(savedSearch, false);
	}

	@Override
	public int delete(final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return dao.delete(builder);
	}

	@Override
	public SavedSearchPojo get(final Long id) {
		return dao.findAny(q -> q.byId(id))
					.orElseThrow(() -> new MiningEntityNotFoundException(SavedSearchPojo.class, String.valueOf(id)));
	}

	@Override
	public Paged<SavedSearchPojo> find(final Pagination paging, final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return dao.find(paging, builder);
	}
	
	@Override
	public List<SavedSearchPojo> find(final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return dao.find(builder);
	}

	@Override
	public Optional<SavedSearchPojo> findAny(final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return dao.findAny(builder);
	}

	@Override
	public List<SavedSearchPojo> findByUsageAndUserId(final EntityId projectId, final String usage, final String createdByUserId) {
		final List<SavedSearchPojo> savedSearches = dao.findByUsageAndUserId(getClientFromProject(projectId), projectId, usage, createdByUserId);
		if (savedSearches.isEmpty()) {
			return Collections.emptyList();
		}

		final List<SavedSearchPojo> savedSearchList = new ArrayList<>(savedSearches.size());
		final Long projectNid = projectService.getNid(projectId);
		for (final SavedSearchPojo savedSearch : savedSearches) {
			if (savedSearch.getModifiers().contains(ADD_ALL_TAXONOMIES)) {
				savedSearchList.add(new SavedSearchPojo(savedSearch.getId(),
														savedSearch.getClient().orElse(null),
														null,
														null,
														savedSearch.getProject().orElse(null),
														null,
														null,
														savedSearch.getName(),
														savedSearch.getSavedSearch().concat(changedDataPointsByUsages(projectNid, usage)),
														savedSearch.getScope(),
														savedSearch.getUsage(),
														savedSearch.getModifiers(),
														savedSearch.getCreatedByUserId().orElse(null)));
			} else {
				savedSearchList.add(savedSearch);
			}
		}

		return savedSearchList;
	}

	private String changedDataPointsByUsages(final Long projectId, final String usage) {
		final List<String> usages = new ArrayList<>();
		usages.add(ADD_ALL_TAXONOMIES);
		usages.add(usage);
		final Map<String, MiningDataPointDefinition> taxonomiesDataPoints = dataPointRegistry.getDataPointDefinitionsWithUsage(Optional.of(projectId), usages)
				.getOrDefault("Taxonomies", Collections.emptyMap());
		final StringBuilder addDataPoint = new StringBuilder();
		for (int i = 0; i < taxonomiesDataPoints.size(); i++) {
			addDataPoint.append("&columns=Taxonomies." + taxonomiesDataPoints.keySet().toArray()[i]);
		}

		return addDataPoint.toString();
	}

	@Override
	@Cacheable(cacheNames = "savedSearchAggregation", cacheResolver = "cacheResolver", keyGenerator = PROJECT_KEY_GENERATOR)
	public List<SavedSearchCountResponse> getSavedSearchCounts(@ProjectIdArgument final EntityId projectId) {
		final List<SavedSearchPojo> savedSearchData = find(q -> q.ofProject(EntityId.of(0L)).withNames(SAVED_SEARCHES));
		final List<SavedSearchCountResponse> savedSearchCountResponse = new LinkedList<>();
		for(final SavedSearchPojo data : savedSearchData) {
			final long count = getRecordCountForSavedSearch(data, projectId);
			final SavedSearchCountResponse savedSearchCount = new SavedSearchCountResponse(data, (int) count);
			savedSearchCountResponse.add(savedSearchCount);
		}
		return savedSearchCountResponse;
	}

	private long getRecordCountForSavedSearch(final SavedSearchPojo savedSearch, final EntityId projectId) {
		final String savedSearchString = savedSearch.getSavedSearch().trim();
		final String filter = savedSearchString.substring(savedSearchString.indexOf("filter=") + 7);
		final Map<String, Object> filterObject = convertFiltersToFilterObject(filter);
		switch (savedSearch.getUsage()) {
			case Usages.MINING_UI_MODULES_TABLE:
				return moduleService.countModules(q -> {
					q.ofProject(projectId);
					if (filterObject.containsKey("content_taxonomyCount")) {
						q.includeTaxonomyCount("taxonomyCount", null);
					}
					if (filterObject.containsKey("content_inboundDependencyCount")) {
						q.includeDependencyCount("inboundDependencyCount", RelationshipDirection.IN, null, false);
					}
					filterObjectService.applyFilterObject(projectService.getNid(projectId), MiningGraphQLQueries.MODULES, filterObject, q);
				});
			case Usages.MINING_UI_ANNOTATIONS_TABLE:
				return annotationService.count(q -> {
					q.ofProject(projectId);
					filterObjectService.applyFilterObject(projectService.getNid(projectId), MiningGraphQLQueries.ANNOTATIONS, filterObject, q);
				});
			case Usages.MINING_UI_DATADICTIONARY_TABLE:
				return dataDictionaryService.count(q -> {
					q.ofModuleProject(projectId);
					filterObjectService.applyFilterObject(projectService.getNid(projectId), MiningGraphQLQueries.DATA_DICTIONARY, filterObject, q);
				});
			default:
				return 0;
		}
	}

	/**
	 * Method that converts filters to filter object
	 * 
	 * @param filters  the filters
	 * @return Map<String, Object> a map of filterObject
	 */
	@SuppressWarnings("unchecked")
	/* package private for testability without reflection */ 
	static Map<String, Object> convertFiltersToFilterObject(final String filters) {
		final Map<String, Object> filterObject = new HashMap<>();
		final List<Map<String, Object>> filtersList = new GsonBuilder() /* Convert "1" to Long instead of Double */
															.setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
															.create()
															.fromJson(filters, new TypeToken<List<Map<String, Object>>>() { }.getType());

		try {
			for (final Map<String, Object> filter : filtersList) {
				if ( ! filter.containsKey(KEY_IN_FILTER) && ! filter.containsKey(VALUE_IN_FILTER)) {
					continue;
				}
				final String key = (String) filter.get(KEY_IN_FILTER);
				Object value = filter.get(VALUE_IN_FILTER);
				String operator = null;
				if (value instanceof List) {
					final List<Object> valueList = (List<Object>) value;
					for (final Object val : valueList) {
						if (val instanceof Map) {
							final Map<String, Object> mapVal = (Map<String, Object>) val;
							operator = (String) mapVal.get("operator");
							value = mapVal.get(VALUE_IN_FILTER);
						}
					}
					addFilter(filterObject, key, value, operator, valueList);
				} else {
					filterObject.put(CONTENT_QUERY_PREFIX + StringUtils.replace(key, "\\.", "_"), Map.of("eq", value));
				}
			}
		} catch (final Exception e) {
			throw new IllegalArgumentException("can't convert filters to filterObject: ", e);
		}

		return filterObject;
	}

	private static void addFilter(final Map<String, Object> filterObject, final String key, final Object value, @Nullable final String operator,
			final List<Object> valueList) {
		if (operator == null) {
			if (valueList.size() == 1) {
				if (value.toString().equals("[true]") || value.toString().equals("[false]")) {
					filterObject.put(CONTENT_QUERY_PREFIX + StringUtils.replace(key, ".", "_"), Map.of("is", valueList.get(0)));
				} else {
					filterObject.put(CONTENT_QUERY_PREFIX + StringUtils.replace(key, ".", "_"), Map.of("eq", valueList.get(0)));
				}
			} else {
				filterObject.put(CONTENT_QUERY_PREFIX + StringUtils.replace(key, ".", "_"), Map.of("in", valueList));
			}
		} else {
			switch (operator) {
				case "eq":
				case "is":
				case "gte":
				case "lte":
					filterObject.put(CONTENT_QUERY_PREFIX + StringUtils.replace(key, ".", "_"), Map.of(operator, value));
					break;
				default:
					break;
			}
		}
	}

	private EntityId getClientFromProject(final EntityId project) {
		return projectService.find(project)
							.orElseThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, project.toString()))
							.getClient();
	}
}
