/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import graphql.GraphQLError;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * Tests for {@link DataPointQueryService}.
 */
class DataPointQueryServiceTest extends DatabaseRelatedTest {
	
	private static final Long PROJECT_ID = Long.valueOf(1);

	@Autowired
	DataPointRegistry registry;
	
	@Autowired
	DataPointQueryService service;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	TaxonomyService taxonomyService;

	@Autowired
	ObjectMapper objectMapper;

	@Autowired
	ApplicationEventPublisher eventPublisher;

	@BeforeAll
	@Override
	public void setup() throws IOException {
		super.setup();
		/* ensure that taxonomy-related data points are refreshed for the test project */
		eventPublisher.publishEvent(new TaxonomiesModifiedEvent(PROJECT_ID));
	}

	@Test
	void testQueryDataPoints() {
		final MiningDataPointDefinition queryDef = registry.getQueryDefinitions().get(MiningGraphQLQueries.MODULES);
		final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(Optional.of(PROJECT_ID),
				assertNotNull(queryDef.getReferenceTypeName()));
		
		final MiningDataPointDefinitionWithPath moduleNameDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.name")).findFirst().get();
		final MiningDataPointDefinitionWithPath taxonomyNameDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.taxonomies.name")).findFirst().get();
		/* the "dataDomain" data point is a project-specific data point existing only on project 1 */
		final MiningDataPointDefinitionWithPath dataDomainDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.taxonomy.dataDomain")).findFirst().get();
		
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		parameters.put("sortObject", Map.of("content_id", SortDirection.ASCENDING));
		final DataPointQueryResult result = service.queryDataPoints(Optional.of(PROJECT_ID), MiningGraphQLQueries.MODULES, parameters,
				Arrays.asList(moduleNameDataPoint, taxonomyNameDataPoint, dataDomainDataPoint));
		throwErrorsIfAny(result);
		
		final DataPointSelection moduleNameDataPointResult = result.getDataPoint(moduleNameDataPoint.getPath());
		final DataPointSelection taxonomyNameDataPointResult = result.getDataPoint(taxonomyNameDataPoint.getPath());
		final DataPointSelection dataDomainDataPointResult = result.getDataPoint(dataDomainDataPoint.getPath());
		
		final List<String> names = moduleNameDataPointResult.getValue();
		final List<List<String>> taxonomyNames = taxonomyNameDataPointResult.getValue();
		final List<List<String>> dataDomains = dataDomainDataPointResult.getValue();
		
		/* retrieve modules the traditional way, for comparison */
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(EntityId.of(PROJECT_ID)));
		modules.sort(Comparator.comparing(ModulePojo::getId));
		
		/* assert module names retrieved via DataPointQueryService are the same as when using ModuleDao */
		assertModuleNames(names, modules);

		/* retrieve Taxonomies via TaxonomyService for comparison - creates list of list of taxonomy names per module */
		final List<List<String>> taxonomyNamesFromDao = modules.stream()
				.map(module -> taxonomyService.find(q -> q.ofProject(EntityId.of(PROJECT_ID)).ofModule(module.identity())).stream()
						.map(TaxonomyPojo::getName).collect(Collectors.toList())).collect(Collectors.toList());
		final List<List<String>> dataDomainsFromDao = modules.stream()
				.map(module -> taxonomyService.find(q -> q.ofProject(EntityId.of(PROJECT_ID)).ofModule(module.identity())).stream()
						.filter(taxonomy -> "DataDomain".equals(taxonomy.getType().getName()))
						.map(TaxonomyPojo::getName).collect(Collectors.toList())).collect(Collectors.toList());
		
		/* the DAO results and the data point results differ: if the Module has no Taxonomies, then the data point is null, in DAO it is empty list
		 * so we need to map nulls to empty lists before comparison, maybe this will change after WMIN-3212 */
		assertTaxonomyNames(taxonomyNamesFromDao, taxonomyNames);

		assertEquals(dataDomainsFromDao, assertNotNull(dataDomains).stream()
				.map(dataDomain -> dataDomain == null ? Collections.emptyList() : dataDomain).collect(Collectors.toList()));
	}
	
	@Test
	void testQueryDataPointsById() {
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		parameters.put("sortObject", Map.of("content_name", SortDirection.ASCENDING));
		final DataPointQueryResult result = service.queryDataPointsById(Optional.of(PROJECT_ID), MiningGraphQLQueries.MODULES, parameters, Arrays.asList("Module.name", "Taxonomy.name"));
		throwErrorsIfAny(result);
		
		/* there's no way to get the data point by id from the result yet, so you either have to "know" the path of the data point for now,
		 * or look it up manually from the registry */
		final DataPointSelection moduleNameDataPointResult = result.getDataPoint("content.name");
		final DataPointSelection taxonomyNameDataPointResult = result.getDataPoint("content.taxonomies.name");
		
		final List<String> names = moduleNameDataPointResult.getValue();
		final List<List<String>> taxonomyNames = taxonomyNameDataPointResult.getValue();
		
		/* retrieve modules the traditional way, for comparison */
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(EntityId.of(PROJECT_ID)).sortName(SortDirection.ASCENDING));
		
		/* assert module names retrieved via DataPointQueryService are the same as when using ModuleDao */
		assertModuleNames(names, modules);
		
		/* retrieve Taxonomies via TaxonomyService for comparison - creates list of list of taxonomy names per module */
		final List<List<String>> taxonomyNamesFromDao = modules.stream()
				.map(module -> taxonomyService.find(q -> q.ofProject(EntityId.of(PROJECT_ID)).ofModule(module.identity())).stream()
					.map(TaxonomyPojo::getName).collect(Collectors.toList())).collect(Collectors.toList());
		
		/* the DAO results and the data point results differ: if the Module has no Taxonomies, then the data point is null, in DAO it is empty list
		 * so we need to map nulls to empty lists before comparison, maybe this will change after WMIN-3212 */
		assertTaxonomyNames(taxonomyNamesFromDao, taxonomyNames);
	}
	
	@Test
	void testQueryDataPointsByPath() {
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		parameters.put("sortObject", Map.of("content_name", SortDirection.ASCENDING));
		final DataPointQueryResult result = service.queryDataPointsByPath(MiningGraphQLQueries.MODULES, parameters, Arrays.asList("content.name", "content.taxonomies.name"));
		throwErrorsIfAny(result);
		
		final DataPointSelection moduleNameDataPointResult = result.getDataPoint("content.name");
		final DataPointSelection taxonomyNameDataPointResult = result.getDataPoint("content.taxonomies.name");
		
		final List<String> names = moduleNameDataPointResult.getValue();
		final List<List<String>> taxonomyNames = taxonomyNameDataPointResult.getValue();
		
		/* retrieve modules the traditional way, for comparison */
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(EntityId.of(PROJECT_ID)).sortName(SortDirection.ASCENDING));
		
		/* assert module names retrieved via DataPointQueryService are the same as when using ModuleDao */
		assertModuleNames(names, modules);
		
		/* retrieve Taxonomies via TaxonomyService for comparison - creates list of list of taxonomy names per module */
		final List<List<String>> taxonomyNamesFromDao = modules.stream()
				.map(module -> taxonomyService.find(q -> q.ofProject(EntityId.of(PROJECT_ID)).ofModule(module.identity())).stream()
					.map(TaxonomyPojo::getName).collect(Collectors.toList())).collect(Collectors.toList());
		
		/* the DAO results and the data point results differ: if the Module has no Taxonomies, then the data point is null, in DAO it is empty list
		 * so we need to map nulls to empty lists before comparison, maybe this will change after WMIN-3212 */
		assertTaxonomyNames(taxonomyNamesFromDao, taxonomyNames);
	}
	
	@Test
	void testQueryAlias() {
		final MiningDataPointDefinition queryDef = registry.getQueryDefinitions().get(MiningGraphQLQueries.MODULES);
		final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(assertNotNull(queryDef.getReferenceTypeName()));
		
		final MiningDataPointDefinitionWithPath inboundDependencyCountDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.inboundDependencyCount")).findFirst().get();
		final MiningDataPointDefinitionWithPath outboundDependencyCountDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.outboundDependencyCount")).findFirst().get();
		final MiningDataPointDefinitionWithPath inboundDependencyNamesDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.inboundDependencyNames.module.name")).findFirst().get();
		final MiningDataPointDefinitionWithPath outboundDependencyNamesDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.outboundDependencyNames.module.name")).findFirst().get();
		
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		/* select a program from the test data set which actually has incoming and outgoing dependencies */
		parameters.put("filterObject", Map.of("content_name", Map.of("eq", "MMRS7101")));
		final DataPointQueryResult result = service.queryDataPoints(Optional.of(PROJECT_ID), MiningGraphQLQueries.MODULES, parameters,
				Arrays.asList(inboundDependencyCountDataPoint, outboundDependencyCountDataPoint,
						inboundDependencyNamesDataPoint, outboundDependencyNamesDataPoint));
		throwErrorsIfAny(result);
		
		final DataPointSelection inboundDependencyCountResult = result.getDataPoint(inboundDependencyCountDataPoint.getPath());
		final DataPointSelection outboundDependencyCountResult = result.getDataPoint(outboundDependencyCountDataPoint.getPath());
		final DataPointSelection inboundDependencyNamesResult = result.getDataPoint(inboundDependencyNamesDataPoint.getPath());
		final DataPointSelection outboundDependencyNamesResult = result.getDataPoint(outboundDependencyNamesDataPoint.getPath());
		
		final Long inboundDependencyCount = inboundDependencyCountResult.getValue(0);
		final Long outboundDependencyCount = outboundDependencyCountResult.getValue(0);
		final String inboundDependencyName = inboundDependencyNamesResult.getValue(0, 0);
		final String outboundDependencyName = outboundDependencyNamesResult.getValue(0, 0);
		
		assertEquals(1L, inboundDependencyCount);
		assertEquals(1L, outboundDependencyCount);
		assertEquals("PRG1", inboundDependencyName);
		assertEquals("IDCAMS", outboundDependencyName);
	}
	
	@Test
	void testQueryAliasById() {
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		/* select a program from the test data set which actually has incoming and outgoing dependencies */
		parameters.put("filterObject", Map.of("content_name", Map.of("eq", "MMRS7101")));
		final DataPointQueryResult result = service.queryDataPointsById(Optional.of(PROJECT_ID), MiningGraphQLQueries.MODULES, parameters,
				Arrays.asList("Module.inboundDependencyCount", "Module.outboundDependencyCount",
						"Module.inboundDependencyNames", "Module.outboundDependencyNames"));
		throwErrorsIfAny(result);
		
		/* there's no way to get the data point by id from the result yet, so you either have to "know" the path of the data point for now,
		 * or look it up manually from the registry */
		final DataPointSelection inboundDependencyCountResult = result.getDataPoint("content.inboundDependencyCount");
		final DataPointSelection outboundDependencyCountResult = result.getDataPoint("content.outboundDependencyCount");
		final DataPointSelection inboundDependencyNamesResult = result.getDataPoint("content.inboundDependencyNames.module.name");
		final DataPointSelection outboundDependencyNamesResult = result.getDataPoint("content.outboundDependencyNames.module.name");
		
		final Long inboundDependencyCount = inboundDependencyCountResult.getValue(0);
		final Long outboundDependencyCount = outboundDependencyCountResult.getValue(0);
		final String inboundDependencyName = inboundDependencyNamesResult.getValue(0, 0);
		final String outboundDependencyName = outboundDependencyNamesResult.getValue(0, 0);
		
		assertEquals(1L, inboundDependencyCount);
		assertEquals(1L, outboundDependencyCount);
		assertEquals("PRG1", inboundDependencyName);
		assertEquals("IDCAMS", outboundDependencyName);
	}
	
	@Test
	void testQueryAliasByPath() {
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		/* select a program from the test data set which actually has incoming and outgoing dependencies */
		parameters.put("filterObject", Map.of("content_name", Map.of("eq", "MMRS7101")));
		final DataPointQueryResult result = service.queryDataPointsByPath(MiningGraphQLQueries.MODULES, parameters,
				Arrays.asList("content.inboundDependencyCount", "content.outboundDependencyCount",
						"content.inboundDependencyNames.module.name", "content.outboundDependencyNames.module.name"));
		throwErrorsIfAny(result);
		
		final DataPointSelection inboundDependencyCountResult = result.getDataPoint("content.inboundDependencyCount");
		final DataPointSelection outboundDependencyCountResult = result.getDataPoint("content.outboundDependencyCount");
		final DataPointSelection inboundDependencyNamesResult = result.getDataPoint("content.inboundDependencyNames.module.name");
		final DataPointSelection outboundDependencyNamesResult = result.getDataPoint("content.outboundDependencyNames.module.name");
		
		final Long inboundDependencyCount = inboundDependencyCountResult.getValue(0);
		final Long outboundDependencyCount = outboundDependencyCountResult.getValue(0);
		final String inboundDependencyName = inboundDependencyNamesResult.getValue(0, 0);
		final String outboundDependencyName = outboundDependencyNamesResult.getValue(0, 0);
		
		assertEquals(1L, inboundDependencyCount);
		assertEquals(1L, outboundDependencyCount);
		assertEquals("PRG1", inboundDependencyName);
		assertEquals("IDCAMS", outboundDependencyName);
	}

	@Test
	void testQueryDataPointWithNestedParameter() {
		final var testModule = moduleService.findModules(q -> q.ofProject(EntityId.of(PROJECT_ID)).withName("MMRS7101"));
		assertEquals(1, testModule.size());
		
		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		/* select a program from the test data set which actually has incoming and outgoing dependencies */
		parameters.put("filterObject", "{ \"content_direction\": { \"eq\": \"OUT\" } }");
		parameters.put("moduleId", testModule.get(0).getId());
		
		/* query the "dependencies" data point with direction:OUT by passing the appropriate parameter to content.dependencies data point */
		final DataPointQueryResult result = service.queryDataPointsByPath("moduleDependencies", parameters,
				Arrays.asList("content.targetId", "content.targetName"));
		throwErrorsIfAny(result);

		final String dependency = result.getDataPoint("content.targetName").getValue(0);

		/* if the direction:OUT parameter wasn't applied, then the count would be 2 as the module also has an incoming dependency */
		assertEquals("IDCAMS", assertNotNull(dependency));
	}

	@Test
	void testQueryDataPointsWithFilterObject() throws JsonProcessingException {
		final MiningDataPointDefinition queryDef = registry.getQueryDefinitions().get(MiningGraphQLQueries.MODULES);
		final List<MiningDataPointDefinitionWithPath> dataPoints = registry.getDataPointsForTypeRecursively(assertNotNull(queryDef.getReferenceTypeName()));

		final MiningDataPointDefinitionWithPath moduleNameDataPoint = dataPoints.stream()
				.filter(dp -> dp.getPath().equals("content.name")).findFirst().get();

		final Map<String, Object> filterObject = new HashMap<>();
		final Map<String, Object> filterOperator = new HashMap<>();
		filterOperator.put("eq", "MMRS");
		filterObject.put("content_name", filterOperator);

		final Map<String, Object> parameters = new HashMap<>();
		parameters.put("projectId", PROJECT_ID);
		parameters.put("filterObject", objectMapper.writeValueAsString(filterObject));
		final DataPointQueryResult result = service.queryDataPoints(Optional.of(PROJECT_ID), MiningGraphQLQueries.MODULES, parameters, Arrays.asList(moduleNameDataPoint));
		throwErrorsIfAny(result);

		final DataPointSelection moduleNameDataPointResult = result.getDataPoint(moduleNameDataPoint.getPath());

		final List<String> names = moduleNameDataPointResult.getValue();
		assertNotNull(names, "Expected the query to return module names");
		/* null check to satisfy eclipse compile */
		if (names != null) {
			assertFalse(names.isEmpty(), "Expected the query to find some modules");
			assertTrue(names.stream().allMatch(name -> name.startsWith("MMRS")), "Expected all Module names to start with 'MMRS' as specified by the filter");
		}
	}
	
	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}
	
	private static void throwErrorsIfAny(final DataPointQueryResult rslt) {
		final List<GraphQLError> errors = rslt.getErrors();
		if (! errors.isEmpty()) {
			final IllegalStateException e = new IllegalStateException("Unexpected errors in GraphQL response");
			errors.forEach(err -> e.addSuppressed(new Exception(err.getErrorType() + ": " + err.getMessage())));
			throw e;
		}
	}

	private static void assertModuleNames(@Nullable final List<String> expected, final List<ModulePojo> actual) {
		assertEquals(assertNotNull(expected).stream().sorted().collect(Collectors.toList()),
					 actual.stream().map(ModulePojo::getName).sorted().collect(Collectors.toList()),
					 "Module names must match");
	}

	private static void assertTaxonomyNames(final List<List<String>> expected, @Nullable final List<List<String>> actual) {
		final Function<List<List<String>>, List<List<String>>> transformList = src -> {
			final List<List<String>> result = new ArrayList<>(expected.size());
			for (final List<String> list : src) {
				if (list == null || list.isEmpty()) {
					result.add(0, Collections.emptyList());
				} else {
					result.add(list.stream().sorted().collect(Collectors.toList()));
				}
			}
			return result;
		};

		final var expectedList = transformList.apply(expected);
		final var actualList = transformList.apply(actual);
		Collections.sort(expectedList, (o1, o2) -> Integer.compare(o1.size(), o2.size()));
		Collections.sort(actualList, (o1, o2) -> Integer.compare(o1.size(), o2.size()));
		assertEquals(expectedList, actualList, "Taxonomy names must match");
	}
}
