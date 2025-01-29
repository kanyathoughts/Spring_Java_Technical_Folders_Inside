/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.persistence;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.entities.StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY;
import static innowake.mining.shared.entities.StatementPojo.PROPERTY_KEY_DISTINCT_TABLES;
import static innowake.mining.shared.entities.StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY;
import static innowake.mining.shared.entities.StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY;
import static innowake.mining.shared.entities.StatementPojo.PROPERTY_KEY_SQL_LENGTH;
import static innowake.mining.shared.entities.StatementPojo.PROPERTY_KEY_TABLES;
import static innowake.mining.shared.entities.testing.ModulePojoDummy.newModuleDefinition;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.ImportResult;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModuleDeadCodePojo;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.hashing.LinkHash;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Test class to test OrientDiscoveryPersistence methods
 */
class DiscoveryPersistenceTest extends DatabaseRelatedTest {

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private DiscoveryPersistenceImpl discoveryPersistence;

	@Autowired
	private ClientService clientService;

	private EntityId projectId = EntityId.of(-1L);
	
	private ModulePojo test1Module = ModulePojoDummy.build(new ModulePojoPrototype());
	private ModulePojo test2Module = test1Module;
	private ModulePojo test3Module = test1Module;
	private ModulePojo test4Module = test1Module;
	private ModulePojo test5Module = test1Module;
	private ModulePojo test6Module = test1Module;
	private ModulePojo test7Module = test1Module;
	private ModulePojo test8Module = test1Module;
	private ModulePojo test9Module = test1Module;
	private ModulePojo test10Module = test1Module;
	private ModulePojo test11Module = test1Module;

	@Nullable
	private DiscoveryContext context;

	@BeforeAll
	void insertData() {
		final ClientPojo client = clientService.get(EntityId.of(Long.valueOf(1)), true);
		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setNatures(Collections.emptySet())
				.setClient(client.identity())
			).identity();

		final String content = "THERE SHOULD BE COBOL IN HERE";
		final String contentHash = CityHash.EMPTY_CONTENT_HASH;
		final SourcePojo testSource = new SourcePojo(UUID.randomUUID(), -1l, projectId, null, null,
				"TEST1", "/src/cobol/programs/TEST1.cbl", Technology.COBOL, Type.PROGRAM, Long.valueOf(1),
				Long.valueOf(1), new BinaryValue(contentHash), new BinaryString(content), CustomPropertiesMap.empty());
		context = new DiscoveryTestContext(Collections.singletonList(testSource), projectId);
		
		test1Module = createCobolModule("Test 1", "/src/cobol/programs/Test1.cbl");
		test2Module = createTestModule("Test 2", "/src/ecl/programs/Test2.job", ModuleType.ECL_JOB);
		test3Module = createTestModule("Test 3", "/src/ecl/programs/Test3.job", ModuleType.ECL_JOB);
		test4Module = createTestModule("Test 4", "/src/java/programs/Test4.java", ModuleType.JAVA_COMPILATION_UNIT);
		test5Module = createTestModule("Test 5", "/src/java/programs/Test5.java", ModuleType.JAVA_COMPILATION_UNIT);
		test6Module = createTestModule("Test 6", "/src/pl/programs/Test6.pl1", ModuleType.PL1_PROGRAM);
		test7Module = createCobolModule("Test 7", "/src/cobol/programs/Test7.cbl");
		test8Module  = createCobolModule("Test 8", "/src/cobol/programs/Test8.cbl");
		test9Module = createTestModule("Test 9", "/src/c/programs/Test9.c", ModuleType.C_PROGRAM);
		test10Module  = createCobolModule("Test 10", "/src/cobol/programs/Test10.cbl", Date.from(Instant.parse("2007-12-03T10:15:30.00Z")));
		test11Module = createTestModule("TEST_SCHEMA_1", null, ModuleType.SQL_SCHEMA);

		final ModuleLocation location = new ModuleLocation(10, 100);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setSrcModule(test1Module.identity())
				.setSrcLocation(location)
				.setDstModule(test7Module.identity())
				.setRelationship(RelationshipType.CONTAINS));
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setSrcModule(test1Module.identity())
				.setSrcLocation(location)
				.setDstModule(test8Module.identity())
				.setRelationship(RelationshipType.CONTAINS));
	}

	@Test
	void testFindModuleById() {
		final ModuleFilter modulefilter = new ModuleFilter();
		modulefilter.setModuleIds(test3Module.identity());
		modulefilter.setNames("Test 5");
		final List<EntityId> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter);
		assertEquals(1, findModules.size()); // Since Id is given it omits the other filter properties.
		assertEquals(test3Module.identity(), findModules.get(0));
	}

	@Test
	void testFindModuleByName() {
		final ModuleFilter modulefilter = new ModuleFilter();
		final Set<String> names = new HashSet<String>(Arrays.asList("Test 1", "Test 5"));
		modulefilter.setNames(names);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(2, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test5Module.getId(), findModules.get(1));
	}

	@Test
	void testFindModuleByModuleType() {
		ModuleFilter modulefilter = new ModuleFilter();
		Set<ModuleType> types = new HashSet<ModuleType>(Arrays.asList(ModuleType.COBOL_PROGRAM, ModuleType.JAVA_COMPILATION_UNIT));
		modulefilter.setTypes(types);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(12, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test4Module.getId(), findModules.get(1));
		assertEquals(test5Module.getId(), findModules.get(2));
		assertEquals(test7Module.getId(), findModules.get(3));
		assertEquals(test8Module.getId(), findModules.get(4));
	}

	@Test
	void testFindModuleByPath() {
		ModuleFilter modulefilter = new ModuleFilter();
		Set<String> paths = new HashSet<String>(Arrays.asList("/src/cobol/programs/Test1.cbl", "/src/ecl/programs/Test2.job"));
		modulefilter.setPaths(paths);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		/* like in the IoDao.findByNamePathTechnologyAndType() we also have to include the path of the parent module when searching for path matches */
		assertEquals(4, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test2Module.getId(), findModules.get(1));
	}

	@Test
	void testFindModuleByRepresentation() {
		final ModuleFilter modulefilter = new ModuleFilter().setPhysical(true);
		final Set<String> names = new HashSet<>(Arrays.asList("Test 7", "Test 8", "TEST_SCHEMA_1"));
		modulefilter.setNames(names);
		final List<Long> modules = discoveryPersistence.findModules(assertNotNull(context), modulefilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(2, modules.size());
		Collections.sort(modules);
		assertEquals(test7Module.getId(), modules.get(0));
		assertEquals(test8Module.getId(), modules.get(1));
	}

	@Test
	void testFindModuleByPathPattern() {
		ModuleFilter modulefilter = new ModuleFilter();
		Set<String> pathPatterns = new HashSet<String>(Arrays.asList("/src/cobol/programs/*.cbl"));
		modulefilter.setPathPatterns(pathPatterns);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(5, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test7Module.getId(), findModules.get(1));
		assertEquals(test8Module.getId(), findModules.get(2));
	}

	@Test
	void testFindModuleByCombinedFilter() {
		final ModuleFilter modulefilter = new ModuleFilter();
		final Set<String> names = new HashSet<String>(
				Arrays.asList("Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Test 6", "Test 7", "Test 8"));
		modulefilter.setNames(names);
		Set<ModuleType> types = new HashSet<ModuleType>(Arrays.asList(ModuleType.ECL_JOB, ModuleType.PL1_PROGRAM, ModuleType.JAVA_COMPILATION_UNIT));
		modulefilter.setTypes(types);
		Set<String> paths = new HashSet<String>(
				Arrays.asList("/src/ecl/programs/Test2.job", "/src/ecl/programs/Test3.job", "/src/java/programs/Test4.java"));
		modulefilter.setPaths(paths);
		Set<String> pathPatterns = new HashSet<String>(Arrays.asList("/src/ecl/programs/*.job"));
		modulefilter.setPathPatterns(pathPatterns);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(2, findModules.size());
		findModules.containsAll(findModules);
		Collections.sort(findModules);
		assertEquals(test2Module.getId(), findModules.get(0));
		assertEquals(test3Module.getId(), findModules.get(1));
	}

	@Test
	void testFindModuleByCombinedFilter2() {
		ModuleFilter modulefilter = new ModuleFilter();
		Set<String> names = new HashSet<String>(
				Arrays.asList("Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Test 6", "Test 7", "Test 8"));
		modulefilter.setNames(names);
		Set<ModuleType> types = new HashSet<ModuleType>(Arrays.asList(ModuleType.ECL_JOB, ModuleType.PL1_PROGRAM, ModuleType.JAVA_COMPILATION_UNIT));
		modulefilter.setTypes(types);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(5, findModules.size());
		Collections.sort(findModules);
		assertEquals(test2Module.getId(), findModules.get(0));
		assertEquals(test3Module.getId(), findModules.get(1));
		assertEquals(test4Module.getId(), findModules.get(2));
		assertEquals(test5Module.getId(), findModules.get(3));
		assertEquals(test6Module.getId(), findModules.get(4));
	}

	@Test
	void testFindModuleByCombinedFilter3() {
		ModuleFilter modulefilter = new ModuleFilter();
		Set<String> names = new HashSet<String>(
				Arrays.asList("Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Test 6", "Test 7", "Test 8"));
		modulefilter.setNames(names);
		Set<ModuleType> types = new HashSet<ModuleType>(
				Arrays.asList(ModuleType.COBOL_PROGRAM, ModuleType.PL1_PROGRAM, ModuleType.JAVA_COMPILATION_UNIT));
		modulefilter.setTypes(types);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(6, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test4Module.getId(), findModules.get(1));
		assertEquals(test5Module.getId(), findModules.get(2));
		assertEquals(test6Module.getId(), findModules.get(3));
		assertEquals(test7Module.getId(), findModules.get(4));
		assertEquals(test8Module.getId(), findModules.get(5));
	}

	@Test
	void testFindModuleByCombinedFilter4() {
		final ModuleFilter modulefilter = new ModuleFilter();
		final Set<String> names = new HashSet<String>(
				Arrays.asList("Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Test 6", "Test 7", "Test 8"));
		modulefilter.setNames(names);
		final Set<String> pathPatterns = new HashSet<String>(Arrays.asList("/src/cobol/**/*.cbl", "/src/pl/programs/*.pl1"));
		modulefilter.setPathPatterns(pathPatterns);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(4, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test6Module.getId(), findModules.get(1));
		assertEquals(test7Module.getId(), findModules.get(2));
		assertEquals(test8Module.getId(), findModules.get(3));
	}
	
	@Test
	void testFindModuleByInContainsModule() {
		final ModuleFilter modulefilter = new ModuleFilter();
		final Set<String> names = new HashSet<String>(Arrays.asList("Test 1", "Test 7"));
		modulefilter.setNames(names);

		final ModuleFilter containedInModuleFilter = new ModuleFilter();
		Set<String> names2 = new HashSet<String>(Arrays.asList("Test 1")); // parent module
		containedInModuleFilter.setNames(names2);

		modulefilter.setContainedIn(containedInModuleFilter);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(1, findModules.size());
		Collections.sort(findModules);
		assertEquals(test7Module.getId(), findModules.get(0));
	}
	
	@Test
	void testFindModuleByInContainsModule2() {
		final ModuleFilter modulefilter = new ModuleFilter();
		final Set<String> names = new HashSet<String>(Arrays.asList("Test 1", "Test 2", "Test 7", "Test 8"));
		modulefilter.setNames(names);

		final ModuleFilter containedInModuleFilter = new ModuleFilter();
		final Set<String> names2 = new HashSet<String>(Arrays.asList("Test 1")); // parent module
		containedInModuleFilter.setNames(names2);
		modulefilter.setContainedIn(containedInModuleFilter);

		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter)
				.stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(2, findModules.size());
		Collections.sort(findModules);
		assertEquals(test7Module.getId(), findModules.get(0));
		assertEquals(test8Module.getId(), findModules.get(1));
	}
	
	@Test
	void testFindModuleByMutlipleModuleIds() {
		final Set<EntityId> expectedModuleIds = Sets.newHashSet(test1Module.identity(), test2Module.identity(), test3Module.identity());
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(expectedModuleIds);
		final var actualModuleIds = discoveryPersistence.findModules(assertNotNull(context), moduleFilter);
		assertEquals(expectedModuleIds.size(), actualModuleIds.size());
		assertTrue(actualModuleIds.containsAll(expectedModuleIds));
	}
	
	@Test
	void testFindModuleByMutlipleModuleIdsWithNotIds() {
		final var expectedModuleIds = Sets.newHashSet(test1Module.identity(), test2Module.identity(), test3Module.identity(), test4Module.identity(), test5Module.identity());
		final var notIds = Sets.newHashSet(test4Module.identity(), test5Module.identity());
		final ModuleFilter notFilter = new ModuleFilter().setModuleIds(notIds);
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(expectedModuleIds).setNot(notFilter);
		final var actualModuleIds = discoveryPersistence.findModules(assertNotNull(context), moduleFilter,
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		assertEquals(3, actualModuleIds.size());
		expectedModuleIds.removeAll(notIds);
		assertTrue(actualModuleIds.containsAll(expectedModuleIds));
	}
	
	@Test
	void testFindModuleByNamesWithNotIds() {
		final Set<String> names = Sets.newHashSet("Test 1", "Test 2", "Test 3", "Test 4");
		final Set<EntityId> notIds = Sets.newHashSet(test3Module.identity(), test4Module.identity());
		final ModuleFilter notFilter = new ModuleFilter().setModuleIds(notIds);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames(names).setNot(notFilter);
		final List<EntityId> actualModuleIds = discoveryPersistence.findModules(assertNotNull(context), moduleFilter);
		assertEquals(2, actualModuleIds.size());
		assertTrue(actualModuleIds.containsAll(Sets.newHashSet(test1Module.identity(), test2Module.identity())));
	}
	
	@Test
	void testFindModuleByContainedInWithNotIds() {
		final Set<EntityId> expectedModuleIds = Sets.newHashSet(test7Module.identity(), test8Module.identity());
		final Set<EntityId> notIds = Sets.newHashSet(test8Module.identity());
		final ModuleFilter notFilter = new ModuleFilter().setModuleIds(notIds);
		/* Here TEST 1 contains both Test7 and Test 8, after filtering out test8, we should only get test7 */
		final ModuleFilter containedInModuleFilter = new ModuleFilter().setNames(Collections.singleton("Test 1"));
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(expectedModuleIds).setNot(notFilter).setContainedIn(containedInModuleFilter);
		final List<EntityId> actualModuleIds = discoveryPersistence.findModules(assertNotNull(context), moduleFilter,
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		assertEquals(1, actualModuleIds.size());
		assertEquals(test7Module.identity(), actualModuleIds.get(0));
	}
	
	@Test
	void testFindModuleByMetricsDate() {
		final ModuleFilter modulefilter = new ModuleFilter().setMetricsDate(test10Module.getMetricsDate().orElseThrow());
		final List<EntityId> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		assertEquals(1, findModules.size());
		assertEquals(test10Module.identity(), findModules.get(0));
	}

	@Test
	void testFindParentModuleIds() {
		final var moduleFilter1 = new ModuleFilter().setModuleIds(List.of(test7Module.identity()));
		final List<EntityId> findModules1 = discoveryPersistence.findModules(assertNotNull(context), moduleFilter1, ResolutionFlag.RESOLVE_CASE_INSENSITIVE,
				 ResolutionFlag.RESOLVE_TO_PARENT);
		assertEquals(1, findModules1.size());
		assertEquals(test1Module.identity(), findModules1.get(0));
		final var moduleFilter2 = new ModuleFilter().setNames("TEST 8");
		final List<EntityId> findModules2 = discoveryPersistence.findModules(assertNotNull(context), moduleFilter2, ResolutionFlag.RESOLVE_CASE_INSENSITIVE,
				ResolutionFlag.RESOLVE_TO_PARENT);
		assertEquals(1, findModules2.size());
		assertEquals(test1Module.identity(), findModules2.get(0));
	}
	
	@Test
	void testPersistInsertModuleThrowsAmbigousResult() {
		final String ambigous = "Ambigous1";
		createCobolModule(ambigous, "/src/cobol/programs/AM1.cbl");
		createCobolModule(ambigous, "/src/cobol/programs/AM2.cbl");
		final ModuleFilter moduleFilter = new ModuleFilter().setNames(ambigous).setPaths("/src/cobol/programs/AM1.cbl", "/src/cobol/programs/AM2.cbl")
				.setTypes(ModuleType.COBOL_PROGRAM);
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("Module1", "cobol/programs/Module107.cbl", Collections.emptyList());
		final ImportResult<EntityId> expectedResult = ImportResult.forAmbiguousMatch("More than one Module found for module filter");
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);

		assertEqualsImportResult(expectedResult, actualResult);
	}
	
	@Test 
	void testPersistWithMultipleContainedInModuleThrowsAmbigousResult() {
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(test1Module.identity());
		/* The modules with these names are created in #insertData */
		final ModuleFilter containedInModuleFilter = new ModuleFilter().setNames("Test 3", "Test 4");
		moduleFilter.setContainedIn(containedInModuleFilter);
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("Module1", "cobol/programs/Module107.cbl", Collections.emptyList());
		final ImportResult<EntityId> expectedResult = ImportResult.forAmbiguousMatch("More than one containedIn Module found");
	    final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
	    		moduleDefinition);
	   
	    assertEqualsImportResult(expectedResult, actualResult);
	}
	
	@Test 
	void testPersistWithContainedInModuleNotFoundThrowsAmbigousResult() {		
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(test1Module.identity());
		final ModuleFilter containedInModuleFilter = new ModuleFilter().setNames("NonExistingModule");
		moduleFilter.setContainedIn(containedInModuleFilter);
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("Module1", "cobol/programs/Module107.cbl", Collections.emptyList());
		final ImportResult<EntityId> expectedResult = ImportResult.forAmbiguousMatch("No containedIn Module found for containedIn filter");
	    final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
	    		moduleDefinition);
	    
	    assertEqualsImportResult(expectedResult, actualResult);
	}
	
	@Test
	void testPersistModuleForDbError() {
		final ModuleFilter moduleFilter =  new ModuleFilter().setNames("Module1");
		final ModulePojoPrototype moduleDefinition = newModuleDefinition(null, ModuleType.COBOL_PROGRAM, 
				new ModuleLocation(1,1), null, Storage.FILE, Representation.PHYSICAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ImportResult<EntityId> expectedResult = ImportResult.forDbError("Error occured while persisting Module", new Throwable());
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		assertEquals(expectedResult.getStatus(), actualResult.getStatus());
		assertTrue(actualResult.getMessage().get().contains(expectedResult.getMessage().get()));
	}
	
	@Test
	void testPersistModuleInsertion() {
		final ModuleFilter moduleFilter =  new ModuleFilter().setNames("Module1");
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("Module1", "cobol/programs/Module107.cbl", Collections.emptyList());
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("Module1"));
		assertEquals(1, actualModule.size());
		assertEqualsModule(moduleDefinition, actualModule.get(0));
	}

	@Test
	void testPersistModuleUpdation() {
		createCobolModule("Module106", "/src/cobol/programs/Module106.cbl");
		final ModuleFilter moduleFilter =  new ModuleFilter().setNames("Module106");
		final ModulePojoPrototype moduleDefinition = newModuleDefinition("updatedModule106", ModuleType.NATURAL_FUNCTION, 
				new ModuleLocation(1,1), "/src/natural/programs/updatedModule106.cbl", Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.MISSING, Origin.ENVIRONMENT);
		
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("updatedModule106"));
		assertEquals(1, actualModule.size());
		assertEqualsModule(moduleDefinition, actualModule.get(0));
	}
	
	@Test
	void testPersistModuleUpdationWithEmptyModuleDefinition() {
		final ModuleFilter moduleFilter =  new ModuleFilter().setNames("Test 1");
		final ModulePojoPrototype moduleDefinition = newModuleDefinition(null, null, null, null, null, null, Collections.emptyList(), null, null);
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("Test 1"));
		assertEquals(1, actualModule.size());
		/* This confirms the module is not updated when ModuleDefinition has no values for any fields*/
		assertEquals(test1Module.getModifiedDate(), actualModule.get(0).getModifiedDate());
	}

	@Test
	void testPersistModuleUpdationWhenModuleDefinitionHasValue() {
		final ModuleFilter moduleFilter =  new ModuleFilter().setNames("Test 1");
		final ModulePojoPrototype moduleDefinition = newModuleDefinition(null, null, null, null, Storage.DATABASE, null, Collections.emptyList(), null, null);
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("Test 1"));
		assertEquals(1, actualModule.size());
		/* This confirms the module is updated when ModuleDefinition has value for atleast one field */
		assertNotEquals(test1Module.getModifiedDate(), actualModule.get(0).getModifiedDate());
	}

	@Test
	void testPersistModulePartialUpdation() {
		createCobolModule("Module107", "/src/cobol/programs/Module107.cbl");
		final ModulePojoPrototype moduleDefinition = newModuleDefinition("updatedTest107", null, 
				new ModuleLocation(15, 20), null, null, Representation.PHYSICAL,
				Collections.emptyList(), Identification.MISSING, null);
		
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context),
				new ModuleFilter().setNames("Module107"), moduleDefinition);
		
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("updatedTest107"));
		assertEquals(1, actualModule.size());
		assertEquals("/src/cobol/programs/Module107.cbl", actualModule.get(0).getPath().orElseThrow());
		assertEquals(Technology.COBOL, actualModule.get(0).getTechnology());
		assertEquals(Type.PROGRAM, actualModule.get(0).getType());
		assertEquals("updatedTest107", actualModule.get(0).getName());	
		assertEquals(Identification.MISSING, actualModule.get(0).getIdentification());
		assertEquals(Origin.CUSTOM, actualModule.get(0).getOrigin());
		assertEquals(Storage.FILE, actualModule.get(0).getStorage());
		assertEquals(Representation.PHYSICAL, actualModule.get(0).getRepresentation().orElseThrow());
		assertEquals(15, actualModule.get(0).getLocation().orElseThrow().getOffset());
		assertEquals(20, actualModule.get(0).getLocation().orElseThrow().getLength());
	}
	
	@Test
	void testPersistModuleInsertionWithContainsInModule() {
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("Module11");
		moduleFilter.setContainedIn( new ModuleFilter().setModuleIds(test7Module.identity()));
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("Module11", "cobol/programs/MODULE100.cbl", Collections.emptyList());
		
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("Module11"));
		assertEquals(1, actualModule.size());
		assertEqualsModule(moduleDefinition, actualModule.get(0));
		final List<ModuleRelationshipPojo> refs = moduleService.findRelationship(q -> q.ofSource(test7Module.identity())
																					.ofDestination(actualModule.get(0).identity())
																					.withType(RelationshipType.CONTAINS));
		/* As we are using optimized edge persistence for inserting containsInModule, which have to query the edge table to for checking the creation of it*/
		assertFalse(refs.isEmpty());
	}
	
	/**
	 *  This test is to make sure that containsInModule is not inserted again for already created Module.
	 */
	@Test
	void testPersistModuleUpdationWithContainsInModule() {
		final EntityId id1 = createTestModule("Contains1", "/src/ecl/programs/Contains1.job", ModuleType.ECL_JOB).identity();
		final EntityId id2 = createTestModule("Contains2", "/src/ecl/programs/Contains2.job", ModuleType.ECL_JOB).identity();
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setSrcModule(id1)
				.setSrcLocation(new ModuleLocation(10, 20))
				.setDstModule(id2)
				.setRelationship(RelationshipType.CONTAINS));
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("Contains2");
		final ModuleFilter containedInModuleFilter = new ModuleFilter().setNames("Contains1");
		moduleFilter.setContainedIn(containedInModuleFilter);
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("Contains2", "cobol/programs/MODULE101.cbl", Collections.emptyList());
		
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("Contains2"));
		assertEquals(1, actualModule.size());
		assertEqualsModule(moduleDefinition, actualModule.get(0));
		assertEquals(id1, actualModule.get(0).getParent().get());
	}
	
	@Test 
	void testPersistModuleInsertionWithAddidtionalInfo() {
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("Module102");
		final SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype();
		sourceMetrics.setCodeLines(Integer.valueOf(1101));
		sourceMetrics.setCommentLines(Integer.valueOf(99));
		sourceMetrics.setPhysicalLines(Integer.valueOf(130));
		sourceMetrics.setComplexityMcCabe(Integer.valueOf(2));
		sourceMetrics.setDeadCodeLines(Integer.valueOf(3));
								
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("ModuleUpdated102", "cobol/programs/MODULE102.cbl",
				Collections.singleton(sourceMetrics));
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		assertEqualsImportResult(expectedResult, actualResult);
		
		final List<ModulePojo> result = moduleService.findModules(b -> b.ofProject(projectId).withName("ModuleUpdated102"));
		assertEquals(1, result.size());
		final ModulePojo actualModule = result.get(0);
		final SourceMetricsPojo actualSourceMetrics = actualModule.getSourceMetrics().orElseThrow();
		assertEqualsModule(moduleDefinition, actualModule);
		assertEquals(sourceMetrics.codeLines.get(), actualSourceMetrics.getCodeLines());
		assertEquals(sourceMetrics.complexityMcCabe.get(), actualSourceMetrics.getComplexityMcCabe());
		assertEquals(sourceMetrics.commentLines.get(), actualSourceMetrics.getCommentLines());
		assertEquals(sourceMetrics.physicalLines.get(), actualSourceMetrics.getPhysicalLines());
		assertEquals(sourceMetrics.deadCodeLines.get(), actualSourceMetrics.getDeadCodeLines());
	}
	
	@Test 
	void testPersistModuleUpdationWithAdditionalInfo() {
		final ModulePojoPrototype cobolProgram = createModule("MODULEADDINFO", null);
		cobolProgram.setTechnology(ModuleType.COBOL_PROGRAM.getTechnology());
		cobolProgram.setType(ModuleType.COBOL_PROGRAM.getType());
		cobolProgram.setCreator(Creator.DISCOVERY);
		SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype();
		sourceMetrics.setCodeLines(Integer.valueOf(100));
		sourceMetrics.setCodeLines(Integer.valueOf(12));
		sourceMetrics.setComplexityMcCabe(Integer.valueOf(3));
		cobolProgram.setSourceMetrics(sourceMetrics);
		final EntityId id = moduleService.create(cobolProgram); 
		
		final SourceMetricsPojoPrototype sourceMetricsV2 = new SourceMetricsPojoPrototype();
		sourceMetricsV2.setCodeLines(Integer.valueOf(211));
		sourceMetricsV2.setCommentLines(Integer.valueOf(99));
		sourceMetricsV2.setPhysicalLines(Integer.valueOf(130));
		sourceMetricsV2.setComplexityMcCabe(Integer.valueOf(2));
		sourceMetricsV2.setDeadCodeLines(Integer.valueOf(3));
		
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(id);
		final ModulePojoPrototype moduleDefinition = createModuleDefinition("ModuleAddInfoUpdated", "cobol/programs/MODULE103.cbl",
				Collections.singleton(sourceMetricsV2));
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		assertEqualsImportResult(expectedResult, actualResult);
		final ModulePojo actualModule = moduleService.getModule(id);
		assertEqualsModule(moduleDefinition, actualModule);
		final SourceMetricsPojo actualSourceMetrics = assertNotNull(actualModule.getSourceMetrics().orElseThrow());
		assertEquals(sourceMetricsV2.codeLines.get(), assertNotNull(actualSourceMetrics).getCodeLines() );
		assertEquals(sourceMetricsV2.complexityMcCabe.get(), assertNotNull(actualSourceMetrics).getComplexityMcCabe());
		assertEquals(sourceMetricsV2.commentLines.get(), assertNotNull(actualSourceMetrics).getCommentLines());
		assertEquals(sourceMetricsV2.physicalLines.get(), assertNotNull(actualSourceMetrics).getPhysicalLines());
		assertEquals(sourceMetricsV2.deadCodeLines.get(), assertNotNull(actualSourceMetrics).getDeadCodeLines() );
	}
	
	@Test
	void testPersistModuleForUtility() {
		final ModuleFilter moduleFilter =  new ModuleFilter().setNames("IKJEFT");
		final ModulePojoPrototype moduleDefinition = newModuleDefinition("IKJEFT", ModuleType.UNKNOWN, 
				new ModuleLocation(1,1), null, Storage.FILE, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.ENVIRONMENT);
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter,
				moduleDefinition);
		
		assertEqualsImportResult(expectedResult, actualResult);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("IKJEFT"));
		assertEquals(1, actualModule.size());
		assertEquals(Origin.ENVIRONMENT, actualModule.get(0).getOrigin());
	}
	
	@Test
    void testPersistStatement() {
    	final StatementPojoPrototype statementDefinition = createStatementDefinition(StatementType.CALL, "CALL PGM", 
    			Collections.emptyMap());
    	final ImportResult<EntityId> expectedImportResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistStatements(assertNotNull(context),
    			test4Module.identity(), ModulePojoDummy.build(test4Module), Collections.singletonList(statementDefinition));
    	assertEquals(1, actualImportResult.size());
    	assertEqualsImportResult(expectedImportResult, actualImportResult.get(0));
    	final List<StatementPojo> actualStatement = moduleService.findStatements(q -> q.ofModule(test4Module.identity()).notWithTechnology(Technology.SQL));
    	assertEquals(1, actualStatement.size());
    	assertEquals("CALL PGM", actualStatement.get(0).getText());
    	assertEquals(StatementType.CALL, actualStatement.get(0).getType());
    }
    
    @Test
    void testPersistSqlStatement() {
    	final Map<String, Object> expectedSqlAdditionalInfo = new HashMap<>();
    	expectedSqlAdditionalInfo.put(PROPERTY_KEY_CUSTOM_COMPLEXITY, Integer.valueOf(1));
    	expectedSqlAdditionalInfo.put(PROPERTY_KEY_HALSTEAD_COMPLEXITY, Double.valueOf(6));
    	expectedSqlAdditionalInfo.put(PROPERTY_KEY_DISTINCT_TABLES, Integer.valueOf(2));
    	expectedSqlAdditionalInfo.put(PROPERTY_KEY_HALSTEAD_DIFFICULTY, Double.valueOf(3));
    	expectedSqlAdditionalInfo.put(PROPERTY_KEY_SQL_LENGTH, Integer.valueOf(23));
    	expectedSqlAdditionalInfo.put(PROPERTY_KEY_TABLES, Integer.valueOf(6));
    	final StatementPojoPrototype statementDefinition = createStatementDefinition(StatementType.CREATE_TABLE, "CREATE TABLE SAMPLE", 
    			expectedSqlAdditionalInfo);
    	
    	final ImportResult<EntityId> expectedImportResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistStatements(assertNotNull(context),
    			test6Module.identity(), ModulePojoDummy.build(test6Module), Collections.singletonList(statementDefinition));
    	
    	assertEquals(1, actualImportResult.size()); 
    	assertEqualsImportResult(expectedImportResult, actualImportResult.get(0));
    	final List<StatementPojo> actualSqlStatement = moduleService.findStatements(q -> q.ofModule(test6Module.identity()).withTechnology(Technology.SQL));
    	assertEquals(1, actualSqlStatement.size());
    	assertEquals("CREATE TABLE SAMPLE", actualSqlStatement.get(0).getText());
    	assertEquals(StatementType.CREATE_TABLE, actualSqlStatement.get(0).getType());

    	final String[] keys = {PROPERTY_KEY_CUSTOM_COMPLEXITY, PROPERTY_KEY_HALSTEAD_COMPLEXITY, PROPERTY_KEY_HALSTEAD_DIFFICULTY,
    							PROPERTY_KEY_DISTINCT_TABLES, PROPERTY_KEY_SQL_LENGTH, PROPERTY_KEY_TABLES };
    	
    	final Map<String, Object> properties = Objects.requireNonNull(actualSqlStatement.get(0).getProperties(), "Statement properties must not be null");
    	for (final String key : keys) {
			assertEquals(expectedSqlAdditionalInfo.get(key), properties.get(key), "Sql property: " + key + " must match.");
    	}
    }
    
    @Test
    void testPersistBatchStatements() {
    	/* we are not setting any property to sqlInvalidAdditionalInfo to trigger database error */
    	final Map<String, Object> sqlInvalidAdditionalInfo = new HashMap<>();
    	sqlInvalidAdditionalInfo.put(PROPERTY_KEY_TABLES, null);

    	final Map<String, Object> sqlAdditionalInfo = new HashMap<>();
    	sqlAdditionalInfo.put(PROPERTY_KEY_CUSTOM_COMPLEXITY, Integer.valueOf(10));
    	sqlAdditionalInfo.put(PROPERTY_KEY_HALSTEAD_COMPLEXITY, Float.valueOf(6));
    	sqlAdditionalInfo.put(PROPERTY_KEY_DISTINCT_TABLES, Integer.valueOf(2));
    	sqlAdditionalInfo.put(PROPERTY_KEY_HALSTEAD_DIFFICULTY, Float.valueOf(3));
    	sqlAdditionalInfo.put(PROPERTY_KEY_SQL_LENGTH, Integer.valueOf(23));
    	sqlAdditionalInfo.put(PROPERTY_KEY_TABLES, Integer.valueOf(6));
    	final StatementPojoPrototype statementDefinition1 = createStatementDefinition(StatementType.CREATE_TABLE, "CREATE TABLE SAMPLE", 
    			sqlInvalidAdditionalInfo);
    	final StatementPojoPrototype statementDefinition2 = createStatementDefinition(StatementType.CALL, "CALL PGM", 
    			Collections.emptyMap());
    	final StatementPojoPrototype statementDefinition3 = createStatementDefinition(StatementType.DISPLAY, "DISPLAY PGM", 
    			Collections.emptyMap());
    	final StatementPojoPrototype statementDefinition4 = createStatementDefinition(StatementType.DISPLAY, "DISPLAY PGM1", 
    			Collections.emptyMap());
    	final StatementPojoPrototype statementDefinition5 = createStatementDefinition(StatementType.CREATE_TABLE, "CREATE TABLE SAMPLE", 
    			sqlAdditionalInfo);
    	final ImportResult<EntityId> expectedSucessImportResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
    	final ImportResult<EntityId> expectedErrorImportResult = ImportResult.forDbError("Database Error occured while persisting Statement", new Throwable());
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistStatements(assertNotNull(context),
    			test8Module.identity(), ModulePojoDummy.build(test8Module), 
    			Arrays.asList(statementDefinition1, statementDefinition2, statementDefinition3, statementDefinition4, statementDefinition5));
    
    	assertEquals(2, actualImportResult.size()); 
    	assertTrue(actualImportResult.get(0).getMessage().get().contains(expectedErrorImportResult.getMessage().get()));
    	assertEquals(expectedErrorImportResult.getStatus(), actualImportResult.get(0).getStatus());
    	assertEqualsImportResult(expectedSucessImportResult, actualImportResult.get(1));
    	final List<StatementPojo> actualStatement = moduleService.findStatements(q -> q.ofModule(test8Module.identity()));
    	assertEquals(3, actualStatement.stream().filter(s -> s.getTechnology() != Technology.SQL).collect(Collectors.toList()).size());
    	assertEquals(1, actualStatement.stream().filter(s -> s.getTechnology() == Technology.SQL).collect(Collectors.toList()).size());
    }
    
    @Test
    void testPersistStatementsForDbError() {
    	final StatementPojoPrototype statementDefinition1 = createStatementDefinition(StatementType.CALL, "CALL PGM", 
    			Collections.emptyMap());
    	final StatementPojoPrototype statementDefinition2 = createStatementDefinition(StatementType.DISPLAY, "DISPLAY PGM", 
    			Collections.emptyMap());
    	final ImportResult<EntityId> expectedErrorImportResult = ImportResult.forDbError("Database Error occured while persisting Statement", new Throwable());
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistStatements(assertNotNull(context),
    			EntityId.of(-1L), new ModulePojoPrototype(), Arrays.asList(statementDefinition1, statementDefinition2));
    
    	assertEquals(3, actualImportResult.size()); 
    	assertTrue(actualImportResult.get(0).getMessage().get().contains(expectedErrorImportResult.getMessage().get()));
    	assertEquals(expectedErrorImportResult.getStatus(), actualImportResult.get(0).getStatus());
    	assertTrue(actualImportResult.get(1).getMessage().get().contains(expectedErrorImportResult.getMessage().get()));
    	assertEquals(expectedErrorImportResult.getStatus(), actualImportResult.get(1).getStatus());
    	final List<StatementPojo> actualStatement = moduleService.findStatements(q -> q.ofModule(EntityId.of(-1L)));
    	assertEquals(0, actualStatement.size());
    }
    
	@Test
	void testFetchModules() {		
		final List<EntityId> expectedModuleIdList = Arrays.asList(test1Module.identity(), test2Module.identity(), test3Module.identity(), test4Module.identity());
		final Set<EntityId> actualModuleIdList = discoveryPersistence.fetchModules(assertNotNull(context), expectedModuleIdList)
				.stream()
				.map(ModulePojo::identity)
				.collect(Collectors.toSet());
		assertEquals(expectedModuleIdList.size(), actualModuleIdList.size());
		assertTrue(actualModuleIdList.containsAll(expectedModuleIdList), "ModuleList contains wrong module");
	}
	
    @Test
    void testPersistDeadCode() {
    	final ModuleDeadCodePojoPrototype deadCode = newModelDeadCode("sampleName", 5, 10);
    	final ImportResult<EntityId> expectedImportResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistDeadCode(assertNotNull(context), test5Module.identity(),
    			Collections.singletonList(deadCode));
    	assertEquals(1, actualImportResult.size());
    	assertEqualsImportResult(expectedImportResult, actualImportResult.get(0));
    	final List<ModuleDeadCodePojo> actualDeadCode = moduleService.findDeadCode(q -> q.ofModule(test5Module.identity()));
    	assertEquals(1, actualDeadCode.size());
    	assertEqualsDeadCode(deadCode, actualDeadCode.get(0));
    }
    
	@Test
    void testPersistBatchDeadCode() {
    	final ModuleDeadCodePojoPrototype deadCode1 = newModelDeadCode("sampleName", 5, 10);
    	final ModuleDeadCodePojoPrototype deadCode2 = newModelDeadCode("sampleName2", 10, 15);
    	final ModuleDeadCodePojoPrototype deadCode3 = newModelDeadCode("sampleName3", 15, 20);
    	final ImportResult<EntityId> expectedImportResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistDeadCode(assertNotNull(context), test3Module.identity(),
    			Arrays.asList(deadCode1, deadCode2, deadCode3));
    	
    	assertEquals(1, actualImportResult.size());
    	assertEqualsImportResult(expectedImportResult, actualImportResult.get(0));
    	final List<ModuleDeadCodePojo> actualDeadCode = moduleService.findDeadCode(q -> q.ofModule(test3Module.identity()));
    	assertEquals(3, actualDeadCode.size());
    	actualDeadCode.sort(Comparator.comparing(ModuleDeadCodePojo::getDeadCode));
    	assertEqualsDeadCode(deadCode1, actualDeadCode.get(0));
    	assertEqualsDeadCode(deadCode2, actualDeadCode.get(1));
    	assertEqualsDeadCode(deadCode3, actualDeadCode.get(2));
    }
    
    @Test
    void testPersistDeadCodeErrorScenario() {
    	final ModuleDeadCodePojoPrototype deadCode1 = newModelDeadCode("sampleName1", 5, 10);
    	final ModuleDeadCodePojoPrototype deadCode2 = newModelDeadCode("sampleName2", 15, 20);
    	final ImportResult<EntityId> expectedErrorImportResult = ImportResult.forDbError("Error occured while persisting deadCodes ",
    			new Throwable());
    	final List<ImportResult<EntityId>> actualImportResult = discoveryPersistence.persistDeadCode(assertNotNull(context),
    			EntityId.of(-1L),
    			Arrays.asList(deadCode1, deadCode2));
    	
    	assertEquals(1, actualImportResult.size());
    	assertTrue(actualImportResult.get(0).getMessage().get().contains(expectedErrorImportResult.getMessage().get()));
    	assertEquals(expectedErrorImportResult.getStatus(),  actualImportResult.get(0).getStatus());
    	final List<ModuleDeadCodePojo> actualDeadCode = moduleService.findDeadCode(q -> q.ofModule(EntityId.of(-1L)));
    	assertEquals(0, actualDeadCode.size());
    }
    
	@Test
	void testPersistErrors() {
		final ErrorMarker firstErrorMarker = new ErrorMarker( Severity.ERROR, ErrorKey.PARSE_ERROR, "ERROR WHILE PARSING", null);
		final ErrorMarker secondErrorMarker = new ErrorMarker( Severity.ERROR, ErrorKey.EMPTY_FILE, "FILE IS EMPTY", null);
		final ImportResult<EntityId> expectedImportResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final List<ImportResult<EntityId>> actualImportResults = discoveryPersistence.persistErrors(assertNotNull(context), test9Module.identity(),
				Arrays.asList(firstErrorMarker, secondErrorMarker));

		assertEquals(1, actualImportResults.size());
		actualImportResults.forEach(actualImportResult -> assertEqualsImportResult(expectedImportResult, actualImportResult));
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(test9Module.identity()));
		assertEquals(2, errors.size());
		errors.sort(Comparator.comparing(ErrorMarkerPojo::getCause));
		assertEqualsErrorMarkers(firstErrorMarker, errors.get(0));
		assertEqualsErrorMarkers(secondErrorMarker, errors.get(1));
	}	
	
	@Test
	void testPersistErrorsOnErrorScenario() {
		final ErrorMarker firstErrorMarker = new ErrorMarker( Severity.ERROR, ErrorKey.PARSE_ERROR, "ERROR WHILE PARSING", null);
		final ErrorMarker secondErrorMarker = new ErrorMarker( Severity.ERROR, ErrorKey.EMPTY_FILE, "FILE IS EMPTY", null);
		final DiscoveryTestContext testContext = new DiscoveryTestContext(Collections.emptyList(), EntityId.of(-1L));
		final ImportResult<EntityId> expectedImportResult = ImportResult.forDbError("Error occured while persisting Error", new Throwable());
		final List<ImportResult<EntityId>> actualImportResults = discoveryPersistence.persistErrors(testContext, EntityId.of(-1L),
				Arrays.asList(firstErrorMarker, secondErrorMarker));
		assertEquals(1, actualImportResults.size());
		actualImportResults.forEach(actualImportResult -> {
		    assertEquals(expectedImportResult.getStatus(), actualImportResult.getStatus());
		    assertTrue(actualImportResult.getMessage().get().contains(expectedImportResult.getMessage().get()));
		});
		final List<ErrorMarkerPojo> errorMarkers = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(EntityId.of(-1L)));
		assertEquals(0, errorMarkers.size());
	}

	@Test
	void testCreateDependencyForCalls() {
		testCreateDependency(RelationshipType.CALLS);
	}
	
	@Test
	void testCreateDependencyForIncludes() {
		testCreateDependency(RelationshipType.INCLUDES);
	}
	
	@Test
	void testCreateDependencyForReferences() {
		testCreateDependency(RelationshipType.REFERENCES);
	}

	@Test
	void testCreateDependencyForReadWrites() {
		testCreateDependency(RelationshipType.ACCESSES);
	}
	
	@Test 
	void testCreateDependencyWithEmptyOptionalValues() {
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualImportResult = discoveryPersistence.createDependency(assertNotNull(context), test1Module.identity(),
				test3Module.identity(), null,null, RelationshipType.ACCESSES, Binding.LATE,
				Collections.emptyMap(), Collections.emptyList());
		assertEqualsImportResult(expectedResult, actualImportResult);
		final List<ModuleRelationshipPojo> actualReference = moduleService.findRelationship(q -> q.ofSource(test1Module.identity()).withType(RelationshipType.ACCESSES));
		assertEquals(1, actualReference.size());
		assertEqualsReference(actualReference.get(0), test1Module.getUid(), test3Module.getUid(), null, RelationshipType.ACCESSES,
				Binding.LATE, Collections.emptyMap());
	}

	@Test
	void testPersistAndRetrievalOfDependencyDefinition() {
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);		
		final DependencyDefinitionPojoPrototype dependencyDefinition = newDependencyDefinition(new ModuleFilter(),
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).collect(Collectors.toSet()),
				null, RelationshipType.CALLS, Binding.LATE, attributes);
		final List<DependencyDefinitionPojoPrototype> dependencyDefinitionList = Collections.singletonList(dependencyDefinition);
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test5Module.identity(), dependencyDefinitionList);
		assertEqualsDependencyDefinition(dependencyDefinitionList, test5Module.identity());
	}

	@Test
	void testPersistDependencyDefinitionWithConditionalDependency() {
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);
		final var reachedFromModules = moduleService.findModuleIds(q -> q.ofProject(projectId).withName(test7Module.getName()))
				.stream().map(uid -> new ModuleFilter().setModuleIds(uid)).collect(Collectors.toList());
		final DependencyDefinitionPojoPrototype dependencyDefinition = newDependencyDefinition(List.of(new ModuleFilter()),
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).collect(Collectors.toSet()),
				null, RelationshipType.CALLS, Binding.LATE, attributes, reachedFromModules);
		final List<DependencyDefinitionPojoPrototype> dependencyDefinitionList = Collections.singletonList(dependencyDefinition);
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test11Module.identity(), dependencyDefinitionList);
		assertEqualsDependencyDefinition(dependencyDefinitionList, test11Module.identity());
	}

	@Test
	void testPersistAndRetrievalOfMultipleDependencyDefinition() {
		final DependencyDefinitionPojoPrototype dependencyDefinition1 = newDependencyDefinition(new ModuleFilter(),
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).collect(Collectors.toSet()),
				null, RelationshipType.CALLS, Binding.LATE, null);
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);		
		final ModuleFilter moduleFilter = new ModuleFilter()
				.setModuleIds(test1Module.identity())
				.setNames("Test 1")
				.setTypes(ModuleType.UNKNOWN)
				.setPaths("/src/cobol/programs/Test1.cbl");
		final DependencyDefinitionPojoPrototype dependencyDefinition2 = newDependencyDefinition(moduleFilter,
				Collections.singleton(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL),
				new ModuleLocation(10, 100), RelationshipType.INCLUDES, Binding.UNKNOWN, attributes);
		final List<DependencyDefinitionPojoPrototype> dependencyDefinitionList = Arrays.asList(dependencyDefinition1, dependencyDefinition2);
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test1Module.identity(), dependencyDefinitionList);
		assertEqualsDependencyDefinition(dependencyDefinitionList, test1Module.identity());
	}
	
	@Test
	void testPersistAndRetrievalOfDependencyDefinitionForMultipleContainedIn() {
		final ModuleFilter containedModule1 = new ModuleFilter()
				.setModuleIds(test2Module.identity())
				.setNames("Contained Module 1")
				.setTypes(ModuleType.ECL_JOB)
				.setPaths("/src/cobol/programs/ContainedModule1.cbl");		
		final ModuleFilter moduleFilter = new ModuleFilter()
				.setModuleIds(test1Module.identity())
				.setNames("Test 1", "Test 2")
				.setContainedIn(containedModule1)
				.setTypes(ModuleType.UNKNOWN, ModuleType.XML)
				.setPaths("/src/cobol/programs/Test1.cbl", "/src/cobol/programs/Test2.cbl")
				.setPathPatterns("**/*", "./*");
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);		
		final DependencyDefinitionPojoPrototype dependencyDefinition1 = newDependencyDefinition(moduleFilter,
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).collect(Collectors.toSet()),
				new ModuleLocation(10, 100), RelationshipType.CALLS, Binding.LATE, attributes);
		final DependencyDefinitionPojoPrototype dependencyDefinition2 = newDependencyDefinition(moduleFilter,
				Collections.singleton(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL),
				null, RelationshipType.INCLUDES, Binding.UNKNOWN, attributes);
		final List<DependencyDefinitionPojoPrototype> dependencyDefinitionList = Arrays.asList(dependencyDefinition1, dependencyDefinition2);
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test3Module.identity(), dependencyDefinitionList);
		assertEqualsDependencyDefinition(dependencyDefinitionList, test3Module.identity());
	}

	@Test
	void testPersistAndRetrievalOfDependencyDefinitionWithMultipleFilters() {
		final ModuleFilter containedModule1 = new ModuleFilter()
				.setModuleIds(test2Module.identity())
				.setNames("Contained Module 1")
				.setTypes(ModuleType.ECL_JOB)
				.setPaths("/src/cobol/programs/ContainedModule1.cbl");
		final ModuleFilter moduleFilter1 = new ModuleFilter()
				.setModuleIds(test1Module.identity())
				.setNames("Test 1", "Test 2")
				.setContainedIn(containedModule1)
				.setTypes(ModuleType.UNKNOWN, ModuleType.XML)
				.setPaths("/src/cobol/programs/Test1.cbl", "/src/cobol/programs/Test2.cbl")
				.setPathPatterns("**/*", "./*");
		final ModuleFilter moduleFilter2 = new ModuleFilter()
				.setModuleIds(test1Module.identity())
				.setNames("Test 1", "Test 2")
				.setTypes(ModuleType.UNKNOWN, ModuleType.XML)
				.setPaths("/src/cobol/programs/Test1.cbl", "/src/cobol/programs/Test2.cbl")
				.setPathPatterns("**/*", "./*");
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);
		final DependencyDefinitionPojoPrototype dependencyDefinition1 = newDependencyDefinition(List.of(moduleFilter1, moduleFilter2),
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).collect(Collectors.toSet()),
				new ModuleLocation(10, 100), RelationshipType.CALLS, Binding.LATE, attributes, null);
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test8Module.identity(), Collections.singletonList(dependencyDefinition1));
		List<DependencyDefinitionPojo> ddList = discoveryPersistence.fetchUnresolvedDependencyDefinitions(test8Module.identity());
		assertEquals(1, ddList.size());
		final var moduleFilters = ddList.get(0).getModuleFilters();
		assertEqualsModuleFilter(moduleFilter1, moduleFilters.get(0));
		assertEqualsModuleFilter(moduleFilter2, moduleFilters.get(1));
	}

	@Test
	void testPersistDependencyDefinitionForDBError() {		
		final ModuleFilter moduleFilter = new ModuleFilter()
				.setModuleIds(test1Module.identity())
				.setNames("Test 1")
				.setTypes(ModuleType.UNKNOWN)
				.setPaths("/src/cobol/programs/Test1.cbl");
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);		
		final DependencyDefinitionPojoPrototype dependencyDefinition = newDependencyDefinition(moduleFilter,
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY).collect(Collectors.toSet()),
				new ModuleLocation(10, 100), RelationshipType.CALLS, Binding.LATE, attributes);
		final List<ImportResult<UUID>> actualImportResultList = discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), EntityId.of(-1L),
				Arrays.asList(dependencyDefinition));
		assertEquals(1, actualImportResultList.size());		
		final ImportResult<EntityId> expectedResult = ImportResult.forDbError("Database Error occured while persisting 1 dependency definitions for ModuleId [uid=null,nid=-1]", new Throwable());
		final ImportResult<UUID> actualImportResult = actualImportResultList.get(0);
		assertTrue(actualImportResult.getMessage().get().contains(expectedResult.getMessage().get()));
		assertEquals(expectedResult.getStatus(), actualImportResult.getStatus());
	}

	@Test
	void testFetchModuleIdsWithMergeDuplicates() {
		/* Create the test data with merge duplicates */
		final var dd1 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.MERGE_DUPLICATES))
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 1").setTypes(ModuleType.UNKNOWN)));
		final var dd2 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.CREATE_IF_MISSING))
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 1").setTypes(ModuleType.UNKNOWN)));
		final var dd3 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.MERGE_DUPLICATES, ResolutionFlag.RESOLVE_CASE_INSENSITIVE))
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 1").setTypes(ModuleType.UNKNOWN)));

		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test9Module.identity(), List.of(dd1, dd2));
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test5Module.identity(), List.of(dd3));
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test10Module.identity(), List.of(dd1,dd2,dd3));
		final var moduleIdsWithMergeDuplicates = discoveryPersistence.fetchModuleIdsWithMergeDuplicates(assertNotNull(context).getProjectId());
		assertEquals(1,moduleIdsWithMergeDuplicates.size());
		assertEquals(test10Module.identity(), moduleIdsWithMergeDuplicates.get(0));
	}

	@Test
	void testDeleteDependencyDefinition() {
		final DependencyDefinitionPojoPrototype dependencyDefinition = newDependencyDefinition(new ModuleFilter(), Collections.emptySet(),
				null, RelationshipType.CALLS, Binding.LATE, null);
		final var importResults = discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test7Module.identity(),
				List.of(dependencyDefinition,dependencyDefinition));

		final var id1 = importResults.get(0).getKey();
		assertTrue(id1.isPresent());
		final var id2 = importResults.get(1).getKey();
		assertTrue(id2.isPresent());
		discoveryPersistence.deleteDependencyDefinitions(List.of(id1.get(), id2.get()));
		final List<DependencyDefinitionPojo> actualDependencyDefinition = moduleService.findDependencyDefinitions(q -> q.ofModule(test7Module.identity()));
		assertEquals(0, actualDependencyDefinition.size());
	}
	
	@Test 
	void testCreateCICSAndIMSModuleTypeModule() { 
		final List<ModuleType> moduleTypes = new ArrayList<>(Arrays.asList(ModuleType.CICS_BMS_MAPSET, ModuleType.CICS_BMS_MAP, ModuleType.IMS_MFS));
		final Long mapsetId  = createTestModule("CICS BMS MAPSET", "cics/programs/mapset.cbl", moduleTypes.get(0)).getId();
		final Long mapsId = createTestModule("CICS BMS MAP", "cics/programs/map.cbl", moduleTypes.get(1)).getId();
		final Long mfs = createTestModule("IMS MFS", "ims/programs/Ims.mfs", moduleTypes.get(2)).getId();
		final List<ModulePojo> moduleList = moduleService.findModules(b -> b.ofProject(projectId).byNids(Arrays.asList(mapsetId, mapsId, mfs)));
		assertEquals(3, moduleList.size());
		moduleList.forEach(module -> moduleTypes.remove(ModuleType.fromTechnologyAndType(module.getTechnology(), module.getType())));
		assertEquals(0, moduleTypes.size());
	}

	@Test
	void testPersistModuleLinkHash() {

		final String containingModulePath = "/src/c/programs/Test9.c";
		final String moduleName = "ModuleLinkHash";
		final ModuleType  moduleType = ModuleType.COBOL_PROGRAM;
		final String expectedHash = LinkHash.calculateLinkHash(moduleName, moduleType.getTechnology().toString(),
				moduleType.getType().toString(), null, containingModulePath, null);
		final ModuleFilter filter = new ModuleFilter().setNames(moduleName).setContainedIn(new ModuleFilter().setModuleIds(test9Module.identity()));
		final ModulePojoPrototype definition = createModuleDefinition(moduleName, null, Collections.emptyList());
		
		final ImportResult<EntityId> result = discoveryPersistence.persistModule(assertNotNull(context), filter, definition);
		assertTrue(result.isSuccess());
		assertTrue(result.getKey().isPresent());
		final EntityId moduleId = result.getKey().get();
		final ModulePojo module = moduleService.getModule(moduleId);
		assertEquals(expectedHash, module.getLinkHash());
	}

	@Test
	void testContainingModuleLinkHash() {
		final String firstSchemaExpectedHash = LinkHash.calculateLinkHash("TEST_SCHEMA_1", "SQL", "SCHEMA", null, null, null);
		final String firstTableExpectedHash = LinkHash.calculateLinkHash("TABLE_1", "SQL", "TABLE", null, null, firstSchemaExpectedHash);
		final ModuleFilter filter = new ModuleFilter().setNames("TABLE_1").setContainedIn(new ModuleFilter().setModuleIds(test11Module.identity()));
		final ModulePojoPrototype moduleDefinition = newModuleDefinition("TABLE_1", ModuleType.SQL_TABLE, 
				null, null, Storage.DATABASE, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);

		final ImportResult<EntityId> result = discoveryPersistence.persistModule(assertNotNull(context), filter, moduleDefinition);
		assertTrue(result.isSuccess());
		assertTrue(result.getKey().isPresent());
		final EntityId moduleId = result.getKey().get();
		final ModulePojo module = moduleService.getModule(moduleId);
		assertEquals(firstTableExpectedHash, module.getLinkHash());
		assertEquals(firstSchemaExpectedHash, moduleService.getModule(test11Module.identity()).getLinkHash());
	}

	/**
	 * This Test is to confirm the EdgeReferences are correctly added for the dependencies created.
	 */
	@Test
	void testCreateDependency() {
		final ModulePojo testAModule = createCobolModule("Test A", "/src/cobol/programs/TestA.cbl");
		createDependency(RelationshipType.CALLS, testAModule.identity());
		final ModulePojo testBModule = createCobolModule("Test B", "/src/cobol/programs/TestB.cbl");
		createDependency(RelationshipType.INCLUDES, testBModule.identity());
		final ModulePojo testCModule = createCobolModule("Test C", "/src/cobol/programs/TestC.cbl");
		createDependency(RelationshipType.REFERENCES, testCModule.identity());
		final ModulePojo testDModule = createCobolModule("Test D", "/src/cobol/programs/TestD.cbl");
		createDependency(RelationshipType.ACCESSES, testDModule.identity());
		createDependencyWithEmptyOptionalValues();
		assertEqualsEdgeReference(test4Module.identity());
	}

	@Test
	void testPersistModuleWithoutCaseInsensitiveFlag() {
		final ModuleFilter moduleFilter1 =  new ModuleFilter().setNames("test module1");
		final ModulePojoPrototype moduleDefinition1 = createModuleDefinition("test module1", "cobol/programs/tests/test module1.cbl", Collections.emptyList());
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult1 = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter1, moduleDefinition1);

		final ModuleFilter moduleFilter2 =  new ModuleFilter().setNames("TEST MODULE1");
		final ModulePojoPrototype moduleDefinition2 = createModuleDefinition("TEST MODULE1", "cobol/programs/test/TEST MODULE1.cbl", Collections.emptyList());
		final ImportResult<EntityId> actualResult2 = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter2, moduleDefinition2);

		assertEqualsImportResult(expectedResult, actualResult1);
		assertEqualsImportResult(expectedResult, actualResult2);
		final List<ModulePojo> actualModules = moduleService.findModules(b -> b.ofProject(projectId).withName("TEST MODULE1", true));
		assertEquals(2, actualModules.size());
		if (actualModules.get(0).getName().equals(moduleDefinition1.name.getNonNull())) {
			assertEqualsModule(moduleDefinition1, actualModules.get(0));
			assertEqualsModule(moduleDefinition2, actualModules.get(1));
		} else {
			assertEqualsModule(moduleDefinition1, actualModules.get(1));
			assertEqualsModule(moduleDefinition2, actualModules.get(0));
		}
	}

	@Test
	void testPersistModuleWithCaseInsensitiveFlag() {
		final ModuleFilter moduleFilter1 =  new ModuleFilter().setNames("test module2");
		final ModulePojoPrototype moduleDefinition1 = createModuleDefinition("test module2", "cobol/programs/test/test module2.cbl", Collections.emptyList());
		final ImportResult<EntityId> expectedResult1 = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualResult1 = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter1, moduleDefinition1,
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE);

		final ImportResult<EntityId> expectedResult2 = ImportResult.forSuccessfulUpdate(EntityId.of(Long.valueOf(0)));
		final ModuleFilter moduleFilter2 =  new ModuleFilter().setNames("TEST MODULE2");
		final ModulePojoPrototype moduleDefinition2 = createModuleDefinition("TEST MODULE2", "cobol/programs/test/TEST MODULE2.cbl", Collections.emptyList());
		final ImportResult<EntityId> actualResult2 = discoveryPersistence.persistModule(assertNotNull(context), moduleFilter2, moduleDefinition2,
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE);

		assertEqualsImportResult(expectedResult1, actualResult1);
		assertEqualsImportResult(expectedResult2, actualResult2);
		final List<ModulePojo> actualModule = moduleService.findModules(b -> b.ofProject(projectId).withName("TEST MODULE2", true));
		assertEquals(1, actualModule.size());
		assertEqualsModule(moduleDefinition2, actualModule.get(0));
	}

	@Test
	void testfindModulesWithCaseInsensitiveFlag() {
		final ModulePojo sampleModule1 = createCobolModule("sample1", "/src/cobol/programs/sample/sample1.cbl");
		final ModulePojo sampleModule2 = createCobolModule("SAMPLE1", "/src/cobol/program/smaple/SAMPLE1.cbl");
		final ModuleFilter moduleFilter1 =  new ModuleFilter().setNames("sample1");
		final List<EntityId> findModules = discoveryPersistence.findModules(assertNotNull(context), moduleFilter1, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		assertEquals(2, findModules.size());
		assertTrue(findModules.contains(sampleModule1.identity()), "Loaded modules must contain id of sample1 - /src/cobol/programs/sample/sample1.cbl");
		assertTrue(findModules.contains(sampleModule2.identity()), "Loaded modules must contain id of SAMPLE1 - /src/cobol/program/sample/SAMPLE1.cbl");
	}

	@Test
	void testfindModulesWithoutCaseInsensitiveFlag() {
		final ModulePojo sampleModule1 = createCobolModule("sample2", "/src/cobol/programs/sample/sample2.cbl");
		final ModulePojo sampleModule2 = createCobolModule("SAMPLE2", "/src/cobol/program/sample/SAMPLE2.cbl");
		final ModuleFilter moduleFilter1 =  new ModuleFilter().setNames("sample2");
		final List<EntityId> findModules1 = discoveryPersistence.findModules(assertNotNull(context), moduleFilter1);
		assertEquals(1, findModules1.size());
		assertEquals(sampleModule1.identity(), findModules1.get(0));
		final ModuleFilter moduleFilter2 =  new ModuleFilter().setNames("SAMPLE2");
		final List<EntityId> findModules2 = discoveryPersistence.findModules(assertNotNull(context), moduleFilter2);
		assertEquals(1, findModules2.size());
		assertEquals(sampleModule2.identity(), findModules2.get(0));
	}

	@Test
	void testFindModuleByOrigin() {
		final ModuleFilter modulefilter = new ModuleFilter().setOrigin(test10Module.getOrigin());
		final List<Long> findModules = discoveryPersistence.findModules(assertNotNull(context), modulefilter, 
				ResolutionFlag.RESOLVE_CASE_INSENSITIVE).stream().map(EntityId::getNid).collect(Collectors.toList());
		assertEquals(29, findModules.size());
		Collections.sort(findModules);
		assertEquals(test1Module.getId(), findModules.get(0));
		assertEquals(test2Module.getId(), findModules.get(1));
		assertEquals(test3Module.getId(), findModules.get(2));
		assertEquals(test4Module.getId(), findModules.get(3));
		assertEquals(test5Module.getId(), findModules.get(4));
	}

	@SuppressWarnings("unused")
	private void createDependency(final RelationshipType relationship, final EntityId targetModuleId) {
		final Map<String, Object> attributes =
				Collections.singletonMap(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE.name(), ModelAttributeValue.SendReceiveAccess.SEND);
		discoveryPersistence.createDependency(assertNotNull(context), test4Module.identity(), test1Module.identity(),
				null, null, relationship, Binding.LATE, attributes, Collections.emptyList());
	}

	void createDependencyWithEmptyOptionalValues() {
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualImportResult = discoveryPersistence.createDependency(assertNotNull(context), test4Module.identity(), test5Module.identity(),
				null, null, RelationshipType.ACCESSES, Binding.LATE, Collections.emptyMap(), Collections.emptyList());
		assertEqualsImportResult(expectedResult, actualImportResult);
	}

	private void assertEqualsEdgeReference(final EntityId fromModuleId) {
		final int countOfCalls = moduleService.findRelationship(q -> q.ofSource(fromModuleId).withType(RelationshipType.CALLS)).size();
		final int countOfIncludes = moduleService.findRelationship(q -> q.ofSource(fromModuleId).withType(RelationshipType.INCLUDES)).size();
		final int countOfReferences = moduleService.findRelationship(q -> q.ofSource(fromModuleId).withType(RelationshipType.REFERENCES)).size();
		final int countOfReadWrites = moduleService.findRelationship(q -> q.ofSource(fromModuleId).withType(RelationshipType.ACCESSES)).size();
		/* Assert the counts for each relationship type */
		assertEquals(1, countOfCalls);
		assertEquals(1, countOfIncludes);
		assertEquals(1, countOfReferences);
		assertEquals(2, countOfReadWrites);
	}

	private ModulePojo createCobolModule(final String name, @Nullable final String path, final Date metricsDate) {
		final ModulePojoPrototype cobolProgram = createModule(name, path);
		cobolProgram.setTechnology(Technology.COBOL);
		cobolProgram.setType(Type.PROGRAM);
		cobolProgram.setCreator(Creator.DISCOVERY);
		cobolProgram.setMetricsDate(metricsDate.toInstant());
		return moduleService.getModule(moduleService.create(cobolProgram));
	}

	private ModulePojo createCobolModule(final String name, @Nullable final String path) {
		final ModulePojoPrototype cobolProgram = createModule(name, path);
		cobolProgram.setTechnology(Technology.COBOL);
		cobolProgram.setType(Type.PROGRAM);
		cobolProgram.setCreator(Creator.DISCOVERY);
		cobolProgram.setRepresentation(Representation.PHYSICAL);
		return moduleService.getModule(moduleService.create(cobolProgram));
	}
	
	private ModulePojo createTestModule(final String name, @Nullable final String path, final ModuleType moduleType) {
		final ModulePojoPrototype cobolProgram = createModule(name, path);
		cobolProgram.setTechnology(moduleType.getTechnology());
		cobolProgram.setType(moduleType.getType());
		return moduleService.getModule(moduleService.create(cobolProgram));
	}

	private ModulePojoPrototype createModule(final String name, @Nullable final String path) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(name);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		if (path != null) {
			module.setPath(path);
		}
		module.setCreator(Creator.DISCOVERY);
		return module;
	}
	
	private ModulePojoPrototype createModuleDefinition(final String name, @Nullable final String path, final Collection<SourceMetricsPojoPrototype> additionalInfo) {
		final var pojo = new ModulePojoPrototype()
				.setName(name)
				.setTechnology(ModuleType.COBOL_PROGRAM.getTechnology())
				.setType(ModuleType.COBOL_PROGRAM.getType())
				.setLocation(new ModuleLocation(1,1))
				.setPath(path)
				.setRepresentation(Representation.PHYSICAL)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM);

		if ( ! additionalInfo.isEmpty()) {
			pojo.setSourceMetrics(additionalInfo.iterator().next());
		}

		return pojo;
	}
	
	private void assertEqualsImportResult(final ImportResult<EntityId> expected, final ImportResult<EntityId> actual) {
		assertEquals(expected.getCause(), actual.getCause());
	    assertEquals(expected.getStatus(), actual.getStatus());
	    assertTrue(actual.getMessage().orElse(StringUtils.EMPTY).startsWith(expected.getMessage().orElse(StringUtils.EMPTY).toString()));
	}
	
	private void assertEqualsModule(final ModulePojoPrototype moduleDefinition, final ModulePojo actualModule) {
		assertEquals(moduleDefinition.name.get(), actualModule.getName());
		assertEquals(Optional.ofNullable(moduleDefinition.path.get()), actualModule.getPath());
		assertEquals(moduleDefinition.technology.get(), actualModule.getTechnology());
		assertEquals(moduleDefinition.type.get(), actualModule.getType());
		assertEquals(moduleDefinition.origin.get(), actualModule.getOrigin());
		assertEquals(moduleDefinition.storage.get(), actualModule.getStorage());
		assertEquals(Optional.ofNullable(moduleDefinition.representation.get()), actualModule.getRepresentation());
		assertEquals(moduleDefinition.identification.get(), actualModule.getIdentification());
		assertEquals(Optional.ofNullable(moduleDefinition.location.get()), actualModule.getLocation());
		assertNotNull(actualModule.getLinkHash());
	}

	private void testCreateDependency(final RelationshipType relationship) {
		final ModuleLocation location = new ModuleLocation(112, 86);
		final Map<String, Object> attributes = Collections.singletonMap(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE.name(),
				ModelAttributeValue.SendReceiveAccess.SEND);
		final ImportResult<EntityId> expectedResult = ImportResult.forSuccessfulCreation(EntityId.of(Long.valueOf(0)));
		final ImportResult<EntityId> actualImportResult = discoveryPersistence.createDependency(assertNotNull(context), test2Module.identity(),
				test3Module.identity(), null, location, relationship , Binding.LATE, attributes, Collections.emptyList());
		assertEqualsImportResult(expectedResult, actualImportResult);
		final List<ModuleRelationshipPojo> actualReference = moduleService.findRelationship(q -> q.ofSource(test2Module.identity()).withType(relationship));
		assertNotNull(actualReference);
		assertEqualsReference(actualReference.get(0), test2Module.getUid(), test3Module.getUid(), location, relationship, Binding.LATE, attributes);
	}

	@Test
	void testMarkDependencyDefinitionResolved() {
		final var dependencyDefinition = newDependencyDefinition(new ModuleFilter(),
				Stream.of(ResolutionFlag.RESOLVE_CASE_INSENSITIVE).collect(Collectors.toSet()),
				null, RelationshipType.CALLS, Binding.LATE, null);
		final List<ImportResult<UUID>> importResults = discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), test4Module.identity(),
				List.of(dependencyDefinition));
		assertEquals(1, importResults.size(), "result size must be 1");
		assertTrue(importResults.get(0).isSuccess(), "import result must have success as true");
		final UUID id =  importResults.get(0).getKey().orElse(new UUID(0, 0));
		discoveryPersistence.markDependencyDefinitionResolved(id);
		final List<DependencyDefinitionPojo> dependencyDefinitions = discoveryPersistence.fetchUnresolvedDependencyDefinitions(test4Module.identity());
		assertTrue(dependencyDefinitions.isEmpty(), "module should not have any unresolved dependencies");
	}

	private void assertEqualsReference(final ModuleRelationshipPojo actualReference, final UUID fromId, final UUID toId, @Nullable final ModuleLocation fromLocation,
			final RelationshipType relationship, final Binding binding, final Map<String, Object> attributes) {
		assertEquals(fromId, actualReference.getSrcModule());
		assertEquals(toId, actualReference.getDstModule());
		assertEquals(fromLocation, actualReference.getSrcLocation().orElse(null));
		assertEquals(relationship, actualReference.getRelationship());
		assertEquals(binding, actualReference.getDependencyBinding().orElse(null));
		assertEquals(StringUtils.join(attributes), StringUtils.join(actualReference.getProperties().orElse(Collections.emptyMap())).replaceAll("\"",""));
	}
	
	private void assertEqualsErrorMarkers(final ErrorMarker expectedErrorMarkers, final ErrorMarkerPojo actualErrorMarkers) {
		assertEquals(expectedErrorMarkers.getKey(), actualErrorMarkers.getKey());
		assertEquals(expectedErrorMarkers.getSeverity(), actualErrorMarkers.getSeverity());
		assertEquals(expectedErrorMarkers.getCause(), actualErrorMarkers.getCause());
	}
	
	private void assertEqualsDeadCode(final ModuleDeadCodePojoPrototype expectedDeadCode, final ModuleDeadCodePojo actualDeadCode) {
		assertEquals(expectedDeadCode.deadCode.getNonNull(), actualDeadCode.getDeadCode());
		assertEquals(expectedDeadCode.startingLine.getNonNull(), actualDeadCode.getStartingLine());
		assertEquals(expectedDeadCode.numberOfLines.getNonNull(), actualDeadCode.getNumberOfLines());
	}
	
	private void assertEqualsDependencyDefinition(final List<DependencyDefinitionPojoPrototype> expectedDependencyDefinitionList, final EntityId actualModuleId) {
		List<DependencyDefinitionPojo> ddList = discoveryPersistence.fetchUnresolvedDependencyDefinitions(actualModuleId);
		List<DependencyDefinitionPojo> actualDependencyDefinitionSortedList = ddList.stream()
				.sorted(Comparator.comparing(DependencyDefinitionPojo::getRelationshipType))
				.collect(Collectors.toList());
		List<DependencyDefinitionPojoPrototype> expectedDependencyDefinitionSortedList = expectedDependencyDefinitionList.stream()
				.sorted(Comparator.comparing(p -> p.type.getNonNull()))
				.collect(Collectors.toList());
		assertEquals(expectedDependencyDefinitionSortedList.size(), actualDependencyDefinitionSortedList.size());
		for (int i = 0; i < expectedDependencyDefinitionSortedList.size(); i++) {
			final DependencyDefinitionPojoPrototype expectedDependencyDefinition = expectedDependencyDefinitionSortedList.get(i);
			final DependencyDefinitionPojo actualDependencyDefinition = actualDependencyDefinitionSortedList.get(i);
			assertEquals(getConvertAndSortedSet.apply(expectedDependencyDefinition.resolutionFlags.get()),
					getSortedSet.apply(actualDependencyDefinition.getResolutionFlags().stream().map(ResolutionFlag::name).collect(Collectors.toSet())));

			assertEquals(1, actualDependencyDefinition.getModuleFilters().size());
			assertEqualsModuleFilter(expectedDependencyDefinition.moduleFilters.getNonNull().get(0), actualDependencyDefinition.getModuleFilters().get(0));
			
			final var expectedAttributes = expectedDependencyDefinition.attributes.orElse(Collections.emptyMap());
			if (expectedAttributes == null) {
				assertTrue(assertNotNull(actualDependencyDefinition.getAttributes()).isEmpty());
			} else {
				var actualAttributes = assertNotNull(actualDependencyDefinition.getAttributes());
				assertEquals(expectedAttributes.size(), actualAttributes.size());
			}			
			assertEquals(expectedDependencyDefinition.bindingType.get(), actualDependencyDefinition.getBindingType());
			assertEquals(expectedDependencyDefinition.type.get(), actualDependencyDefinition.getRelationshipType());
			assertEquals(expectedDependencyDefinition.location.orElse(null), actualDependencyDefinition.getLocation().orElse(null));
			if (expectedDependencyDefinition.reachedFromModules.isPresent()) {
				assertEquals(expectedDependencyDefinition.reachedFromModules.getNonNull().size(),
						assertNotNull(actualDependencyDefinition.getReachedFromModules()).size());
			}
		}
	}
	
	private final Function<Set<String>, Set<String>> getSortedSet = set -> set.stream().sorted().collect(Collectors.toSet());
	private final Function<Set<?>, Set<String>> getConvertAndSortedSet = set -> set.stream().map(Object::toString).sorted().collect(Collectors.toSet());

	private void assertEqualsModuleFilter(final ModuleFilter expectedModuleFilter, final ModuleFilter actualModuleFilter) {
		assertEquals(getSortedSet.apply(expectedModuleFilter.getNames()), getSortedSet.apply(actualModuleFilter.getNames()));
		final Set<String> moduleTypes = actualModuleFilter.getTypes().stream().map(ModuleType::name).collect(Collectors.toSet());
		assertEquals(getConvertAndSortedSet.apply(expectedModuleFilter.getTypes()), getSortedSet.apply(moduleTypes));
		assertEquals(getSortedSet.apply(expectedModuleFilter.getPaths()), getSortedSet.apply(actualModuleFilter.getPaths()));
		assertEquals(getSortedSet.apply(expectedModuleFilter.getPathPatterns()), getSortedSet.apply(actualModuleFilter.getPathPatterns()));
		if (expectedModuleFilter.getContainedIn().isPresent()) {
			assertEqualsModuleFilter(expectedModuleFilter.getContainedIn().get(), actualModuleFilter.getContainedIn().get());
		} else {
			assertFalse(actualModuleFilter.getContainedIn().isPresent());
		}
		assertEquals(expectedModuleFilter.getModuleIds(), actualModuleFilter.getModuleIds());
	}

	private StatementPojoPrototype createStatementDefinition(final StatementType statementType, final String text, final Map<String, Object> properties) {
		return new StatementPojoPrototype()
				.setType(statementType)
				.setText(text)
				.setProperties(properties);
	}

	private DependencyDefinitionPojoPrototype newDependencyDefinition(final ModuleFilter moduleFilter, final Set<ResolutionFlag> resolutionFlags,
			@Nullable final ModuleLocation location, final RelationshipType relationshipType, final Binding bindingType, @Nullable final Map<String, Object> attributes) {
		return newDependencyDefinition(List.of(moduleFilter), resolutionFlags, location, relationshipType, bindingType, attributes, null);
	}

	private DependencyDefinitionPojoPrototype newDependencyDefinition(final List<ModuleFilter> moduleFilters, final Set<ResolutionFlag> resolutionFlags,
			@Nullable final ModuleLocation location, final RelationshipType relationshipType, final Binding bindingType, @Nullable final Map<String, Object> attributes,
			@Nullable List<ModuleFilter> reachedFromModules) {
		final DependencyDefinitionPojoPrototype definition = new DependencyDefinitionPojoPrototype()
				.setModuleFilters(moduleFilters)
				.setResolutionFlags(resolutionFlags)
				.setRelationshipType(relationshipType)
				.setBindingType(bindingType);

		if (location != null) {
			definition.setLocation(location);
		}
		if (attributes != null && ! attributes.isEmpty()) {
			definition.setAttributes(attributes);
		}
		if (reachedFromModules != null && ! reachedFromModules.isEmpty()) {
			definition.setReachedFromModules(reachedFromModules);
		}

		return definition;
	}

	private ModuleDeadCodePojoPrototype newModelDeadCode(final String name, final int offset, final int length) {
		return new ModuleDeadCodePojoPrototype()
				.setDeadCode(name)
				.setStartingLine(offset)
				.setNumberOfLines(length);
	}

}
