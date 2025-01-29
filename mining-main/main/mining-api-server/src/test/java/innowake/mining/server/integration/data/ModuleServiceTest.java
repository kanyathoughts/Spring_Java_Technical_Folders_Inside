package innowake.mining.server.integration.data;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.testcontainers.shaded.org.hamcrest.MatcherAssert.assertThat;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.calcite.avatica.org.apache.commons.codec.binary.Hex;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;
import org.testcontainers.shaded.org.hamcrest.Matchers;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipBasePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourceContentPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
* Tests {@link ModuleService}
*/
@ActiveProfiles("no_caching")
class ModuleServiceTest extends DatabaseResettingTest {
	
	private final int TEST_MODULE_COUNT = 100;
	private static final Long ZERO = Long.valueOf(0);
	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	private final Long TEST_PROJECT_ID_LONG = Long.valueOf(4);
	private final EntityId TEST_PROJECT_ID = EntityId.of(TEST_PROJECT_ID_LONG);
	/* Module name and description for testing special character escaping for Lucene text search */
	private static final String BRKG_TRD_FIXED = "brkg.trd.fixed( ) [ ] { } | & \\ + - ! ? : ^ \" ~ ";

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private TaxonomyService taxonomyService;

	@Autowired
	protected AnnotationService annotationService;
	
	@Autowired
	protected SourceService sourceService;
	
	@Autowired
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;
	
	/**
	 * Tests if the field {@code module.parent} value is populated through {@link ModuleService}
	 */
	@Test
	void testFetchContainingModule() {
		final ModulePojo module = moduleService.getModule(EntityId.of(Long.valueOf(2001)));
		assertEquals(Long.valueOf(2000), module.getParent().orElseThrow().getNid());
	}
	
	/**
	 * It should delete a single module and its Annotations, SourceAttachments, Excel stuff, SourceMetrics and HasAdditionalInfo edges for SourceMetrics
	 */
	@Test
	void testDeleteSingleModule() {
		/* verify project is empty before running test */
		assertCount(0, 0);
		
		final EntityId firstModuleId = insertTestData("TEST_MODULE_SINGLE_1");
		insertTestData("TEST_MODULE_SINGLE_2"); /* also insert a second module */
		
		/* verify data was inserted */
		assertCount(2, 2);
		
		/* verify data was inserted into SourceMetrics and linked with HasAdditionalInfo edges */
		assertEquals(2, getSourceMetricsCount());

		moduleService.deleteModule(firstModuleId, true);
		
		/* verify only 1 Module was deleted */
		assertCount(1, 2);
		
		/* verify only 1 SourceMetrics record was deleted along with HasAdditionalInfo edge*/
		assertEquals(1, getSourceMetricsCount());

		/* Verify that the deleted DependencyDefinition belongs to the correct module */
		final List<DependencyDefinitionPojo> unresolvedDependencyDefinitions = orientDiscoveryPersistence.fetchUnresolvedDependencyDefinitions(firstModuleId);
		assertTrue(unresolvedDependencyDefinitions.isEmpty(), "No dependency Definition should be present for the deleted module");
	}

	/**
	 * It should delete all Modules, Annotations, SourceAttachments and Excel stuff
	 * but no SourceObjects.
	 * 
	 * (this variant is used when re-running Discovery on an existing Project,
	 * as everything must be deleted but not the source code)
	 */
	@Test
	void testDeleteAllNoSourceObjects() {
		/* verify project is empty before running test */
		assertCount(0, 0);
		assertEquals(0, getModuleUndiscoveredCount());
		
		for (int i = 0; i < TEST_MODULE_COUNT; i++) {
			insertTestData("TEST_MODULE_SOURCE_" + i);			
		}
		
		/* verify data was inserted */
		assertCount(TEST_MODULE_COUNT, TEST_MODULE_COUNT);
		assertEquals(TEST_MODULE_COUNT, getModuleUndiscoveredCount());
		
		moduleService.deleteModules(TEST_PROJECT_ID, false, false);
		
		/* verify everything was deleted except source objects */
		assertCount(0, TEST_MODULE_COUNT);
		assertEquals(0, getModuleUndiscoveredCount());
	}
	
	@Test
	void testDependentModuleIds() {
		final EntityId moduleIdA = createTestModule("MODULEA", Storage.FILE_SECTION);
		final EntityId moduleIdB = createTestModule("MODULEB", Storage.FILE_SECTION);
		final EntityId moduleIdC = createTestModule("MODULEC", Storage.FILE_SECTION);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(moduleIdA)
				.setDstModule(moduleIdB));
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.REFERENCES)
				.setSrcModule(moduleIdA)
				.setDstModule(moduleIdC));
		
		final List<EntityId> dependentModuleIds = moduleService.findModuleIds(b -> b.ofProject(TEST_PROJECT_ID)
				.withSourceRelationshipsFrom(moduleIdA, RelationshipType.DEPENDENCY_TYPES_ARR)
				.withStorage(Storage.FILE_SECTION));
		
		assertNotNull(dependentModuleIds);
		assertEquals(2, dependentModuleIds.size());
		assertTrue(dependentModuleIds.containsAll(Arrays.asList(moduleIdB, moduleIdC)));
	}
	
	@Test
	void testModuleWithPathAndContentWithoutSourceObject() {
		final ModulePojoPrototype testModule1 = new ModulePojoPrototype();
		testModule1.setName("TEST MODULE 1");
		testModule1.setProject(TEST_PROJECT_ID);
		testModule1.setTechnology(Technology.COBOL);
		testModule1.setType(Type.fromName("COBOL_PROGRAM"));
		testModule1.setStorage(Storage.FILE);
		testModule1.setIdentification(Identification.IDENTIFIED);
		testModule1.setOrigin(Origin.CUSTOM);
		testModule1.setCreator(Creator.DISCOVERY);
		testModule1.setDescription("questions contented him few extensive supported. "
				+ "Of remarkably thoroughly he appearance in. Supposing tolerably applauded or of be. "
				+ "Suffering unfeeling so objection agreeable allowance me of. " + "Ask within entire season appearance common far who family.");
		testModule1.setContent("Data");
		final String path = "src/cobol/programs/DPG1.cpy";
		testModule1.setPath(path);
		Assertions.assertThrows(MiningEntityNotFoundException.class, () -> sourceService.get(q -> q.ofProject(TEST_PROJECT_ID).withPath(path)));

		final ModulePojo module = moduleService.getModule(moduleService.create(testModule1));
		assertTrue(module.isSourceCodeAvailable());
		assertTrue(module.getContentHash().isPresent());
		assertEquals("B6814E542CB015EAB8D2D6723D4CA45C", Hex.encodeHexString(module.getContentHash().get().get(), false));
		/* SourcePojo was created */
		final SourcePojo sourcePojo = sourceService.get(q -> q.ofProject(TEST_PROJECT_ID).withPath(path));
		assertEquals(path, sourcePojo.getPath());
	}
	
	@Test
	void testModuleWithPathWithoutContentAndWithoutSourceObject() {
		final ModulePojoPrototype testModule2 = new ModulePojoPrototype();
		testModule2.setName("TEST MODULE 2");
		testModule2.setProject(EntityId.of(TWO));
		testModule2.setTechnology(Technology.NATURAL);
		testModule2.setType(Type.fromName("NATURAL_PROGRAM"));
		testModule2.setStorage(Storage.DATABASE);
		testModule2.setIdentification(Identification.MISSING);
		testModule2.setOrigin(Origin.ENVIRONMENT);
		testModule2.setCreator(Creator.DISCOVERY);
		testModule2.setDescription(
				"Was drawing natural fat respect husband. " + "An as noisy an offer drawn blush place. " + "These tried for way joy wrote witty. "
						+ "In mr began music weeks after at begin. " + "Education no dejection so direction pretended household do to.");
		final String path = "src/cobol/programs/DPG2.cpy";
		testModule2.setPath(path);
		Assertions.assertThrows(MiningEntityNotFoundException.class, () -> sourceService.get(q -> q.ofProject(EntityId.of(ONE)).withPath(path)));
		final ModulePojo module = moduleService.getModule(moduleService.create(testModule2));
		assertFalse(module.isSourceCodeAvailable());
		assertFalse(module.getContentHash().isPresent());
	}
	
	@Test
	void testModuleWithoutPathWithContent() {
		final String content = "BEGIN; UPDATE FOO SET bar=true; COMMIT;";
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setName("TEST MODULE 2");
		testModule.setProject(TEST_PROJECT_ID);
		testModule.setTechnology(Technology.SQL);
		testModule.setType(Type.fromName("STORED_PROCEDURE"));
		testModule.setStorage(Storage.DATABASE);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setContent(content);
		testModule.setCreator(Creator.DISCOVERY);
		
		final ModulePojo module = moduleService.findAnyModule(q -> q.byId(moduleService.create(testModule)).includeContent(true))
												.orElseThrow(() -> new MiningEntityNotFoundException("Created module must exist"));
		assertTrue(module.isSourceCodeAvailable());
		assertTrue(module.getContent().isPresent());
		assertEquals(content, module.getContent().get());
		assertEquals(Collections.emptyList(), sourceService.find(q -> q.ofProject(TEST_PROJECT_ID))); /* no SourcePojo was created, just SourceAttachment */
		final List<SourceContentPojo> sourceContents = sourceService.findContent(q -> q.ofProject(TEST_PROJECT_ID));
		assertEquals(1, sourceContents.size());
		assertEquals(content, sourceContents.get(0).getContent().toString());
	}

	@Test
	void testModuleWithPathWithContentAndWithSourceObject() {
		final ModulePojoPrototype testModule1 = new ModulePojoPrototype();
		testModule1.setName("TEST MODULE 1");
		testModule1.setProject(TEST_PROJECT_ID);
		testModule1.setTechnology(Technology.COBOL);
		testModule1.setType(Type.fromName("COBOL_PROGRAM"));
		testModule1.setStorage(Storage.FILE);
		testModule1.setIdentification(Identification.IDENTIFIED);
		testModule1.setOrigin(Origin.CUSTOM);
		testModule1.setCreator(Creator.DISCOVERY);
		testModule1.setDescription("Repulsive questions contented him few extensive supported. "
				+ "Of remarkably thoroughly he appearance in. Supposing tolerably applauded or of be. "
				+ "Suffering unfeeling so objection agreeable allowance me of. " + "Ask within entire season appearance common far who family.");
		testModule1.setContent("Test Data");
		testModule1.setPath("programs/DPG1.cbl");
		createProgram(TEST_PROJECT_ID, "programs", "DPG1");
		Assertions.assertThrows(IllegalStateException.class, () -> moduleService.create(testModule1));
	}
	
	@Test
	void testModuleWithPathWithOutContentAndWithSourceObject() {
		final ModulePojoPrototype testModule2 = new ModulePojoPrototype();
		testModule2.setName("TEST MODULE 2");
		testModule2.setProject(TEST_PROJECT_ID);
		testModule2.setTechnology(Technology.NATURAL);
		testModule2.setType(Type.fromName("NATURAL_PROGRAM"));
		testModule2.setStorage(Storage.DATABASE);
		testModule2.setIdentification(Identification.MISSING);
		testModule2.setOrigin(Origin.ENVIRONMENT);
		testModule2.setCreator(Creator.DISCOVERY);
		testModule2.setDescription(
				"Was drawing natural fat respect husband. " + "An as noisy an offer drawn blush place. " + "These tried for way joy wrote witty. "
						+ "In mr began music weeks after at begin. " + "Education no dejection so direction pretended household do to.");
		testModule2.setPath("programs/DPG2.cbl");
		createProgram(TEST_PROJECT_ID, "programs", "DPG2");
		final ModulePojo module = moduleService.getModule(moduleService.create(testModule2));
		assertTrue(module.isSourceCodeAvailable());
		assertTrue(module.getContentHash().isPresent());
		assertEquals("8E8D16F9A8D83AA2886F6C95A830CD99", Hex.encodeHexString(module.getContentHash().get().get(), false));
	}
	
	@Test
	void testModuleStatistics() {
		createTestModules("TEST MODULE 3", Technology.COBOL, 700, "programs3/DPG2.cbl");
		createTestModules("TEST MODULE 4", Technology.COBOL, 800, "programs4/DPG2.cbl");
		createTestModules("TEST MODULE 5", Technology.NATURAL, 6000, "programs5/DPG2.cbl");
		createTestModules("TEST MODULE 6", Technology.CICS, 100, "programs6/DPG2.cbl");
		createTestModules("TEST MODULE 7", Technology.CICS, 300, "programs7/DPG2.cbl");
		createTestModules("TEST MODULE 8", Technology.CICS, 500, "programs8/DPG2.cbl");

		final long lineOfCode = moduleService.countSourceMetricsCodeLines(b -> b.ofProject(TEST_PROJECT_ID));
		final Map<String, Long> tec = moduleService.countSourceMetricsCodeLinesByTechnology(b -> b.ofProject(TEST_PROJECT_ID));

		assertEquals(8400L, lineOfCode);
		assertEquals(3, tec.size());
		assertEquals(1500, tec.get(Technology.COBOL.toString()));
		assertEquals(6000, tec.get(Technology.NATURAL.toString()));
		assertEquals(900, tec.get(Technology.CICS.toString()));
	}
	
	@Test
	void testFetchModuleByListOfIdAndProjectId() {
		final List<Long> ids = Arrays.asList(Long.valueOf(2000L), Long.valueOf(2001L), Long.valueOf(2002L), Long.valueOf(2003L));
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(EntityId.of(ONE)).byNids(ids));
		assertEquals(3, modules.size(), "Returned modules should be one less as one of them is in a different project");
	}

	@Test
	void testModuleWithMetadata() {
		final EntityId moduleId = insertTestData("TEST_MODULE_METADATA");
		final List<EntityId> moduleIds = moduleService.findModulesWithMetaData(TEST_PROJECT_ID);
		assertEquals(1, moduleIds.size());
		assertEquals(moduleId, moduleIds.get(0));
	}
	
	/**
	 * Find Modules by passing Single Taxonomy ID
	 */
	@Test
	void testFindModulesWithSingleTaxonomyId() {
		final var modules = taxonomyService.findTaxonomyModulesIds(q -> q.ofProject(EntityId.of(ONE)).byId(EntityId.of(1L)));
		assertEquals(2, modules.size());
	}
	
	/**
	 * Find Modules by passing More than 1 Taxonomy IDs
	 */
	@Test
	void testFindModulesWithMultipleTaxonomyId() {
		final var project = EntityId.of(TWO);
		final Collection<EntityId> moduleIds = taxonomyService.findTaxonomyModulesIds(q -> q.ofProject(project)
																							.byIds(Stream.of(4L, 5L, 6L)
																									.map(EntityId::of)
																									.collect(Collectors.toList())));
		
		final List<ModuleLightweightPojo> modules = moduleIds.isEmpty() ? Collections.emptyList() : 
													moduleService.findModulesLightweight(q -> q.ofProject(project).byIds(moduleIds));
		assertEquals(1, modules.size());
		assertEquals(Long.valueOf(2003), modules.get(0).getId());
	}
	
	/**
	 * Find Modules by passing 0 Taxonomy ID, It should return an empty Module list
	 */
	@Test
	void testFindModulesWithNoTaxonomyId() {
		final var modules = taxonomyService.findTaxonomyModulesIds(q -> q.ofProject(EntityId.of(ONE)).byIds(Collections.emptyList()));
		assertEquals(0, modules.size());
	}

	/**
	 * Tests that {@link ModuleService.ModuleInquiryBuilder#withDescription(String)} escapes special characters in the description before performing the query with Lucene.
	 * <p>Tests that {@link ModuleService.ModuleInquiryBuilder#withName(String)} escapes special characters in the name before performing the query with Lucene.</p>
	 */
	@Test
	void testFindByDescriptionWithSpecialCharacters() {
		createTestModule(BRKG_TRD_FIXED, Storage.FILE);
		final List<ModulePojo> byDescription = moduleService.findModules(b -> b.ofProject(TEST_PROJECT_ID).withDescription(BRKG_TRD_FIXED));
		assertEquals(1, byDescription.size(), String.format("findByDescription() for description='%s' must return one matching module", BRKG_TRD_FIXED));
		assertEquals(BRKG_TRD_FIXED, byDescription.get(0).getName());
		final List<ModulePojo> byName = moduleService.findModules(b -> b.ofProject(TEST_PROJECT_ID).withName(BRKG_TRD_FIXED));
		assertEquals(1, byName.size(), String.format("findByName() for name='%s' must return one matching module", BRKG_TRD_FIXED));
		assertEquals(BRKG_TRD_FIXED, byName.get(0).getName());
	}
	
	/**
	 * Tests that {@link Module} with Project id 0, should not have any record with  {@link Type} UNKNOWN,
	 * if the artifact is utility then use the Type UTILITY and not UNKNOWN.
	 */
	@Test
	void testModuleCountByProjectIdAndTypeUnknown() {
		assertEquals(0, getModuleCountWithTypeUnknown());
	}

	@Test
	void moduleWithoutDirectSourceAttachmentHasSourceCodeAvailableWhenContainedInAnotherModule() {
		final EntityId parentModule = createTestModule("Parent Module", Storage.FILE, "Some arbitrary content");
		final EntityId childModule = createTestModule("Child Module", Storage.FILE);
		final EntityId moduleWithoutContent = createTestModule("Module without Content", Storage.FILE);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
												.setSrcModule(parentModule)
												.setDstModule(childModule)
												.setRelationship(RelationshipType.CONTAINS));
		
		final ModulePojo containingModuleUnderTest = moduleService.getModule(childModule);
		assertTrue(containingModuleUnderTest.isSourceCodeAvailable(), "isSourceCodeAvailable must be true when loading child module with includeContent=false");

		final ModulePojo childModuleLoadedIncludingContent = moduleService.getModule(childModule);
		assertTrue(childModuleLoadedIncludingContent.isSourceCodeAvailable(), "isSourceCodeAvailable must be true when loading child module with includeContent=true");

		final ModulePojo moduleWithoutContentUnderTest = moduleService.getModule(moduleWithoutContent);
		assertFalse(moduleWithoutContentUnderTest.isSourceCodeAvailable(), "isSourceCodeAvailable must be false when module has no content");
	}

	@Test
	void testModuleForLinkHash() {
		final ModulePojoPrototype testModule1 = getModuleWithContent();
		testModule1.setStorage(Storage.FILE);
		final ModulePojo module = moduleService.getModule(moduleService.create(testModule1));
		assertEquals("76S6JT8gDsakGk07hH9B2Y", module.getLinkHash());

		testModule1.withId(module.identity());
		testModule1.setPath("");
		testModule1.setName("MISS 1");
		testModule1.linkHash.unset();

		final ModulePojo module2 = moduleService.getModule(moduleService.update(testModule1));
		assertEquals("1tGs4mxtn9GYVRv9bOcfX", module2.getLinkHash());

		testModule1.withId(module.identity());
		testModule1.setName("SUB 1");
		testModule1.setParentPath("/src/MOD 1");
		testModule1.linkHash.unset();

		final ModulePojo module3 = moduleService.getModule(moduleService.update(testModule1));
		assertEquals("4YhqNVlpX3RvaLlvO3Vje0", module3.getLinkHash());
	}
	
	@Test
	void testFindModuleByLinkHashWithContent() { 
		final ModulePojoPrototype testModule1 = getModuleWithContent();
		final Optional<ModulePojo> module = moduleService.findAnyModule(q -> q.byId(moduleService.create(testModule1)).includeContent(true));
		assertTrue(module.isPresent());
		final String hash = module.get().getLinkHash();
		final Optional<ModulePojo> daoModule = moduleService.findAnyModule(b -> b.ofProject(EntityId.of(ONE)).withLinkHash(hash).includeContent(true));
		assertTrue(daoModule.isPresent());
		assertEquals(testModule1.name.get(), daoModule.get().getName());
		assertEquals(testModule1.content.get(), daoModule.get().getContent().get());
	}
	
	@Test
	void testFindModuleByInvalidLinkHash() {
		final var module = moduleService.findAnyModule(b -> b.ofProject(EntityId.of(ONE)).withLinkHash("INVALIDHASH"));
		assertFalse(module.isPresent());
	}

	@Test
	void testFindModuleIdByLinkHash() {
		final EntityId module1Id = createTestModule("module1", Storage.FILE);
		final EntityId module2Id = createTestModule("module2", Storage.FILE);
		final EntityId module3Id = createTestModule("module3", Storage.FILE);

		final ModulePojo module1 = moduleService.getModule(module1Id);
		final ModulePojo module2 = moduleService.getModule(module2Id);
		final ModulePojo module3 = moduleService.getModule(module3Id);

		final Map<String, EntityId> expected = Map.of(
				module1.getLinkHash(), module1.identity(),
				module2.getLinkHash(), module2.identity(),
				module3.getLinkHash(), module3.identity()
		);

		final Map<String, EntityId> moduleIdsByLinkHash = moduleService.findModuleIdsByLinkHash(q -> q.ofProject(TEST_PROJECT_ID));

		assertEquals(expected, moduleIdsByLinkHash);
	}
	
	@Test
	void testHotSpotForReferencesWithNoContent() {
		insertTestData("TEST_MODULE_SINGLE_1");
		insertTestData("TEST_MODULE_SINGLE_2");
		final List<HotSpot> hotSpotList = moduleService.findHotSpots(q -> q.ofProject(TEST_PROJECT_ID).limit(4), FilterType.REFERENCES);
		assertTrue(hotSpotList.size() > 0, "List should not be empty ");
		hotSpotList.stream()
				.forEach(e -> assertFalse(e.getModule().isSourceCodeAvailable(), "isSourceCodeAvailable must be false when module has no content"));
	}

	@Test
	void testHotSpotForReferencesWithContent() {
		createTestModule("TEST_MODULE_SINGLE_1", Storage.FILE, "Piece of source code");
		final List<HotSpot> hotSpotList = moduleService.findHotSpots(q -> q.ofProject(TEST_PROJECT_ID).limit(3), FilterType.REFERENCES);
		assertTrue(hotSpotList.size() > 0, "List should not be empty ");
		assertTrue(hotSpotList.get(0).getModule().isSourceCodeAvailable(), "isSourceCodeAvailable must be true when module has content");
	}

	@Test
	void testHotSpotForDataBaseTablesWithContent() {
		final ModulePojoPrototype moduleA = getModuleWithContent();
		moduleA.setTechnology(Technology.SQL);
		moduleA.setType(Type.fromName("TABLE"));
		moduleA.setOrigin(Origin.CUSTOM);
		moduleA.setProject(TEST_PROJECT_ID);

		moduleService.create(moduleA);
		final List<HotSpot> hotSpotList = moduleService.findHotSpots(q -> q.ofProject(TEST_PROJECT_ID).limit(3), FilterType.DATABASE_TABLES);
		assertTrue(hotSpotList.size() > 0, "List should not be empty ");
		assertFalse(hotSpotList.get(0).getModule().isSourceCodeAvailable(), "isSourceCodeAvailable must be false when module has content for dataTables");
	}
	
	@Test
	void testModuleCountByCreator() {
		final long moduleCountForDiscovery1 = moduleService.countModules(q -> q.ofProject(TEST_PROJECT_ID).withCreator(Creator.DISCOVERY));
		final long moduleCountForApi1 = moduleService.countModules(q -> q.ofProject(TEST_PROJECT_ID).withCreator(Creator.API));
		assertEquals(0, moduleCountForDiscovery1);
		assertEquals(0, moduleCountForApi1);
		/*Creating a module with creator DISCOVERY.*/
		createTestModule("ABC", Storage.FILE);
		/*Creating a module with creator API.*/
		final ModulePojoPrototype module = getModuleWithContent();
		module.setProject(TEST_PROJECT_ID);
		module.setCreator(Creator.API);
		moduleService.create(module);
		
		final long moduleCountForDiscovery2 = moduleService.countModules(q -> q.ofProject(TEST_PROJECT_ID).withCreator(Creator.DISCOVERY));
		final long moduleCountForApi2 = moduleService.countModules(q -> q.ofProject(TEST_PROJECT_ID).withCreator(Creator.API));
		assertEquals(1, moduleCountForDiscovery2);
		assertEquals(1, moduleCountForApi2);
	}

	@Test
	void testfindIncludedModuleIds() {
		final EntityId moduleId1 = createTestModule("Root Module", Storage.FILE);
		final EntityId moduleId2 = createTestModule("Included Module", Storage.FILE);
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleId1)
				.setDstModule(moduleId2));
		
		final List<ModuleRelationshipPojo> moduleIdsList = moduleService.findRelationship(q -> q.ofSource(moduleId1)
																								.withType(RelationshipType.INCLUDES));
		assertEquals(1, moduleIdsList.size());
		assertEquals(moduleId1.getUid(), moduleIdsList.get(0).getSrcModule(), "Source module should match");
		assertEquals(moduleId2.getUid(), moduleIdsList.get(0).getDstModule(), "Destination module should match");
	}

	@Test
	void testFindAnnotationBasedOnOffset() {
		final EntityId moduleId = createTestModule("Module", Storage.FILE, "Some arbitrary content");
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(600));
		dummyLocation.setLength(Integer.valueOf(50));
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(moduleId)
				.setName("Annotation 1")
				.setSourceAttachment("This is if-ELSE source attachment \n content")
				.setLocation(dummyLocation)
				.setType(AnnotationType.DATABASE)
				.setState(WorkingState.CANDIDATE)
				.setCreatedByUserId("1"));

		final ModuleLocation dummyLocation1 = new ModuleLocation();
		dummyLocation1.setOffset(Integer.valueOf(1000));
		dummyLocation1.setLength(Integer.valueOf(150));
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(moduleId)
				.setName("Annotation 2")
				.setSourceAttachment("This is SUBTract source attachment content")
				.setLocation(dummyLocation1)
				.setType(AnnotationType.DATABASE)
				.setState(WorkingState.FOR_REVIEW)
				.setCreatedByUserId("1"));

		final ModuleLocation dummyLocation2 = new ModuleLocation();
		dummyLocation2.setOffset(Integer.valueOf(2000));
		dummyLocation2.setLength(Integer.valueOf(350));
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(moduleId)
				.setName("Annotation 3")
				.setSourceAttachment("This is if and WHILE source attachment content")
				.setLocation(dummyLocation2)
				.setType(AnnotationType.RULE)
				.setState(WorkingState.CANDIDATE)
				.setCreatedByUserId("1"));

		final List<AnnotationPojo> annotationList1 = annotationService.find(q -> q.ofProject(TEST_PROJECT_ID).ofModule(moduleId).withOffsetBetween(0, 500));
		assertEquals(0, annotationList1.size());
		
		final List<AnnotationPojo> annotationList2 = annotationService.find(q -> q.ofProject(TEST_PROJECT_ID).ofModule(moduleId).withOffsetBetween(1000, 3000));
		assertEquals(2, annotationList2.size());
		assertEquals(Set.of("Annotation 2", "Annotation 3"), annotationList2.stream().map(ann -> ann.getName()).collect(Collectors.toSet()));
		
		final List<AnnotationPojo> annotationList3 = annotationService.find(q -> q.ofProject(TEST_PROJECT_ID).ofModule(moduleId).withMinOffset(500));
		assertEquals(3, annotationList3.size());
		assertEquals(Set.of("Annotation 1", "Annotation 2", "Annotation 3"), annotationList3.stream().map(ann -> ann.getName()).collect(Collectors.toSet()));
	}

	@Test
	void testModuleFilteringByRelationshipType() {
		final EntityId moduleId1 = createTestModule("Module 1", Storage.FILE);
		final EntityId moduleId2 = createTestModule("Module 2", Storage.FILE);
		final EntityId moduleId3 = createTestModule("Module 3", Storage.FILE);
		final EntityId moduleId4 = createTestModule("Module 4", Storage.FILE);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(moduleId1)
				.setDstModule(moduleId2));

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleId1)
				.setDstModule(moduleId3));

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(moduleId1)
				.setDstModule(moduleId4));

		final List<ModuleRelationshipPojo> moduleIdsList = moduleService.findRelationship(q -> q.ofSource(moduleId1)
				.withType(RelationshipType.CALLS));
		assertEquals(2, moduleIdsList.size());
		assertThat (Set.of(moduleId1.getUid()), Matchers.containsInAnyOrder(
				moduleIdsList.stream().map(ModuleRelationshipBasePojo::getSrcModule).distinct().toArray()));
		assertThat(Set.of(moduleId2.getUid(), moduleId4.getUid()), Matchers.containsInAnyOrder(
				moduleIdsList.stream().map(ModuleRelationshipBasePojo::getDstModule).distinct().toArray()));
	}

	@Test
	void testModuleRelationshipByProperties() {
		final Map<String, Object> properties1 = Map.of("Property1", "Value1");
		final Map<String, Object> properties2 = Map.of("Property2", "Value2");
		final EntityId moduleId1 = createTestModule("Module 1", Storage.FILE);
		final EntityId moduleId2 = createTestModule("Module 2", Storage.FILE);
		final EntityId moduleId3 = createTestModule("Module 3", Storage.FILE);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(moduleId1)
				.setDstModule(moduleId2)
				.setProperties(properties1));

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.INCLUDES)
				.setSrcModule(moduleId1)
				.setDstModule(moduleId3)
				.setProperties(properties2));

		/* Find the Module Relationship by input Property map */
		List<ModuleRelationshipPojo> moduleIdsList = moduleService.findRelationship(q -> q.ofSource(moduleId1)
				.withProperties(properties1));
		assertEquals(1, moduleIdsList.size());
		assertEquals(moduleId2.getUid(), moduleIdsList.get(0).getDstModule());

		/* Find the Module Relationship which matches the input key pattern (Case Insensitive) */
		moduleIdsList = moduleService.findRelationship(q -> q.ofSource(moduleId1)
				.withProperties("pROpeRtY2"));
		assertEquals(1, moduleIdsList.size());
		assertEquals(moduleId3.getUid(), moduleIdsList.get(0).getDstModule());
	}

	@Test
	void testModuleFilteringAndOrderingByAnnotation() {
		final String annotationCount = "annotationCount";
		final var testModule1 = createTestModule("Module1", Storage.FILE, null);
		final var testModule2 = createTestModule("Module2", Storage.FILE, null);

		createAnnotations(testModule1, "Annotation 1");
		createAnnotations(testModule1, "Annotation 2");
		createAnnotations(testModule2, "Annotation 3");

		assertEquals(2, annotationService.count(q -> q.ofModule(testModule1)));
		assertEquals(1, annotationService.count(q -> q.ofModule(testModule2)));

		/* Filtering by Annotation count Greater than equal to 1 */
		List<ModulePojo> modules = moduleService.findModules(q -> q.ofProject(TEST_PROJECT_ID)
				.includeAnnotationCount(annotationCount, null, null, null)
				.sortBy(annotationCount, SortDirection.DESCENDING)
				.filterBy(annotationCount, Comperator.GREATER_OR_EQUAL, 1));
		assertEquals(2, modules.size());
		assertEquals(testModule1.getNid(), modules.get(0).getId());

		/* Filtering by Annotation count equals to 2 */
		modules = moduleService.findModules(q -> q.ofProject(TEST_PROJECT_ID)
				.includeAnnotationCount(annotationCount, null, null, null)
				.filterBy(annotationCount, Comperator.EQUAL, 2));
		assertEquals(1, modules.size());
		assertEquals(testModule1.getNid(), modules.get(0).getId());
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

	private void createAnnotations(final EntityId module, final String name) {
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(2000);
		dummyLocation.setLength(350);
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(module)
				.setName(name)
				.setSourceAttachment("Source")
				.setLocation(dummyLocation)
				.setType(AnnotationType.RULE)
				.setState(WorkingState.CANDIDATE)
				.setCreatedByUserId("1"));
	}
	
	private ModulePojoPrototype getModuleWithContent() {
		final ModulePojoPrototype testModule1 = new ModulePojoPrototype();
		testModule1.setProject(EntityId.of(ONE));
		testModule1.setName("MOD 1");
		final String path = "/src/MOD 1";
		testModule1.setPath(path);
		testModule1.setTechnology(Technology.COBOL);
		testModule1.setType(Type.PROGRAM);
		testModule1.setIdentification(Identification.IDENTIFIED);
		testModule1.setOrigin(Origin.CUSTOM);
		testModule1.setStorage(Storage.FILE);
		testModule1.setContent("Test Data");
		testModule1.setCreator(Creator.DISCOVERY);
		return testModule1;
	}
	
	private SourcePojo createProgram(final EntityId projectId, final String folder, final String name) {
		final String path = folder + "/" + name + ".cbl";
		sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName(name)
				.setPath(path)
				.setTechnology(COBOL)
				.setType(PROGRAM)
				.setContent(new BinaryString("some code")));
		return sourceService.get(q -> q.ofProject(projectId).withPath(path));
	}
	
	private void createTestModules(final String name, final Technology technology, final Integer linesOfCode, final String path ) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setName(name);
		testModule.setProject(TEST_PROJECT_ID);
		testModule.setTechnology(technology);
		testModule.setType(Type.fromName("NATURAL_PROGRAM"));
		testModule.setStorage(Storage.fromName("DATABASE"));
		testModule.setIdentification(Identification.MISSING);
		testModule.setOrigin(Origin.ENVIRONMENT);
		testModule.setCreator(Creator.DISCOVERY);
		testModule.setRepresentation(Representation.PHYSICAL);

		final SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype();
		sourceMetrics.setCodeLines(linesOfCode);
		testModule.setSourceMetrics(sourceMetrics);
		testModule.setDescription(
				"Was drawing natural fat respect husband. " + "An as noisy an offer drawn blush place. " + "These tried for way joy wrote witty. "
						+ "In mr began music weeks after at begin. " + "Education no dejection so direction pretended household do to.");
		testModule.setPath(path);
		moduleService.create(testModule);
	}

	private EntityId createTestModule(final String name, final Storage storage, @Nullable final String content) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setProject(TEST_PROJECT_ID);
		testModule.setName(name);
		testModule.setDescription(name);
		testModule.setTechnology(Technology.COBOL);
		testModule.setType(Type.PROGRAM);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setStorage(storage);
		testModule.setRepresentation(Representation.PHYSICAL);
		testModule.setCreator(Creator.DISCOVERY);
		if (content != null) {
			testModule.setContent(content);
			testModule.setPath("some arbitrary path");
		}
		return moduleService.create(testModule);
	}
	
	private EntityId createTestModule(final String name, final Storage storage) {
		return createTestModule(name, storage, null);
	}

	private EntityId insertTestData(final String name) {
		final EntityId moduleId = createTestModule(name, Storage.FILE);
		
		/* after insert, recordId must be assigned */
		final var statementClass = new StatementPojoPrototype();
		final var module = moduleService.getModule(moduleId);
		statementClass.setModule(moduleId);
		statementClass.setTechnology(module.getTechnology());
		statementClass.setType(StatementType.DISPLAY);
		statementClass.setText("DISPLAY 'yay'");
		moduleService.createStatement(statementClass, false);
		
		final StatementPojoPrototype sqlStatement = new StatementPojoPrototype();
		sqlStatement.setModule(moduleId);
		sqlStatement.setTechnology(module.getTechnology());
		sqlStatement.setType(StatementType.SELECT);
		sqlStatement.setText( "SELECT FROM WHERE");
		
		final Map<String, Object> properties = new HashMap<>();
		properties.put(StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY, Integer.valueOf(1));
		properties.put(StatementPojo.PROPERTY_KEY_DISTINCT_TABLES, Integer.valueOf(2));
		properties.put(StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY, Float.valueOf(3.14f));
		properties.put(StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY, Float.valueOf(0.815f));
		properties.put(StatementPojo.PROPERTY_KEY_SQL_LENGTH, Integer.valueOf(42));
		properties.put(StatementPojo.PROPERTY_KEY_TABLES, Integer.valueOf(3));
		sqlStatement.setProperties(properties);
		
		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
				.setProject(TEST_PROJECT_ID)
				.setCause("baz")
				.setModule(moduleId));
		
		moduleService.createDeadCode(new ModuleDeadCodePojoPrototype()
											.setModule(moduleId)
											.setDeadCode("I'm dead")
											.setStartingLine(15)
											.setNumberOfLines(10));
		moduleService.createUndiscovered(new ModuleUndiscoveredPojoPrototype()
											.setProject(TEST_PROJECT_ID)
											.setName("UNDISCOVERED-" + moduleId)
											.setPath("/foo/bar/test-" + moduleId));

		final SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype()
			.setPhysicalLines(Integer.valueOf(30))
			.setCodeLines(Integer.valueOf(20))
			.setCommentLines(Integer.valueOf(10))
			.setComplexityMcCabe(Integer.valueOf(10))
			.setDeadCodeLines(Integer.valueOf(0))
			.setModule(moduleId);
		
		moduleService.putSourceMetrics(sourceMetrics);
		
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype()
			.setModule(moduleId)
			.setName("I am an Annotation")
			.setSourceAttachment("This is a piece of source code")
			.setLocation(new ModuleLocation(47, 11))
			.setType(AnnotationType.RULE)
			.setState(WorkingState.CANDIDATE)
			.setCreatedByUserId("roflcopter");
		annotationService.create(annotation);
		
		final SourcePojo sourceObject = sourceService.get(sourceService.create(new SourcePojoPrototype()
				.setProject(TEST_PROJECT_ID)
				.setName("TESTSOURCE")
				.setPath("/foo/baz/test-" + moduleId)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setContent(new BinaryString("I am a Source Object"))));
		final DependencyDefinitionPojoPrototype definition = new DependencyDefinitionPojoPrototype()
				.setModuleFilters(List.of(new ModuleFilter().setNames(name)))
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY))
				.setLocation(new ModuleLocation(1, 1))
				.setRelationshipType(RelationshipType.ACCESSES)
				.setBindingType(Binding.LATE);
		orientDiscoveryPersistence.persistDependencyDefinitions(new DiscoveryTestContext(Collections.singletonList(sourceObject), TEST_PROJECT_ID),
				moduleId, Collections.singletonList(definition));
		return moduleId;
	}
	
	private void assertCount(final long count, final long sourceObjectCount) {
		assertEquals(count, getModuleCount());
		assertEquals(count, getAnnotationCount());
		assertEquals(count, getSourceAttachmentCount());
		assertEquals(sourceObjectCount, getSourceObjectCount());
		assertEquals(count, getStatementsCount());
		assertEquals(count, getErrorMarkersCount());
		assertEquals(count, getSqlStatementCount());
		assertEquals(count, getExcelSheetDeadCodeCount());
		assertEquals(count, getDependencyDefinitionCount());
	}
	
	private long getModuleCount() {
		return moduleService.countModules(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getAnnotationCount() {
		return annotationService.count(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getSourceAttachmentCount() {
		return annotationService.count(q -> q.ofProject(TEST_PROJECT_ID).filterHasSource(true));
	}
	
	private long getSourceMetricsCount() {
		return moduleService.countSourceMetrics(b -> b.ofProject(TEST_PROJECT_ID));
	}
	
	private long getSourceObjectCount() {
		return sourceService.count(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getStatementsCount() {
		return moduleService.countStatements(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getErrorMarkersCount() {
		return moduleService.countErrorMarkers(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getSqlStatementCount() {
		return moduleService.findStatements(q -> q.ofProject(TEST_PROJECT_ID)).size();
	}
	
	private long getExcelSheetDeadCodeCount() {
		return moduleService.countDeadCode(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getModuleUndiscoveredCount() {
		return moduleService.countUndiscovered(q -> q.ofProject(TEST_PROJECT_ID));
	}
	
	private long getModuleCountWithTypeUnknown() {
		return moduleService.countModules(q -> q.ofProject(EntityId.of(ZERO)).withTechnology(Technology.UNKNOWN).withType(Type.UNKNOWN));
	}

	private long getDependencyDefinitionCount() {
		return moduleService.countDependencyDefinitions(q -> q.ofProject(TEST_PROJECT_ID));
	}
}
