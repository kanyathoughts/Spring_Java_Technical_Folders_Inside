/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static innowake.mining.shared.model.FeatureId.DEPENDENCY_GRAPH_EXPLORE;
import static innowake.mining.test.CustomProperties.getCustomPropertyByName;
import static innowake.mining.test.CustomProperties.verifyNumberOfCustomProperties;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.http.HttpStatus;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.annotation.AnnotationServiceProvider;
import innowake.mining.client.service.datadictionary.DataDictionaryServiceProvider;
import innowake.mining.client.service.feature.FeatureServiceProvider;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.client.service.module.StoreAstNodes;
import innowake.mining.client.service.module.TraverseDependencies;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.client.service.reference.ReferenceServiceProvider;
import innowake.mining.data.access.postgres.AnnotationPgDao;
import innowake.mining.data.access.postgres.AstPgDao;
import innowake.mining.data.access.postgres.DataDictionaryPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.HotSpot;
import innowake.mining.shared.model.HotSpot.FilterType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.LinkedModule;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import innowake.mining.test.util.JobStatusUtil;

/**
 * Integration tests for the {@link Module} service.
 */
public class ModuleServiceTest extends IntegrationTest {

	private final String IDENTIFIED_MODULE_MSG = "Identified 1 Module(s) in selection which are supported by the Module description identification, "
			+ "i.e. COBOL Program and BMS Mapset Modules.";
	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	private final ReferenceServiceProvider referenceServiceProvider = MiningApiClient.referenceService(getConnectionInfo());
	private final AnnotationServiceProvider annotationServiceProvider = MiningApiClient.annotationService(getConnectionInfo());
	private final DataDictionaryServiceProvider dataDictionaryServiceProvider = MiningApiClient.dataDictionaryService(getConnectionInfo());
	private final JobServiceProvider jobServiceProvider = MiningApiClient.jobService(getConnectionInfo());
	private final ProjectServiceProvider projectServiceProvider = MiningApiClient.projectService(getConnectionInfo());
	private final FeatureServiceProvider featureService = MiningApiClient.featureService(getConnectionInfo());
	private static final EntityId NON_EXISTING_ID = EntityId.of(Long.MAX_VALUE);
	private static final EntityId ONE = EntityId.of(1l);
	private static final EntityId TWO = EntityId.of(2l);
	private static final EntityId THREE = EntityId.of(3l);
	private static final EntityId FOUR = EntityId.of(4l);
	private static final EntityId FIRST_CUSTOM_MODULE_ID = EntityId.of(2000l);
	private static final EntityId CUSTOM_MODULE_WITH_DD_ID = EntityId.of(2002l);
	private static final int NUMBER_OF_EXPECTED_CUSTOM_PROPERTIES = 1;
	private static final String TEST_MODULE_NAME = "MMRS7101";
	private static final String TEST_MODULE_DESCRIPTION = "A test description for MMRS7101";
	private static final int JOB_TIMEOUT = 2;
	
	private static final ModulePojoPrototype TEST_MODULE_1 = new ModulePojoPrototype();
	private static final ModulePojoPrototype TEST_MODULE_2 = new ModulePojoPrototype();
	private static final ModulePojoPrototype TEST_MODULE_3 = new ModulePojoPrototype();
	private static final ModulePojoPrototype TEST_MODULE_4 = new ModulePojoPrototype();
	private static final ModulePojoPrototype TEST_MODULE_5 = new ModulePojoPrototype();
	private static final ModulePojoPrototype TEST_MODULE_6 = new ModulePojoPrototype();
	private static final ModulePojoPrototype TEST_MODULE_7 = new ModulePojoPrototype();
	
	@Nullable
	private static ModulePgDao moduleDao;
	@Nullable
	private static AnnotationPgDao annotataionDao;
	@Nullable
	private static DataDictionaryPgDao dataDictionaryDao;
	
	@BeforeAll
	public static void init() {
		moduleDao = new ModulePgDao(getDataSource());
		annotataionDao = new AnnotationPgDao(getDataSource());
		dataDictionaryDao = new DataDictionaryPgDao(getDataSource());
		
		TEST_MODULE_1.setName("TEST MODULE 1");
		TEST_MODULE_1.setPath("src/cobol/programs/TESTMODULE1.cbl");
		TEST_MODULE_1.setProject(ONE);
		TEST_MODULE_1.setTechnology(Technology.COBOL);
		TEST_MODULE_1.setType(Type.PROGRAM);
		TEST_MODULE_1.setStorage(Storage.FILE);
		TEST_MODULE_1.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_1.setOrigin(Origin.CUSTOM);
		TEST_MODULE_1.setCreator(Creator.DISCOVERY);
		TEST_MODULE_1.setDescription("Repulsive questions contented him few extensive supported. "
				+ "Of remarkably thoroughly he appearance in. Supposing tolerably applauded or of be. "
				+ "Suffering unfeeling so objection agreeable allowance me of. " + "Ask within entire season appearance common far who family.");
		TEST_MODULE_2.setName("TEST MODULE 2");
		TEST_MODULE_2.setPath("src/natural/programs/TESTMODULE2.nsp");
		TEST_MODULE_2.setProject(TWO);
		TEST_MODULE_2.setTechnology(Technology.NATURAL);
		TEST_MODULE_2.setType(Type.PROGRAM);
		TEST_MODULE_2.setStorage(Storage.DATABASE);
		TEST_MODULE_2.setIdentification(Identification.MISSING);
		TEST_MODULE_2.setOrigin(Origin.ENVIRONMENT);
		TEST_MODULE_2.setCreator(Creator.DISCOVERY);
		TEST_MODULE_2.setDescription(
				"Was drawing natural fat respect husband. " + "An as noisy an offer drawn blush place. " + "These tried for way joy wrote witty. "
						+ "In mr began music weeks after at begin. " + "Education no dejection so direction pretended household do to.");
		TEST_MODULE_3.setName("TEST MODULE 3");
		TEST_MODULE_3.setPath("src/cobol/programs/TESTMODULE3.cbl");
		TEST_MODULE_3.setProject(ONE);
		TEST_MODULE_3.setTechnology(Technology.COBOL);
		TEST_MODULE_3.setType(Type.PROGRAM);
		TEST_MODULE_3.setStorage(Storage.DATABASE);
		TEST_MODULE_3.setIdentification(Identification.MISSING);
		TEST_MODULE_3.setOrigin(Origin.CUSTOM);
		TEST_MODULE_3.setCreator(Creator.DISCOVERY);
		TEST_MODULE_3.setDescription(
				"Listening appearance shameless by abilities pronounce oh suspected is affection. " + "Next it draw in season much bred. Agreeable entire.");
		
		TEST_MODULE_4.setTechnology(Technology.BASIC);
		TEST_MODULE_5.setTechnology(Technology.JCL);
		
		TEST_MODULE_6.setName("TEST MODULE 6");
		TEST_MODULE_6.setPath("src/cobol/programs/TESTMODULE6.cbl");
		TEST_MODULE_6.setProject(ONE);
		TEST_MODULE_6.setTechnology(Technology.COBOL);
		TEST_MODULE_6.setType(Type.PROGRAM);
		TEST_MODULE_6.setStorage(Storage.FILE);
		TEST_MODULE_6.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_6.setOrigin(Origin.CUSTOM);
		TEST_MODULE_6.setCreator(Creator.DISCOVERY);
		TEST_MODULE_6.setDescription("Repulsive questions contented him few extensive supported. "
				+ "Of remarkably thoroughly he appearance in. Supposing tolerably applauded or of be. "
				+ "Suffering unfeeling so objection agreeable allowance me of. " + "Ask within entire season appearance common far who family.");
		
		TEST_MODULE_7.setTechnology(Technology.CICS);
		final var sm = new SourceMetricsPojoPrototype();
		TEST_MODULE_6.setSourceMetrics(sm);
		sm.setCodeLines(Integer.valueOf(114));
		sm.setCommentLines(Integer.valueOf(115));
		sm.setComplexityMcCabe(Integer.valueOf(116));
		sm.setDeadCodeLines(Integer.valueOf(117));
		sm.setPhysicalLines(Integer.valueOf(118));
	}
	
	@Test
	void testInfoPropertyOnModule() throws Exception {
		final String moduleName = "Myname";
		final String firstKey = "mykey";
		final String firstValue = "myvalue";
		final String secondKey = "yourkey";
		final String secondValue = "yourvalue";
		
		/* Create module with Map information */
		final Map<String, Object> info = new HashMap<>();
		info.put(firstKey, firstValue);
		info.put(secondKey, secondValue);

		final SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype()
				.setCodeLines(5).setCodeLines(3).setComplexityMcCabe(27).setDeadCodeLines(0);
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName(moduleName)
				.setPath("src/myname.cbl")
				.setProject(ONE)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setInfo(info)
				.setDescription("test description for module")
				.setSourceMetrics(sourceMetrics)
				.setContent("test content for module");

		final Result<ModulePojo> execute = moduleServiceProvider.createModule().setProjectId(ONE).setModule(module).execute();

		assertEquals(HttpStatus.SC_CREATED, execute.getStatusCode());

		/* Find newly created module */
		final Result<ModulePojo[]> findCreatedModuleResult = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName(moduleName).execute();

		assertEquals(HttpStatus.SC_OK, findCreatedModuleResult.getStatusCode());

		final ModulePojo[] modules = findCreatedModuleResult.getValue().get();
		assertEquals(1, modules.length);

		final ModulePojo foundModule = modules[0];
		final var foundInfo = foundModule.getInfo().get();
		assertNotNull("Info should not be null", foundInfo);

		final var entrySet = foundInfo.entrySet();
		assertEquals(2, entrySet.size());

		assertEquals(firstValue, foundInfo.get(firstKey));
		assertEquals(secondValue, foundInfo.get(secondKey));
		
		/* Update the map in the module */
		foundInfo.remove(secondKey);
		final String thirdKey = "ourkey";
		final String thirdValue = "ourvalue";
		foundInfo.put(thirdKey, thirdValue);
		
		final Result<ModulePojo> updateResult = moduleServiceProvider.updateModule().setProjectId(ONE).setModule(new ModulePojoPrototype()
				.withId(foundModule.identity())
				.setInfo(foundInfo)
			).execute();
		assertEquals(HttpStatus.SC_OK, updateResult.getStatusCode());
		
		/* Find updated module */
		final Result<ModulePojo[]> findUpdatedModuleResult = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName(moduleName).execute();
		assertEquals(HttpStatus.SC_OK, findUpdatedModuleResult.getStatusCode());
		
		final ModulePojo[] updatedModules = findCreatedModuleResult.getValue().get();
		assertEquals(1, updatedModules.length);

		final ModulePojo updatedModule = updatedModules[0];
		final var updatedInfo = updatedModule.getInfo().get();
		assertNotNull("Info should not be null", updatedInfo);

		final var updatedEntrySet = updatedInfo.entrySet();
		assertEquals(2, updatedEntrySet.size());

		assertEquals(firstValue, foundInfo.get(firstKey));
		assertEquals(thirdValue, foundInfo.get(thirdKey));
	}
	
	@Test
	void testFindByNameNotFound() throws IOException {
		final Result<ModulePojo[]> findCreatedModuleResult = moduleServiceProvider.findModuleByName()
				.setProjectId(ONE)
				.setName("I DO NOT EXIST ... REALLY NOT")
				.execute();
		assertEquals(HttpStatus.SC_OK, findCreatedModuleResult.getStatusCode());
		final ModulePojo[] result = findCreatedModuleResult.getValue().get();
		assertEquals(0, result.length);
	}

	@Test
	void testFindByNameIncludesAllRelevantAttributes() throws IOException {
		final Result<ModulePojo[]> result = moduleServiceProvider.findModuleByName().setName(TEST_MODULE_NAME).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final ModulePojo[] modules = result.getValue().get();
		assertEquals(1, modules.length);
		for (final var module : modules) {
			if (TEST_MODULE_NAME.equals(module.getName())) {
				validateTestModule(module);
				return;
			}
		}
	}
	
	@Test
	void testFindByIdIncludesCFGAttributes() throws IOException {
		final Result<ModulePojo> result = moduleServiceProvider.findModuleById().setProjectId(ONE).setModuleId(FIRST_CUSTOM_MODULE_ID).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final ModulePojo module = result.getValue().get();
		// assertFalse(module.isControlFlowCalculated());
		assertFalse(module.isSourceCodeAvailable());
		
		final Result<ModulePojo> resultTwo = moduleServiceProvider.findModuleById().setProjectId(ONE).setModuleId(EntityId.of(2016L)).execute();
		assertEquals(HttpStatus.SC_OK, resultTwo.getStatusCode());
		assertTrue(resultTwo.getValue().isPresent());
		final ModulePojo moduleTwo = resultTwo.getValue().get();
		// assertFalse(moduleTwo.isControlFlowCalculated());
		assertTrue(moduleTwo.isSourceCodeAvailable());
	}
	
	@Test
	void testFindByNonExistingProject() throws IOException {
		final Result<ModulePojo[]> findCreatedModuleResult = moduleServiceProvider.findModuleByName()
				.setProjectId(NON_EXISTING_ID)
				.setName("I DO NOT EXIST ... REALLY NOT")
				.execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, findCreatedModuleResult.getStatusCode());
		assertFalse(findCreatedModuleResult.getValue().isPresent());
	}
	
	@Test
	void testDeleteAllModules() throws IOException {

		/* Create 2 modules */
		moduleServiceProvider
				.createModule()
				.setProjectId(ONE)
				.setModule(new ModulePojoPrototype()
						.setName("firstModule")
						.setPath("src/firstModule.cbl")
						.setProject(ONE)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API))
				.execute();
		
		moduleServiceProvider
				.createModule()
				.setProjectId(ONE)
				.setModule(new ModulePojoPrototype()
						.setName("secondModule")
						.setPath("src/secondModule.cbl")
						.setProject(ONE)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API))
				.execute();

		/* Verify that at least 2 modules are present in the project */
		final Result<ModulePojo[]> findAllBeforeDeletionResult = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		assertTrue(findAllBeforeDeletionResult.getValue().get().length >= 2);
	
		/* Trigger deletion of all modules */
		final Result<Void> deletionResult = moduleServiceProvider.deleteAllModules().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, deletionResult.getStatusCode());
		
		/* Verify that no modules are present in the project */
		final Result<ModulePojo[]> findAllAfterDeletionResult = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		assertEquals(0, findAllAfterDeletionResult.getValue().get().length);
	}
	
	@Test
	void testDeleteAllModulesAlsoDeletesTheAssociatedSourceAttachments() throws IOException {
		final long sourceAttachmentCountBeforeCreation = getSourceAttachmentCount();
		/* Create 2 modules */
		moduleServiceProvider
				.createModule()
				.setProjectId(TWO)
				.setModule(new ModulePojoPrototype()
						.setName("firstModule")
						.setPath("src/firstModule.cbl")
						.setProject(TWO)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API)
						.setContent("Content for first module"))
				.execute();
		
		moduleServiceProvider
				.createModule()
				.setProjectId(TWO)
				.setModule(new ModulePojoPrototype()
						.setName("secondModule")
						.setPath("src/secondModule.cbl")
						.setProject(TWO)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API)
						.setContent("Content for second module"))
				.execute();
		
		final long sourceAttachmentCountAfterCreation = getSourceAttachmentCount();
		assertEquals(sourceAttachmentCountBeforeCreation + 2, sourceAttachmentCountAfterCreation);
	
		final Result<Void> deletionResult = moduleServiceProvider.deleteAllModules().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, deletionResult.getStatusCode());
		
		final long sourceAttachmentCountAfterDeletion = getSourceAttachmentCount();
		assertTrue(sourceAttachmentCountAfterDeletion <= sourceAttachmentCountBeforeCreation);
	}

	@Test
	void testDeleteAllModulesWithoutAssociatedSourceAttachments() throws IOException {
		final EntityId projectId = EntityId.of(2L);
		final long sourceAttachmentCountBeforeCreation = getSourceAttachmentCount();
		/* Create 2 modules */
		moduleServiceProvider
				.createModule()
				.setProjectId(projectId)
				.setModule(new ModulePojoPrototype()
						.setName("firstModule")
						.setPath("src/firstModule.cbl")
						.setProject(projectId)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API)
						.setContent("Content for first module"))
				.execute();
		
		moduleServiceProvider
				.createModule()
				.setProjectId(projectId)
				.setModule(new ModulePojoPrototype()
						.setName("secondModule")
						.setPath("src/secondModule.cbl")
						.setProject(projectId)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API)
						.setContent("Content for second module"))
				.execute();
		
		final long sourceAttachmentCountAfterCreation = getSourceAttachmentCount();
		assertEquals(sourceAttachmentCountBeforeCreation + 2, sourceAttachmentCountAfterCreation);
	
		final Result<Void> deletionResult = moduleServiceProvider.deleteAllModules().setDeleteSourceObjects(Boolean.FALSE).setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, deletionResult.getStatusCode());
		assertEquals(sourceAttachmentCountAfterCreation, getSourceAttachmentCount());
	}

	@Test
	void testFindAllModulesWithDatabase() throws IOException {
		final Result<ModulePojo[]> resultFindAll = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		final ModulePojo[] modules = resultFindAll.getValue().get();
		verifyFindAll(modules, findAllByJDBC());
	}
	
	@Test
	void testFindAllModulesAndNewModule() throws IOException {
		final Result<ModulePojo[]> resultFindAll = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		createModule(ONE, TEST_MODULE_1);
		createModule(TWO, TEST_MODULE_2);
		final Result<ModulePojo[]> resultFindAll2 = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		final ModulePojo[] foundModules = resultFindAll2.getValue().get();
		assertEquals(foundModules.length, resultFindAll.getValue().get().length + 1);
		resultContains(foundModules, TEST_MODULE_1.name.getNonNull());
		verifyFindAll(foundModules, findAllByJDBC());
	}
	
	@Test
	void testFindAllIncludesAllRelevantAttributes() throws IOException {
		final Result<ModulePojo[]> result = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		for (final ModulePojo module : result.getValue().get()) {
			if (TEST_MODULE_NAME.equals(module.getName())) {
				validateTestModule(module);
				return;
			}
		}
	}
	
	@Test
	void testCountWithNewModule() throws IOException {
		final Result<Long> resultCount = moduleServiceProvider.getModuleCount().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, resultCount.getStatusCode());
		createModule(ONE, TEST_MODULE_1);
		createModule(TWO, TEST_MODULE_2);
		final Result<Long> resultCount2 = moduleServiceProvider.getModuleCount().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, resultCount2.getStatusCode());
		assertEquals(resultCount2.getValue().get().longValue(), resultCount.getValue().get().longValue() + 1);
	}
	
	@Test
	void testCountNonExisting() throws IOException {
		final Result<Long> resultCount = moduleServiceProvider.getModuleCount().setProjectId(NON_EXISTING_ID).execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, resultCount.getStatusCode());
	}
	
	@Test
	void testCountProjectWithoutModules() throws IOException {
		final ProjectServiceProvider projectServiceProvider = new ProjectServiceProvider(getConnectionInfo());
		final Result<ProjectPojo> resultCreate = projectServiceProvider.createProject().setProject(new ProjectPojoPrototype()
				.setName("NEW PROJECT")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet())
			).execute();
		assertEquals(HttpStatus.SC_CREATED,resultCreate.getStatusCode());
		final Result<Long> resultCount = moduleServiceProvider.getModuleCount().setProjectId(resultCreate.getValue().get().getId()).execute();
		assertEquals(HttpStatus.SC_OK, resultCount.getStatusCode());
		assertEquals(0, resultCount.getValue().get().longValue());
	}
	
	@Test
	void testFindByPath() throws IOException {
		final Result<ModulePojo> result = moduleServiceProvider.findModuleByPath().setProjectId(ONE).setPath("src-natural/LibA/PRG1.nsp").execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		verifyNumberOfCustomProperties(result.getValue().get(), 2);
	}
	
	@Test
	void findByPathIncludesAllRelevantAttributes() throws IOException {
		final Result<ModulePojo> result = moduleServiceProvider.findModuleByPath().setPath("src/cobol/programs/MMRS7101.cbl").setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final var module = result.getValue().get();
		validateTestModule(module);
	}
	
	@Test
	void testCreateModuleWithDuplicatePath() throws IOException {
		final var newModule = new ModulePojoPrototype();
		newModule.setName("NEW MODULE 1");
		newModule.setProject(ONE);
		newModule.setTechnology(Technology.NATURAL);
		newModule.setType(Type.PROGRAM);
		newModule.setStorage(Storage.FILE);
		newModule.setIdentification(Identification.IDENTIFIED);
		newModule.setOrigin(Origin.CUSTOM);
		newModule.setPath("src-natural/LibA/PRG1.nsp");
		newModule.setCreator(Creator.DISCOVERY);
		
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(newModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
	}
	
	@Test
	void testCreateModuleWithDuplicatePathAndDifferentFileSection() throws IOException {
		final var newModule = new ModulePojoPrototype();
		newModule.setName("NEW MODULE 1");
		newModule.setProject(ONE);
		newModule.setTechnology(Technology.NATURAL);
		newModule.setType(Type.PROGRAM);
		newModule.setStorage(Storage.FILE_SECTION);
		newModule.setIdentification(Identification.IDENTIFIED);
		newModule.setOrigin(Origin.CUSTOM);
		newModule.setPath("src-natural/LibA/PRG1.nsp");
		newModule.setCreator(Creator.DISCOVERY);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(newModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
	}
	
	@Test
	void testFindByPathNonExisting() throws IOException {
		final Result<?> result = moduleServiceProvider.findModuleByPath().setProjectId(ONE).setPath("i/am/a/non/existing/path.nsp").execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}

	@Test
	void testFindAnnotationByModule() throws IOException {
		final Result<AnnotationPojo[]> result = moduleServiceProvider.findAnnotationsByModule().setProjectId(ONE).setModuleId(FIRST_CUSTOM_MODULE_ID).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final var annotations = result.getValue().get();
		AnnotationServiceTest.verifyFindAll(annotations, findAllAnnotationsByDao(FIRST_CUSTOM_MODULE_ID));
		assertEquals(4, annotations.length);
		//assert that the NIDs are 1 - 4
		assertTrue(annotations[0].identity().getNid() <= 4);
		assertEquals(annotations[0].getCreatedByUserId(), "admin");
	}
	
	@Test
	void testFindAnnotationByModuleNonExisting() throws IOException {
		final Result<AnnotationPojo[]> result = moduleServiceProvider.findAnnotationsByModule().setProjectId(ONE).setModuleId(NON_EXISTING_ID).execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testFindTaxonomiesByModule() throws IOException {
		final Result<TaxonomyPojo[]> result = moduleServiceProvider.findTaxonomiesByModule().setProjectId(ONE).setModuleId(FIRST_CUSTOM_MODULE_ID).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		Optional<TaxonomyPojo[]> value = result.getValue();
		assertTrue(value.isPresent());
		
		List<String> taxonomyPojos = value.map(Stream::of).get()
				.map(TaxonomyPojo::getName)
				.collect(Collectors.toList());
		
		//but none assigned to the module
		assertThat(taxonomyPojos, Matchers.containsInAnyOrder("ARB100", "Employee domain"));
	}
	
	@Test
	void testFindTaxonomiesByModuleNoData() throws IOException {
		final Result<TaxonomyPojo[]> result = moduleServiceProvider.findTaxonomiesByModule().setProjectId(ONE).setModuleId(CUSTOM_MODULE_WITH_DD_ID).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		assertEquals(0, result.getValue().get().length);
	}
	
	@Test
	void testCreateModule() throws IOException {
		final ModulePojo createdModule = createModule(ONE, TEST_MODULE_1);
		verifyModuleWithoutIdAndRid(TEST_MODULE_1, createdModule);
		CustomProperties.verifyNumberOfCustomProperties(createdModule, NUMBER_OF_EXPECTED_CUSTOM_PROPERTIES);
	}

	@Test
	void testCreateModuleWithSourceMetrics() throws IOException {
		final ModulePojo createdModule = createModule(ONE, TEST_MODULE_6);
		verifyModuleWithoutIdAndRid(TEST_MODULE_6, createdModule);
		CustomProperties.verifyNumberOfCustomProperties(createdModule, NUMBER_OF_EXPECTED_CUSTOM_PROPERTIES);
	}
	
	@Test
	void testCreateModuleWithoutName() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setPath("src/cobol/program/TESTINCOMPLETE.cbl");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
		assertFalse(resultCreate.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleWithoutProject() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setPath("src/cobol/program/TESTINCOMPLETE.cbl");
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
		assertFalse(resultCreate.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleWithoutTechnology() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setPath("src/cobol/program/TESTINCOMPLETE.cbl");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
		assertFalse(resultCreate.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleWithoutType() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setPath("src/cobol/program/TESTINCOMPLETE.cbl");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
		assertFalse(resultCreate.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleWithoutStorage() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setPath("src/cobol/program/TESTINCOMPLETE.cbl");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
		assertFalse(resultCreate.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleWithoutIdentification() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setPath("src/cobol/program/TESTINCOMPLETE.cbl");
		incompleteModule.setProject(ONE);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, resultCreate.getStatusCode());
		assertFalse(resultCreate.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleWithAllRelevantAttributes() throws IOException {
		final var moduleToBeCreated = new ModulePojoPrototype();
		moduleToBeCreated.setName("TEST MODULE 1");
		moduleToBeCreated.setPath("src/cobol/programs/TESTMODULE1.cbl");
		moduleToBeCreated.setProject(ONE);
		moduleToBeCreated.setTechnology(Technology.COBOL);
		moduleToBeCreated.setType(Type.PROGRAM);
		moduleToBeCreated.setStorage(Storage.FILE);
		moduleToBeCreated.setIdentification(Identification.IDENTIFIED);
		moduleToBeCreated.setOrigin(Origin.CUSTOM);
		moduleToBeCreated.setContent("Test Data");
		final String expectedDescription = "test description for newly created module";
		moduleToBeCreated.setDescription(expectedDescription);

		final var sourceMetrics = new SourceMetricsPojoPrototype()
				.setCodeLines(1).setCodeLines(2).setComplexityMcCabe(3).setDeadCodeLines(4);
		moduleToBeCreated.setSourceMetrics(sourceMetrics);

		final Result<ModulePojo> creationResult = moduleServiceProvider.createModule().setProjectId(ONE).setModule(moduleToBeCreated).execute();
		assertEquals(HttpStatus.SC_CREATED, creationResult.getStatusCode());
		assertTrue(creationResult.getValue().isPresent());
		final var createdModule = creationResult.getValue().get();
		verifyModuleWithoutIdAndRid(moduleToBeCreated, createdModule);
	}
	
	@Test
	void testCreateModuleWithNegativeIntegerValuesHasThoseValuesAsNull() throws IOException {
		final var moduleToBeCreated = new ModulePojoPrototype();
		moduleToBeCreated.setName("TEST MODULE 1");
		moduleToBeCreated.setPath("src/cobol/programs/TESTMODULE1.cbl");
		moduleToBeCreated.setProject(ONE);
		moduleToBeCreated.setTechnology(Technology.COBOL);
		moduleToBeCreated.setType(Type.PROGRAM);
		moduleToBeCreated.setStorage(Storage.FILE);
		moduleToBeCreated.setIdentification(Identification.IDENTIFIED);
		moduleToBeCreated.setOrigin(Origin.CUSTOM);
		moduleToBeCreated.setContent("Test Data");
		moduleToBeCreated.setCreator(Creator.DISCOVERY);
		final String expectedDescription = "test description for newly created module";
		moduleToBeCreated.setDescription(expectedDescription);

		final var sourceMetrics = new SourceMetricsPojoPrototype()
				.setCodeLines(-1).setCodeLines(-2).setComplexityMcCabe(-3).setDeadCodeLines(-4);
		moduleToBeCreated.setSourceMetrics(sourceMetrics);

		final Result<ModulePojo> creationResult = moduleServiceProvider.createModule().setProjectId(ONE).setModule(moduleToBeCreated).execute();
		assertEquals(HttpStatus.SC_CREATED, creationResult.getStatusCode());
		assertTrue(creationResult.getValue().isPresent());
		final var createdModule = creationResult.getValue().get();
		verifyIntegerValuesAreNull(createdModule);
	}
	
	@Test
	void testCreateModuleForJclInclude() throws IOException {
		final var moduleToBeCreated = new ModulePojoPrototype();
		moduleToBeCreated.setName("ABEND");
		moduleToBeCreated.setPath("src/jcl/procs/ABEND");
		moduleToBeCreated.setProject(ONE);
		moduleToBeCreated.setTechnology(Technology.JCL);
		moduleToBeCreated.setType(Type.INCLUDE);
		moduleToBeCreated.setStorage(Storage.FILE);
		moduleToBeCreated.setIdentification(Identification.IDENTIFIED);
		moduleToBeCreated.setOrigin(Origin.CUSTOM);
		moduleToBeCreated.setContent("Test Data");
		moduleToBeCreated.setCreator(Creator.DISCOVERY);
		
		final Result<ModulePojo> creationResult = moduleServiceProvider.createModule().setProjectId(ONE).setModule(moduleToBeCreated).execute();
		assertEquals(HttpStatus.SC_CREATED, creationResult.getStatusCode());
		assertTrue(creationResult.getValue().isPresent());
	}
	
	@Test
	void testCreateModuleForJclIncludeWithoutContent() throws IOException {
		final var moduleToBeCreated = new ModulePojoPrototype();
		moduleToBeCreated.setName("ABEND");
		moduleToBeCreated.setPath("src/jcl/procs/ABEND");
		moduleToBeCreated.setProject(ONE);
		moduleToBeCreated.setTechnology(Technology.JCL);
		moduleToBeCreated.setType(Type.INCLUDE);
		moduleToBeCreated.setStorage(Storage.FILE);
		moduleToBeCreated.setIdentification(Identification.IDENTIFIED);
		moduleToBeCreated.setOrigin(Origin.CUSTOM);
		
		final Result<ModulePojo> creationResult = moduleServiceProvider.createModule().setProjectId(ONE).setModule(moduleToBeCreated).execute();
		assertEquals(HttpStatus.SC_CREATED, creationResult.getStatusCode());
		assertTrue(creationResult.getValue().isPresent());
	}
	
	@Test
	void testCreateBasicProgramModule() throws IOException {
		final var moduleToBeCreated = new ModulePojoPrototype();
		moduleToBeCreated.setName("HELLO WORLD");
		moduleToBeCreated.setPath("src/basic/programs/hello.bas");
		moduleToBeCreated.setProject(ONE);
		moduleToBeCreated.setTechnology(Technology.BASIC);
		moduleToBeCreated.setType(Type.PROGRAM);
		moduleToBeCreated.setStorage(Storage.FILE);
		moduleToBeCreated.setIdentification(Identification.IDENTIFIED);
		moduleToBeCreated.setOrigin(Origin.CUSTOM);
		moduleToBeCreated.setContent("Test Data");
		
		final Result<ModulePojo> creationResult = moduleServiceProvider.createModule().setProjectId(ONE).setModule(moduleToBeCreated).execute();
		assertEquals(HttpStatus.SC_CREATED, creationResult.getStatusCode());
		assertTrue(creationResult.getValue().isPresent());
	}

	
	@Test
	@SuppressWarnings("unchecked")
	void testUpdateModule() throws IOException {
		final ModulePojoPrototype newModule = new ModulePojoPrototype();
		newModule.setName("TEST MODULE 1");
		newModule.setPath("src/cobol/programs/TESTMODULE1.cbl");
		newModule.setProject(ONE);
		newModule.setTechnology(Technology.COBOL);
		newModule.setType(Type.PROGRAM);
		newModule.setStorage(Storage.FILE);
		newModule.setIdentification(Identification.IDENTIFIED);
		newModule.setOrigin(Origin.CUSTOM);
		newModule.setContent("Test Data");
		newModule.setCustomProperties(new HashMap<>(Map.of(CustomPropertyClass.ModuleCustomProperties.name(),
				new HashMap<>(Map.of("customMetaInfo2", "Created custom value")))));

		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(newModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_CREATED, resultCreate.getStatusCode());
		final ModulePojo resultPojo = resultCreate.getValue().get();
		final EntityId createdId = resultPojo.identity();
		final CustomPropertiesMap props = resultPojo.getCustomProperties();
		final ModulePojoPrototype newModule2 = new ModulePojoPrototype()
				.withId(createdId)
				.setName("NEW NAME")
				.setTechnology(Technology.NATURAL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.DATABASE)
				.setIdentification(Identification.MISSING)
				.setOrigin(Origin.ENVIRONMENT)
				.setContent("updated source contents")
				.setCustomProperties(props);
		final Map<String, String> oldProps = (Map<String, String>) props.get(CustomPropertyClass.ModuleCustomProperties.name());
		oldProps.put("customMetaInfo2", "Updated value");
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(newModule2).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, resultUpdate.getStatusCode());
		final ModulePojo updatedModule = resultUpdate.getValue().get();

		assertEquals(resultPojo.identity(), updatedModule.identity());
		assertEquals(resultPojo.getPath(), updatedModule.getPath());
		assertEquals("NEW NAME", updatedModule.getName());

		verifyNumberOfCustomProperties(updatedModule, NUMBER_OF_EXPECTED_CUSTOM_PROPERTIES);
		final Object updatedProperty = getCustomPropertyByName("customMetaInfo2", updatedModule);
		assertEquals("Updated value", updatedProperty);
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testUpdateModuleWithCustomProperties() throws IOException {
		final Result<ModulePojo[]> resultInitialFind = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName("IDCAMS").execute();
		assertEquals(HttpStatus.SC_OK, resultInitialFind.getStatusCode());
		final ModulePojo expected = resultInitialFind.getValue().get()[0];
		final CustomPropertiesMap customPropertiesMap = expected.getCustomProperties();
		Map<String, String> customProperties = (Map<String, String>) customPropertiesMap.get("ModuleCustomProperties");
		customProperties.put("customMetaInfo2","New value 0");
		ModulePojoPrototype proto = new ModulePojoPrototype()
				.withId(expected.identity())
				.setCustomProperties(customPropertiesMap);
		
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(proto).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, resultUpdate.getStatusCode());
		
		final Result<ModulePojo[]> resultAfterUpdateFind = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName("IDCAMS").execute();
		assertEquals(HttpStatus.SC_OK, resultAfterUpdateFind.getStatusCode());
		final ModulePojo expectedAfterUpdate = resultAfterUpdateFind.getValue().get()[0];
		Map<String, String> customPropertiesAfterUpdate = (Map<String, String>) expectedAfterUpdate.getCustomProperties().get("ModuleCustomProperties");
		assertEquals("New value 0", customPropertiesAfterUpdate.get("customMetaInfo2"));
	}
	
	@Test
	void testUpdateModuleWithoutName() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		//Update is fine with an incomplete module, if not new
		assertEquals(HttpStatus.SC_OK, resultUpdate.getStatusCode());
		assertTrue(resultUpdate.getValue().isPresent());
	}
	
	@Test
	void testUpdateModuleWithoutProject() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		//Update is fine with an incomplete module, if not new
		assertEquals(HttpStatus.SC_OK, resultUpdate.getStatusCode());
		assertTrue(resultUpdate.getValue().isPresent());
	}
	@Test
	void testUpdateModuleWithoutProjectMissingModule() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(123123L);
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		//Update is fine with an incomplete module, if not new
		assertEquals(HttpStatus.SC_NOT_FOUND, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateModuleWithoutTechnology() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		//updating incomplete modules is fine
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(incompleteModule.name.get(), resultUpdate.getValue().get().getName());
	}
	
	@Test
	void testUpdateModuleWithoutType() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setStorage(Storage.FILE);
		//updating incomplete modules is fine
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(incompleteModule.name.get(), resultUpdate.getValue().get().getName());

	}
	
	@Test
	void testUpdateModuleWithoutStorage() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		//updating incomplete modules is fine
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(incompleteModule.name.get(), resultUpdate.getValue().get().getName());
	}
	
	@Test
	void testUpdateModuleWithoutIdentification() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setNid(FIRST_CUSTOM_MODULE_ID.getNid());
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setProject(ONE);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		//updating incomplete modules is fine
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(incompleteModule.name.get(), resultUpdate.getValue().get().getName());
	}
	
	@Test
	void testUpdateModuleWithoutId() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		try {
			moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
			Assert.fail("Expected Exception");
		} catch (IllegalStateException e) {
			assertEquals("Module numeric or unique id must be set.", e.getMessage());
		}
	}

	@Test
	void testUpdateModuleWithNonExistingId() throws IOException {
		final var incompleteModule = new ModulePojoPrototype();
		incompleteModule.setName("TEST INCOMPLETE");
		incompleteModule.setProject(ONE);
		incompleteModule.setIdentification(Identification.IDENTIFIED);
		incompleteModule.setTechnology(Technology.COBOL);
		incompleteModule.setType(Type.PROGRAM);
		incompleteModule.setStorage(Storage.FILE);
		incompleteModule.setNid(NON_EXISTING_ID.getNid());
		final Result<ModulePojo> resultUpdate = moduleServiceProvider.updateModule().setModule(incompleteModule).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}
	
	@Test
	void testUpdateModuleWithAllRelevantAttributes() throws IOException {
		final Result<ModulePojo> preUpdateResult = moduleServiceProvider.findModuleByPath().setPath("src/cobol/programs/MMRS7101.cbl").setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, preUpdateResult.getStatusCode());
		assertTrue(preUpdateResult.getValue().isPresent());
		final var preUpdateModule = preUpdateResult.getValue().get();
		validateTestModule(preUpdateModule);
		
		/* verify SourceMetrics was received properly */
		final var sourceMetrics = preUpdateModule.getSourceMetrics().get();
		assertNull(sourceMetrics.getPhysicalLines());
		assertEquals(Integer.valueOf(107), sourceMetrics.getCodeLines());
		assertEquals(Integer.valueOf(46), sourceMetrics.getCommentLines());
		assertEquals(null, sourceMetrics.getDeadCodeLines());
		assertEquals(Integer.valueOf(3), sourceMetrics.getComplexityMcCabe());

		final String expectedDescription = "updated description for module";
		final var updateModule = new ModulePojoPrototype()
				.withId(preUpdateModule.identity())
				.setDescription(expectedDescription)
				.setSourceMetrics(new SourceMetricsPojoPrototype()
						.setCodeLines(Integer.valueOf(42))
						.setCommentLines(Integer.valueOf(24))
						.setComplexityMcCabe(Integer.valueOf(84))
						.setDeadCodeLines(Integer.valueOf(85))
					);

		final Result<ModulePojo> postUpdateResult = moduleServiceProvider.updateModule().setProjectId(ONE).setModule(updateModule).execute();
		assertEquals(HttpStatus.SC_OK, postUpdateResult.getStatusCode());
		assertTrue(postUpdateResult.getValue().isPresent());
		final var postUpdateModule = postUpdateResult.getValue().get();
		assertEquals("updated description for module", postUpdateModule.getDescription().orElse(null));
		assertEquals(preUpdateModule.getName(), postUpdateModule.getName());

		final Result<ModulePojo> postUpdateFindResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/MMRS7101.cbl")
				.setProjectId(ONE)
				.execute();
		assertEquals(HttpStatus.SC_OK, postUpdateFindResult.getStatusCode());
		assertTrue(postUpdateFindResult.getValue().isPresent());
		final var postUpdateFindModule = postUpdateFindResult.getValue().get();
		verifyModuleWithoutIdAndRid(postUpdateModule, postUpdateFindModule);
	}

	@Test
	void testUpdateModuleWithNegativeIntegerValuesHasThoseValuesAsNull() throws IOException {
		final Result<ModulePojo> preUpdateResult = moduleServiceProvider.findModuleByPath().setPath("src/cobol/programs/MMRS7101.cbl").setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, preUpdateResult.getStatusCode());
		assertTrue(preUpdateResult.getValue().isPresent());
		final ModulePojo preUpdateModule = preUpdateResult.getValue().get();
		validateTestModule(preUpdateModule);

		/* verify SourceMetrics was received properly */
		final var sourceMetrics = preUpdateModule.getSourceMetrics().get();
		assertNull(sourceMetrics.getPhysicalLines());
		assertEquals(Integer.valueOf(107), sourceMetrics.getCodeLines());
		assertEquals(Integer.valueOf(46), sourceMetrics.getCommentLines());
		assertEquals(null, sourceMetrics.getDeadCodeLines());
		assertEquals(Integer.valueOf(3), sourceMetrics.getComplexityMcCabe());

		final String expectedDescription = "updated description for module";
		final var updateModule = new ModulePojoPrototype()
				.withId(preUpdateModule.identity())
				.setDescription(expectedDescription)
				.setSourceMetrics(new SourceMetricsPojoPrototype()
						.setCodeLines(Integer.valueOf(-34))
						.setCommentLines(Integer.valueOf(-1))
						.setComplexityMcCabe(Integer.valueOf(-1))
						.setDeadCodeLines(Integer.valueOf(-85))
					);

		final Result<ModulePojo> postUpdateResult = moduleServiceProvider.updateModule().setProjectId(ONE)
				.setModule(updateModule).execute();
		assertEquals(HttpStatus.SC_OK, preUpdateResult.getStatusCode());
		assertTrue(preUpdateResult.getValue().isPresent());
		final var postUpdateModule = postUpdateResult.getValue().get();
		verifyIntegerValuesAreNull(postUpdateModule);

		final Result<ModulePojo> postUpdateFindResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/MMRS7101.cbl")
				.setProjectId(ONE)
				.execute();
		assertEquals(HttpStatus.SC_OK, postUpdateFindResult.getStatusCode());
		assertTrue(postUpdateFindResult.getValue().isPresent());
		final var postUpdateFindModule = postUpdateFindResult.getValue().get();
		verifyIntegerValuesAreNull(postUpdateFindModule);
	}

	@Test
	void testDeleteModule() throws IOException {
		final long countBefore = moduleServiceProvider.getModuleCount().setProjectId(ONE).execute().getValue().get().longValue();
		final Result<Void> resultDelete = moduleServiceProvider.deleteModule().setProjectId(ONE).setModuleId(FIRST_CUSTOM_MODULE_ID).execute();
		final long countAfter = moduleServiceProvider.getModuleCount().setProjectId(ONE).execute().getValue().get().longValue();
		assertEquals(HttpStatus.SC_NO_CONTENT, resultDelete.getStatusCode());
		assertEquals(countBefore, countAfter + 1);
	}
	
	@Test
	void testDeleteModuleAlsoRemovesTheCorrespondingSourceAttachment() throws IOException {
		final long sourceAttachmentCountBeforeModuleCreation = getSourceAttachmentCount(ONE);
		final var createdModule = createModule(ONE, TEST_MODULE_1);
		final long sourceAttachmentCountAfterModuleCreation = getSourceAttachmentCount(ONE);
		assertEquals(sourceAttachmentCountAfterModuleCreation, sourceAttachmentCountBeforeModuleCreation + 1);
		final Result<Void> resultDelete = moduleServiceProvider.deleteModule().setProjectId(ONE).setModuleId(createdModule.identity()).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, resultDelete.getStatusCode());
		final long sourceAttachmentCountAfterModuleDeletion = getSourceAttachmentCount(ONE);
		assertEquals(sourceAttachmentCountBeforeModuleCreation, sourceAttachmentCountAfterModuleDeletion);
	}
	
	@Test
	void testDeleteModuleNonExisting() throws IOException {
		final Result<Void> resultDelete = moduleServiceProvider.deleteModule().setProjectId(ONE).setModuleId(NON_EXISTING_ID).execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, resultDelete.getStatusCode());
	}
	
	@Test
	void testDeleteModuleDifferentProject() throws IOException {
		final Result<Void> resultDelete = moduleServiceProvider.deleteModule().setProjectId(TWO).setModuleId(FIRST_CUSTOM_MODULE_ID).execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, resultDelete.getStatusCode());
	}
	
	@Test
	void testDeleteModuleWithJdbcVerification() throws IOException {
		final Result<ModulePojo[]> resultFindAll = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		final ModulePojo[] foundModules = resultFindAll.getValue().get();
		final ModulePojo newModule = createModule(ONE, TEST_MODULE_1);
		final Result<ModulePojo[]> resultFindAll2 = moduleServiceProvider.findAllModules().setProjectId(ONE).execute();
		assertEquals(foundModules.length, resultFindAll2.getValue().get().length - 1);
		resultContains(foundModules, TEST_MODULE_1.name.getNonNull());
		final Result<Void> resultDelete = moduleServiceProvider.deleteModule().setProjectId(ONE).setModuleId(newModule.identity()).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, resultDelete.getStatusCode());
		verifyFindAll(foundModules, findAllByJDBC());
	}
	
	@Test
	void testTraversalDependencyGraphLinks() throws IOException {
		final Result<DependencyGraph> resultTraverseDependencies = moduleServiceProvider.traverseDependencies()
				.setProjectId(ONE)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setMaxDepth(1L)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultTraverseDependencies.getStatusCode());
		assertEquals(4, resultTraverseDependencies.getValue().get().getModules().size());
		assertEquals(3, resultTraverseDependencies.getValue().get().getReferences().size());
		assertEquals("CALLS", resultTraverseDependencies.getValue().get().getReferences().get(0).getRelationship().toString());
		assertEquals("CALLS", resultTraverseDependencies.getValue().get().getReferences().get(1).getRelationship().toString());
		assertEquals("CALLS", resultTraverseDependencies.getValue().get().getReferences().get(2).getRelationship().toString());
		final Result<DependencyGraph> resultTraverseDependenciesDepthTwo = moduleServiceProvider.traverseDependencies()
				.setProjectId(TWO)
				.setModuleId(EntityId.of(2004L))
				.setMaxDepth(1L)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultTraverseDependenciesDepthTwo.getStatusCode());
		assertEquals(1, resultTraverseDependenciesDepthTwo.getValue().get().getModules().size());
		assertEquals(0, resultTraverseDependenciesDepthTwo.getValue().get().getReferences().size());
		final Result<DependencyGraph> resultTraverseDependencies3 = moduleServiceProvider.traverseDependencies()
				.setProjectId(ONE)
				.setModuleId(EntityId.of(7L))
				.setMaxDepth(1L)
				.execute();
		assertEquals(HttpStatus.SC_NOT_FOUND, resultTraverseDependencies3.getStatusCode());
		final Result<DependencyGraph> resultTraverseDepMaxDepthTwo = moduleServiceProvider.traverseDependencies()
				.setProjectId(ONE)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setMaxDepth(2L)
				.execute();
		final List<ModuleRelationshipPojo> resultTraverserModuleLinkListDepthTwo = resultTraverseDepMaxDepthTwo.getValue().get().getReferences();
		assertEquals(HttpStatus.SC_OK, resultTraverseDepMaxDepthTwo.getStatusCode());
		assertEquals(7, resultTraverseDepMaxDepthTwo.getValue().get().getModules().size());
		assertEquals(6, resultTraverserModuleLinkListDepthTwo.size());
		final Result<DependencyGraph> resultTraverseDepMaxDepthThree = moduleServiceProvider.traverseDependencies()
				.setProjectId(ONE)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setMaxDepth(3L)
				.execute();
		final List<ModuleRelationshipPojo> resultTraverserModuleLinkList3 = resultTraverseDepMaxDepthThree.getValue().get().getReferences();
		assertEquals(HttpStatus.SC_OK, resultTraverseDepMaxDepthThree.getStatusCode());
		assertEquals(9, resultTraverseDepMaxDepthThree.getValue().get().getModules().size());
		assertEquals(8, resultTraverserModuleLinkList3.size());

		final Result<ModulePojo[]> resultIdcams = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName("IDCAMS").execute();
		assertEquals(HttpStatus.SC_OK, resultIdcams.getStatusCode());
		assertTrue(resultIdcams.getValue().isPresent());
		final var idcams = resultIdcams.getValue().get()[0];
		
		final List<Tuple2<Long, Long>> expectedPairs = new ArrayList<>();
		expectedPairs.add(Tuple2.of(FIRST_CUSTOM_MODULE_ID.getNid(), 2001L));
		expectedPairs.add(Tuple2.of(FIRST_CUSTOM_MODULE_ID.getNid(), 2002L));
		expectedPairs.add(Tuple2.of(2002L, idcams.getId()));
		expectedPairs.add(Tuple2.of(FIRST_CUSTOM_MODULE_ID.getNid(), 2017L));
		expectedPairs.add(Tuple2.of(2017L, 2018L));
		expectedPairs.add(Tuple2.of(2017L, 2019L));
		assertReferencePairs(expectedPairs, resultTraverserModuleLinkListDepthTwo);
	}
	
	@Test
	void testTraverseDependenciesWithNodeTypeFilterQuery() throws IOException {
		final String graphFilterOptions = "modules=in=(JCL_EXEC_PGM)";
		final Result<DependencyGraph> graphResult = moduleServiceProvider.traverseDependencies()
																				.setProjectId(ONE)
																				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
																				.setMaxDepth(2L)
																				.setGraphFilterOptions(graphFilterOptions)
																				.execute();
		assertEquals(HttpStatus.SC_OK, graphResult.getStatusCode());
		final DependencyGraph graph = graphResult.getValue().get();
		final int graphModuleSize = graph.getModules().size();
		assertEquals(4, graphModuleSize);
		
		boolean isJclExecPgm = false;
		for (final var module : graph.getModules()) {
			if (Type.EXEC_PGM.equals(module.getType())) {
				isJclExecPgm = true;
			}
		}
		
		assertFalse(isJclExecPgm);
	}
	
	@Test
	void testTraverseDependenciesWithEdgeTypeFilterQuery() throws IOException {
		final String graphFilterOptions = "relationships=in=(CALLS)";
		final Result<DependencyGraph> graphResult = moduleServiceProvider.traverseDependencies()
																				.setProjectId(ONE)
																				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
																				.setMaxDepth(2L)
																				.setGraphFilterOptions(graphFilterOptions)
																				.execute();
		assertEquals(HttpStatus.SC_OK, graphResult.getStatusCode());
		final DependencyGraph graph = graphResult.getValue().get();
		final int graphModuleSize = graph.getModules().size();
		assertEquals(1, graphModuleSize);
		assertEquals(0, graph.getReferences().size());
		
		assertEquals(CUSTOM_MODULE_WITH_DD_ID, graph.getModules().get(0).getId());
	}
	
	@Test
	void testCreateModuleWithEmptyName() throws IOException {
		final Result<ModulePojo> createEmpty = moduleServiceProvider
				.createModule()
				.setProjectId(ONE)
				.setModule(new ModulePojoPrototype()
						.setName("")
						.setPath("src/firstModule.cbl")
						.setProject(ONE)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API)
						.setContent("Content for first module"))
				.execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, createEmpty.getStatusCode());
	}
	
	@Test
	void testCreateModuleWithSpacesAsName() throws IOException {
		final Result<ModulePojo> createSpaces = moduleServiceProvider
				.createModule()
				.setProjectId(ONE)
				.setModule(new ModulePojoPrototype()
						.setName("    ")
						.setPath("src/secondModule.cbl")
						.setProject(ONE)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API)
						.setContent("Content for first module"))
				.execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, createSpaces.getStatusCode());
	}
	
	@Test
	void testUpdateModuleWithEmptyName() throws IOException {
		final Result<ModulePojo> findModule = moduleServiceProvider
				.findModuleById()
				.setProjectId(ONE)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.execute();
		assertEquals(HttpStatus.SC_OK, findModule.getStatusCode());
		final var module = findModule.getValue().get();
		final Result<ModulePojo> updateModule = moduleServiceProvider
				.updateModule()
				.setProjectId(ONE)
				.setModule(new ModulePojoPrototype().withId(module.identity()).setName(""))
				.execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, updateModule.getStatusCode());
	}
	
	@Test
	void testUpdateModuleWithSpacesAsName() throws IOException {
		final Result<ModulePojo> findModule = moduleServiceProvider
				.findModuleById()
				.setProjectId(ONE)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.execute();
		assertEquals(HttpStatus.SC_OK, findModule.getStatusCode());
		final var module = findModule.getValue().get();
		final Result<ModulePojo> updateModule = moduleServiceProvider
				.updateModule()
				.setProjectId(ONE)
				.setModule(new ModulePojoPrototype().withId(module.identity()).setName("   "))
				.execute();
		assertEquals(HttpStatus.SC_BAD_REQUEST, updateModule.getStatusCode());
	}
	
	@Test
	void testFindByIdIncludesContent() throws IOException {
		final Result<ModulePojo[]> resultPRG1 = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName("PRG1").execute();
		assertEquals(HttpStatus.SC_OK, resultPRG1.getStatusCode());
		assertTrue(resultPRG1.getValue().isPresent());
		final ModulePojo[] modulePRG1Arr = resultPRG1.getValue().get();
		assertEquals(1, modulePRG1Arr.length);
		final var modulePRG1 = modulePRG1Arr[0];
		
		
		final Result<ModulePojo[]> resultCC2 = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName("CC2").execute();
		assertEquals(HttpStatus.SC_OK, resultCC2.getStatusCode());
		assertTrue(resultCC2.getValue().isPresent());
		final ModulePojo[] moduleCC2Arr = resultCC2.getValue().get();
		assertEquals(1, moduleCC2Arr.length);
		final var moduleCC2 = moduleCC2Arr[0];
		
		final Result<ModulePojo> resultPRG1WithContent = moduleServiceProvider.findModuleById()
				.setProjectId(ONE)
				.setModuleId(modulePRG1.identity())
				.setIncludeContent(true)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultPRG1WithContent.getStatusCode());
		assertTrue(resultPRG1WithContent.getValue().isPresent());
		
		final var modulePRG1WithContent = resultPRG1WithContent.getValue().get();
		assertTrue(modulePRG1WithContent.getContent().isEmpty());
		assertFalse(modulePRG1WithContent.isSourceCodeAvailable());
		
		final Result<ModulePojo> resultCC2WithContent = moduleServiceProvider.findModuleById()
				.setProjectId(ONE)
				.setModuleId(moduleCC2.identity())
				.setIncludeContent(true)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultCC2WithContent.getStatusCode());
		assertTrue(resultCC2WithContent.getValue().isPresent());
		
		final var moduleCC2WithContent = resultCC2WithContent.getValue().get();
		final String content = moduleCC2WithContent.getContent().get();
		assertEquals("DISPLAY 'CC2'", content.trim());
		assertTrue(moduleCC2WithContent.isSourceCodeAvailable());
		
		final Result<ModulePojo> resultCC2WithoutContent = moduleServiceProvider.findModuleById()
				.setProjectId(ONE)
				.setModuleId(moduleCC2.identity())
				.execute();
		assertEquals(HttpStatus.SC_OK, resultCC2WithoutContent.getStatusCode());
		assertTrue(resultCC2WithoutContent.getValue().isPresent());
		
		final var moduleCC2WithoutContent = resultCC2WithoutContent.getValue().get();
		assertTrue(moduleCC2WithoutContent.getContent().isEmpty());
		assertTrue(moduleCC2WithoutContent.isSourceCodeAvailable());
	}
	
	private Optional<AstNodePojo> queryHasAst(EntityId moduleId) {
		final var astDao = new AstPgDao(getDataSource());
		final var node = astDao.findAny(q -> q.withRelationshipToModule(moduleId, AstModuleRelationshipType.ROOT));
		return node;
	}
	
	@Test
	void testStoreAstForModule() throws IOException {
		final Result<ModulePojo[]> resultEXECSQL = moduleServiceProvider
				.findModuleByName()
				.setProjectId(ONE)
				.setName("EXECSQL")
				.execute();
		assertNotNull(resultEXECSQL);
		assertEquals(HttpStatus.SC_OK, resultEXECSQL.getStatusCode());
		final ModulePojo[] moduleEXECSQLArr = resultEXECSQL.getValue().get();
		assertEquals(1, moduleEXECSQLArr.length);
		final var moduleEXECSQL = moduleEXECSQLArr[0];
		final var moduleIdEXECSQL = moduleEXECSQL.identity();

		/* The module initially should not have any AST Nodes generated for it. */
		assertTrue(queryHasAst(moduleIdEXECSQL).isEmpty());

		final StoreAstNodes storeAstService = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(ONE)
				.setModuleId(moduleIdEXECSQL);

		final Result<Boolean> resultAstEXECSQL = storeAstService.execute();
		assertNotNull(resultAstEXECSQL);
		assertEquals(HttpStatus.SC_ACCEPTED, resultAstEXECSQL.getStatusCode());
		final Boolean storeAstResultEXECSQL = resultAstEXECSQL.getValue().get();
		assertTrue(storeAstResultEXECSQL.booleanValue());

		final var edgeId = queryHasAst(moduleIdEXECSQL);
		assertFalse(edgeId.isEmpty());

		/* Calling the ModuleServiceProvider#storeAstNodes() again to ensure
		 * that the service does not store the AST Nodes again if they already exist. */
		final Result<Boolean> resultAstDuplicateEXECSQL = storeAstService.execute();
		assertNotNull(resultAstDuplicateEXECSQL);
		assertEquals(HttpStatus.SC_ACCEPTED, resultAstDuplicateEXECSQL.getStatusCode());
		final Boolean duplicateAstEdgeIdEXECSQL = resultAstDuplicateEXECSQL.getValue().get();
		assertTrue(duplicateAstEdgeIdEXECSQL.booleanValue());

		final var duplicateEdgeId = queryHasAst(moduleIdEXECSQL);
		assertFalse(duplicateEdgeId.isEmpty());

		/* The service should return the same HasAst Edge ID. */
		assertEquals(edgeId.get().getId(), duplicateEdgeId.get().getId());
	}
	
	/**
	 * Tests the retrieval of linked modules from a given module of a given path.
	 *
	 * @throws IOException if error occurs during IO operation
	 */
	@Test
	void testFindAllLinkedModules() throws IOException {
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final Result<ModulePojo[]> moduleResult = moduleService.findModuleByName().setProjectId(ONE).setName("EXECSQL").execute();
		assertEquals(200, moduleResult.getStatusCode());
		assertTrue(moduleResult.getValue().isPresent());
		final var module = moduleResult.getValue().get()[0];
		final String path = module.getPath().get();
		final Result<List<LinkedModule>> linkedModuleResult = moduleService.findAllLinkedModules()
				.setPath(path)
				.setProjectId(ONE).execute();
		assertEquals(200, linkedModuleResult.getStatusCode());
		assertTrue(linkedModuleResult.getValue().isPresent());
		final List<LinkedModule> linkedModules = linkedModuleResult.getValue().get();
		final LinkedModule linkedModule = linkedModules.get(0);
		assertNotNull(linkedModule);
		final ModuleLocation fromLocation = linkedModule.getFromLocation();
		assertNotNull(fromLocation);
		final ModuleLocation toLocation = linkedModule.getToLocation();
		assertNotNull(toLocation);
		assertEquals(100, fromLocation.getOffset().intValue());
		assertEquals(4, fromLocation.getLength().intValue());
		assertEquals(200, toLocation.getOffset().intValue());
		assertEquals(5, toLocation.getLength().intValue());
	}
	
	/**
	 * Tests the retrieval of linked modules from a given module of a given path using ContainsModule Relationship.
	 *
	 * @throws IOException if error occurs during IO operation
	 */
	@Test
	void testFindAllLinkedModules2() throws IOException {
		final ModuleServiceProvider moduleService = MiningApiClient.moduleService(getConnectionInfo());
		final Result<ModulePojo[]> moduleResult = moduleService.findModuleByName().setProjectId(THREE).setName("BasicModule2").execute();
		assertEquals(200, moduleResult.getStatusCode());
		assertTrue(moduleResult.getValue().isPresent());
		final var module = moduleResult.getValue().get()[0];
		final String path = module.getPath().get();
		final Result<List<LinkedModule>> linkedModuleResult = moduleService.findAllLinkedModules()
				.setPath(path)
				.setProjectId(THREE).execute();
		assertEquals(200, linkedModuleResult.getStatusCode());
		assertTrue(linkedModuleResult.getValue().isPresent());
		final List<LinkedModule> linkedModules = linkedModuleResult.getValue().get();
		final LinkedModule linkedModule = linkedModules.get(0);
		assertNotNull(linkedModule);
		final ModuleLocation fromLocation = linkedModule.getFromLocation();
		assertNotNull(fromLocation);
		final ModuleLocation toLocation = linkedModule.getToLocation();
		assertNotNull(toLocation);
		assertEquals(100, fromLocation.getOffset().intValue());
		assertEquals(4, fromLocation.getLength().intValue());
		assertEquals(200, toLocation.getOffset().intValue());
		assertEquals(5, toLocation.getLength().intValue());
	}
	
	/**
	 * Checks that the given pairs are represented in the given references.
	 * Both lists must match in size.
	 * Attention: This modifies the input lists to be empty.
	 *
	 * @param pairs the expected pairs of IDs
	 * @param references the actual references
	 */
	private void assertReferencePairs(final List<Tuple2<Long, Long>> pairs, final List<ModuleRelationshipPojo> references) {
		final Map<UUID, Long> nids = new HashMap<>();
		references.forEach(r -> {
			nids.computeIfAbsent(r.getSrcModule(), srcId -> Objects.requireNonNull(moduleDao)
					.findModuleIds(q -> q.byUid(srcId)).stream().findAny().orElseThrow().getNid());
			nids.computeIfAbsent(r.getDstModule(), dstId -> Objects.requireNonNull(moduleDao)
					.findModuleIds(q -> q.byUid(dstId)).stream().findAny().orElseThrow().getNid());
		});
		for (final var referenceIterator = references.iterator(); referenceIterator.hasNext();) {
			final var reference = referenceIterator.next();
			final var fromId = reference.getSrcModule();
			final var toId = reference.getDstModule();
			boolean pairExists = false;
			for (final var pairIterator = pairs.iterator(); pairIterator.hasNext();) {
				final var pair = pairIterator.next();
				if (pair.a.equals(nids.get(fromId)) && pair.b.equals(nids.get(toId))) {
					pairExists = true;
				}
			}
			assertTrue("Failed for pair: " + fromId + " " + toId, pairExists);
		}
	}

	private void verifyIntegerValuesAreNull(final ModulePojo module) {
		final var sourceMetrics = module.getSourceMetrics().get();
		assertNull(sourceMetrics.getCodeLines());
		assertNull(sourceMetrics.getCommentLines());
		assertNull(sourceMetrics.getComplexityMcCabe());
		assertNull(sourceMetrics.getPhysicalLines());
		assertEquals(Integer.valueOf(-1), sourceMetrics.getDeadCodeLines());
	}

	private void validateTestModule(final ModulePojo actual) {
		assertEquals(TEST_MODULE_DESCRIPTION, actual.getDescription().orElse(null));
		
		final var sourceMetrics = actual.getSourceMetrics().get();
		
		assertEquals(Integer.valueOf(107), sourceMetrics.getCodeLines());
		assertEquals(Integer.valueOf(46), sourceMetrics.getCommentLines());
		assertEquals(Integer.valueOf(3), sourceMetrics.getComplexityMcCabe());
	}

	private void verifyFindAll(final ModulePojo[] modules, final Map<Long, ModulePojo> databaseResult) {
		for (final var module : modules) {
			final Long id = module.getId();
			final var expected = databaseResult.get(id);
			assertNotNull(expected);
			verifyModuleAllFields(expected, module);
			databaseResult.remove(id);
		}
		assertTrue(databaseResult.isEmpty());
	}
	
	private void verifyModuleWithoutIdAndRid(final ModulePojo expected, final ModulePojo module) {
		verifyModuleAllFields(ModulePojoDummy.build(expected), module);
	}

	private void verifyModuleWithoutIdAndRid(final ModulePojoPrototype expected, final ModulePojo module) {
		try {
			final EntityId completeProjectId = projectServiceProvider.findProjectById()
					.setProjectId(module.getProject())
					.execute()
					.getValue()
					.get()
					.identity();
			assertEquals(expected.project.orElse(null), completeProjectId);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		assertEquals(expected.name.orElse(null), module.getName());
		assertEquals(expected.identification.orElse(null), module.getIdentification());
		assertEquals(expected.path.orElse(null), module.getPath().orElse(null));
		assertEquals(expected.storage.orElse(null), module.getStorage());
		assertEquals(expected.technology.orElse(null), module.getTechnology());
		assertEquals(expected.type.orElse(null), module.getType());
		assertEquals(expected.origin.orElse(null), module.getOrigin());
		assertEquals(expected.description.orElse(null), module.getDescription().orElse(null));

		final var sourceMetricsExpected = expected.sourceMetrics.orElse(null);
		final var sourceMetricsActual = module.getSourceMetrics().orElse(null);
		if (sourceMetricsExpected == null) {
			if (sourceMetricsActual != null) {
				assertNull(sourceMetricsActual.getCodeLines());
				assertNull(sourceMetricsActual.getCommentLines());
				assertNull(sourceMetricsActual.getComplexityMcCabe());
				assertNull(sourceMetricsActual.getPhysicalLines());
				assertEquals(Integer.valueOf(-1), sourceMetricsActual.getDeadCodeLines());
			}
		} else {
			assertNotNull(sourceMetricsActual);
			assertEquals(sourceMetricsExpected.codeLines.orElse(null), sourceMetricsActual.getCodeLines());
			assertEquals(sourceMetricsExpected.commentLines.orElse(null), sourceMetricsActual.getCommentLines());
			assertEquals(sourceMetricsExpected.complexityMcCabe.orElse(null), sourceMetricsActual.getComplexityMcCabe());
			assertEquals(sourceMetricsExpected.deadCodeLines.orElse(null), sourceMetricsActual.getDeadCodeLines());
			assertEquals(sourceMetricsExpected.physicalLines.orElse(null), sourceMetricsActual.getPhysicalLines());
		}
	}

	private void verifyModuleAllFields(final ModulePojo expected, final ModulePojo module) {
		assertEquals(expected.identity(), module.identity());
		verifyModuleAllFields(ModulePojoDummy.build(expected), module);
	}

	private void verifyModuleAllFields(final ModulePojoPrototype expected, final ModulePojo module) {
		verifyModuleWithoutIdAndRid(expected, module);
	}
	
	private boolean resultContains(final ModulePojo[] result, final String moduleName) {
		for (final var module : result) {
			if (moduleName.equals(module.getName())) {
				return true;
			}
		}
		return false;
	}

	private List<AnnotationPojo> findAllAnnotationsByDao(final EntityId moduleId) {
		return Objects.requireNonNull(annotataionDao).find(q -> q.ofModule(moduleId));
	}

	private Map<Long, ModulePojo> findAllByJDBC() {
		return Objects.requireNonNull(moduleDao).findModules(q -> q.ofProject(ONE)).stream().collect(Collectors.toMap(m -> m.getId(), m -> m));
	}
	
	private ModulePojo createModule(final EntityId projectId, final ModulePojoPrototype module) throws IOException {
		module.setCustomProperties(new HashMap<>(Map.of(CustomPropertyClass.ModuleCustomProperties.name(),new HashMap<>(Map.of("customMetaInfo2", "Created custom value")))));
		module.setContent("created source contents");
		final Result<ModulePojo> resultCreate = moduleServiceProvider.createModule().setModule(module).setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_CREATED, resultCreate.getStatusCode());
		assertTrue(resultCreate.getValue().isPresent());
		final ModulePojo createdModule = resultCreate.getValue().get();
		final Object createdCustomProperty = getCustomPropertyByName("customMetaInfo2", createdModule);
		assertEquals("Created custom value", createdCustomProperty);
		return createdModule;
	}

	private long getSourceAttachmentCount(final EntityId projectId) {
		try {
			if (connectionPostgres != null) {
				try (final PreparedStatement statement = connectionPostgres.prepareStatement(
						"SELECT count(*) as count FROM source WHERE id IN (SELECT source FROM Module WHERE project = (SELECT uid FROM project WHERE nid = ?))")) {
					statement.setLong(1, projectId.getNid());
					statement.execute();
					final ResultSet resultSet = statement.getResultSet();
					while (resultSet.next()) {
						return resultSet.getLong("count");
					}
				}
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		throw new IllegalStateException("Could not determine the count of source attachments in the database");
	}
	
	private long getSourceAttachmentCount() {
		try {
			if (connectionPostgres != null) {
				try (final Statement statement = connectionPostgres.createStatement()) {
					statement.execute("SELECT count(*) FROM source");
					final ResultSet resultSet = statement.getResultSet();
					if (resultSet.next()) {
						return resultSet.getLong(1);
					}
				}
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		throw new IllegalStateException("Could not determine the count of source attachments in the database");
	}

	@Test
	void testHotSpotsForReferences() throws IOException {
		final var TEST_MODULE_HOTSPOT_REFERENCES = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_REFERENCES.setName("TEST MODULE for Hotspot References");
		TEST_MODULE_HOTSPOT_REFERENCES.setPath("src/program/testmodule.sql");
		TEST_MODULE_HOTSPOT_REFERENCES.setProject(ONE);
		TEST_MODULE_HOTSPOT_REFERENCES.setTechnology(Technology.RESOURCE);
		TEST_MODULE_HOTSPOT_REFERENCES.setType(Type.PROGRAM);
		TEST_MODULE_HOTSPOT_REFERENCES.setStorage(Storage.FILE);
		TEST_MODULE_HOTSPOT_REFERENCES.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_REFERENCES.setOrigin(Origin.CUSTOM);
		TEST_MODULE_HOTSPOT_REFERENCES.setCreator(Creator.DISCOVERY);
		final ModulePojo createdModule = createModule(ONE, TEST_MODULE_HOTSPOT_REFERENCES);
		final Result<HotSpot[]> result = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.REFERENCES).setLimit(100)
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		ModuleServiceTest.testHotSpots(result.getValue().get(), createdModule);
	}

	@Test
	void testHotSpotsForCandidateRules() throws IOException {
		final Long businessRuleAnnotationCategoryId = getBusinessRuleAnnotationCateogryId();
		final var annotation = new AnnotationPojoPrototype();
		annotation.setName("BusinessRuleAnnotation");
		annotation.setCategoryId(businessRuleAnnotationCategoryId);
		annotation.setSourceAttachment("1234");
		annotation.setType(AnnotationType.RULE);
		annotation.setState(WorkingState.CANDIDATE);
		annotation.setModule(EntityId.of(2000L));
		annotation.setLocation(new ModuleLocation(1, 10));
		annotationServiceProvider.createAnnotation().setProjectId(ONE).setModuleId(EntityId.of(2000L)).setAnnotation(annotation).execute();
		
		final Result<ModulePojo> resultModule = moduleServiceProvider.findModuleById().setModuleId(EntityId.of(2000L)).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, resultModule.getStatusCode());
		assertTrue(resultModule.getValue().isPresent());
		final ModulePojo module = resultModule.getValue().get();
		final Result<HotSpot[]> result = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.CANDIDATE_RULE).setLimit(10)
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		ModuleServiceTest.testHotSpots(result.getValue().get(), module);
	}

	private Long getBusinessRuleAnnotationCateogryId() throws IOException {
		final Result<AnnotationCategory[]> annotationCategoryResult = MiningApiClient.annotationCategoryService(getConnectionInfo())
				.findAllAnnotationCategories().setProjectId(ONE).execute();
		
		final AnnotationCategory[] annotationCategories = annotationCategoryResult.getValue()
				.orElseThrow(() -> new IllegalStateException("Could not determine the Annotation categories for project " + ONE));
		Long businessRuleAnnotationCategoryId = null;
		for (final AnnotationCategory annotationCategory : annotationCategories) {
			if ("Business Rule".equals(annotationCategory.getName())) {
				businessRuleAnnotationCategoryId = annotationCategory.getId();
				break;
			}
		}
		if (businessRuleAnnotationCategoryId == null) {
			throw new IllegalStateException("Could not determine the ID of the Annotation category with the name 'Business Rule'.");
		}
		return businessRuleAnnotationCategoryId;
	}

	@Test
	void testHotSpotsForDataSets() throws IOException {
		final var TEST_MODULE_HOTSPOT_DATASET = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_DATASET.setName("TEST MODULE for Hotspot Datasets");
		TEST_MODULE_HOTSPOT_DATASET.setProject(ONE);
		TEST_MODULE_HOTSPOT_DATASET.setPath("src/testmodule.sql");
		TEST_MODULE_HOTSPOT_DATASET.setTechnology(Technology.RESOURCE);
		TEST_MODULE_HOTSPOT_DATASET.setType(Type.FILE);
		TEST_MODULE_HOTSPOT_DATASET.setStorage(Storage.FILE);
		TEST_MODULE_HOTSPOT_DATASET.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_DATASET.setOrigin(Origin.CUSTOM);
		TEST_MODULE_HOTSPOT_DATASET.setCreator(Creator.DISCOVERY);
		final var createdModule = createModule(ONE, TEST_MODULE_HOTSPOT_DATASET);
		createEdge(RelationshipType.ACCESSES, createdModule.identity(), EntityId.of(2000L));
		final Result<HotSpot[]> result = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.DATA_SETS).setLimit(10)
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		ModuleServiceTest.testHotSpots(result.getValue().get(), createdModule);
	}

	@Test
	void testHotSpotsForDataSetsDoesNotContainTemporaryDatasetModules() throws IOException {
		final var TEST_MODULE_HOTSPOT_DATASET = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_DATASET.setName("TEST MODULE for Hotspot Datasets without Ampersand");
		TEST_MODULE_HOTSPOT_DATASET.setPath("src/listcat/programs/TESTMODULE1.cbl");
		TEST_MODULE_HOTSPOT_DATASET.setProject(ONE);
		TEST_MODULE_HOTSPOT_DATASET.setTechnology(Technology.RESOURCE);
		TEST_MODULE_HOTSPOT_DATASET.setType(Type.FILE);
		TEST_MODULE_HOTSPOT_DATASET.setStorage(Storage.FILE);
		TEST_MODULE_HOTSPOT_DATASET.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_DATASET.setOrigin(Origin.CUSTOM);
		final var createdModule = createModule(ONE, TEST_MODULE_HOTSPOT_DATASET);
		createEdge(RelationshipType.ACCESSES, createdModule.identity(), FIRST_CUSTOM_MODULE_ID);
		final var TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setName("&TEST MODULE for Hotspot Datasets with Single Ampersand");
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setPath("src/program/listcat.sql");
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setProject(ONE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setTechnology(Technology.RESOURCE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setType(Type.FILE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setStorage(Storage.FILE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND.setOrigin(Origin.CUSTOM);
		final var createdModuleWithSingleAmpersand = createModule(ONE, TEST_MODULE_HOTSPOT_DATASET_WITH_SINGLE_AMPERSAND);
		createEdge(RelationshipType.ACCESSES, createdModuleWithSingleAmpersand.identity(), FIRST_CUSTOM_MODULE_ID);
		final ModulePojoPrototype TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setName("&&TEST MODULE for Hotspot Datasets with Double Ampersand");
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setPath("src/program/testmodule.sql");
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setProject(ONE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setTechnology(Technology.RESOURCE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setType(Type.FILE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setStorage(Storage.FILE);
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND.setOrigin(Origin.CUSTOM);
		final var createdModuleWithDoubleAmpersand = createModule(ONE, TEST_MODULE_HOTSPOT_DATASET_WITH_DOUBLE_AMPERSAND);
		createEdge(RelationshipType.ACCESSES, createdModuleWithDoubleAmpersand.identity(), CUSTOM_MODULE_WITH_DD_ID);
		final Result<HotSpot[]> result = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.DATA_SETS).setLimit(10).execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final HotSpot[] hotSpots = result.getValue().get();
		assertEquals(2, hotSpots.length);
		final List<String> expectedNames = Arrays.asList("&TEST MODULE for Hotspot Datasets with Single Ampersand", 
					  									 "TEST MODULE for Hotspot Datasets without Ampersand");
		final List<String> actualHotSpotModuleNames = Arrays.stream(hotSpots)
				.map(HotSpot::getModule)
				.map(ModulePojo::getName)
				.collect(Collectors.toList());
		
		/* The ordering is not relevant, therefore we just check that all expected names are present */
		for (final Iterator<String> iterator = actualHotSpotModuleNames.iterator(); iterator.hasNext();) {
			final String actualHotSpotModuleName = iterator.next();
			if (expectedNames.contains(actualHotSpotModuleName)) {
				iterator.remove();
			}
		}
		/* Make sure that the list is empty and we have handled all names */
		assertTrue(actualHotSpotModuleNames.isEmpty());
	}

	@Test
	void testHotSpotsForCalls() throws IOException {
		final Result<ModulePojo[]> moduleResult = moduleServiceProvider.findModuleByName().setProjectId(ONE).setName("IO001A").execute();
		assertEquals(HttpStatus.SC_OK, moduleResult.getStatusCode());
		assertTrue(moduleResult.getValue().isPresent());
		final var module = moduleResult.getValue().get()[0];
		
		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(ONE)
				.setModuleId(module.identity())
				.execute();
		assertEquals(HttpStatus.SC_ACCEPTED, storeAstResult.getStatusCode());
		final Result<HotSpot[]> hotspotResult = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.CALLS).setLimit(10)
				.execute();
		assertEquals(HttpStatus.SC_OK, hotspotResult.getStatusCode());
		assertTrue(hotspotResult.getValue().isPresent());
		testHotSpots(hotspotResult.getValue().get(), module);
	}
	
	@Test
	void testHotSpotsForDatabaseTables() throws IOException {
		final var TEST_MODULE_HOTSPOT_DATABASE_TABLES = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setName("TEST MODULE for Hotspot Database tables");
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setPath("src/program/testmodule.sql");
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setProject(ONE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setTechnology(Technology.SQL);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setType(Type.TABLE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setStorage(Storage.DATABASE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setOrigin(Origin.CUSTOM);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setCreator(Creator.DISCOVERY);
		final var createdModule = createModule(ONE, TEST_MODULE_HOTSPOT_DATABASE_TABLES);
		createEdge(RelationshipType.ACCESSES, createdModule.identity(), EntityId.of(2000L));
		final Result<HotSpot[]> result = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.DATABASE_TABLES).setLimit(10)
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		ModuleServiceTest.testHotSpots(result.getValue().get(), createdModule);
	}
	
	@Test
	void testHotSpotsForDatabaseTablesShouldNotConsiderTechnologyResource() throws IOException {
		final var TEST_MODULE_HOTSPOT_DATABASE_TABLES = new ModulePojoPrototype();
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setName("TEST MODULE for Hotspot Database tables");
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setPath("src/testmodule.sql");
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setProject(ONE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setTechnology(Technology.RESOURCE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setType(Type.TABLE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setStorage(Storage.DATABASE);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setIdentification(Identification.IDENTIFIED);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setOrigin(Origin.CUSTOM);
		TEST_MODULE_HOTSPOT_DATABASE_TABLES.setCreator(Creator.DISCOVERY);
		final ModulePojo createdModule = createModule(ONE, TEST_MODULE_HOTSPOT_DATABASE_TABLES);
		createEdge(RelationshipType.ACCESSES, createdModule.identity(), EntityId.of(2000L));
		final Result<HotSpot[]> result = moduleServiceProvider.getHotSpots().setProjectId(ONE).setHotSpotType(FilterType.DATABASE_TABLES).setLimit(10)
				.execute();
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<HotSpot> hotspotList = Stream.of(result.getValue().get())
				.filter(hotSpot -> hotSpot.getModule().getId().equals(createdModule.getId()))
				.collect(Collectors.toList());
		assertEquals("HotSpots for database tables should not consider technology RESOURCE", 0, hotspotList.size());
	}
	
	@Test
	void testDeleteModuleCleanup() throws IOException {
		final Result<AnnotationPojo[]> annotationsResult = moduleServiceProvider.findAnnotationsByModule()
				.setProjectId(ONE)
				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
				.execute();
		assertEquals(HttpStatus.SC_OK, annotationsResult.getStatusCode());
		final int numberOfAnnotations = annotationsResult.getValue().get().length;
		assertNotEquals(0, numberOfAnnotations);
		final Result<AnnotationPojo[]> allAnnotationsResult = annotationServiceProvider.findAllAnnotations().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, allAnnotationsResult.getStatusCode());
		final int numberOfAllAnnotations = allAnnotationsResult.getValue().get().length;
		assertNotEquals(0, numberOfAllAnnotations);
		
		final AtomicLong numberOfDds = new AtomicLong(Objects.requireNonNull(dataDictionaryDao).count(q -> {}));
		assertNotEquals(0, numberOfDds.get());
		
		final Result<DataDictionaryPojo[]> ddResult = dataDictionaryServiceProvider.findAllDataDictionaryEntries()
				.setProjectId(ONE)
				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
				.execute();
		assertEquals(HttpStatus.SC_OK, ddResult.getStatusCode());
		final int numberOfDd = ddResult.getValue().get().length;
		assertNotEquals(0, numberOfDd);
		
		final Result<Void> deleteResult = moduleServiceProvider.deleteModule().setProjectId(ONE).setModuleId(CUSTOM_MODULE_WITH_DD_ID).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, deleteResult.getStatusCode());
		
		final Result<AnnotationPojo[]> annotationsAfterDeleteResult = annotationServiceProvider.findAllAnnotations().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, annotationsAfterDeleteResult.getStatusCode());
		assertEquals(numberOfAllAnnotations - numberOfAnnotations, annotationsAfterDeleteResult.getValue().get().length);
		
		assertEquals(numberOfDds.get() - numberOfDd, Objects.requireNonNull(dataDictionaryDao).count(q -> {}).longValue());
	}
	
	@Test
	void testDeleteAllModulesCleanup() throws IOException {
		final Result<AnnotationPojo[]> annotationsResult = annotationServiceProvider.findAllAnnotations().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, annotationsResult.getStatusCode());
		final int numberOfAnnotations = annotationsResult.getValue().get().length;
		assertNotEquals(0, numberOfAnnotations);
		
		final AtomicLong numberOfDds = new AtomicLong(Objects.requireNonNull(dataDictionaryDao).count(q -> {}));
		assertNotEquals(0, numberOfDds.get());
		
		final AtomicLong numberOfProjectDds = new AtomicLong(Objects.requireNonNull(dataDictionaryDao).count(q -> q.ofModuleProject(ONE)));
		assertNotEquals(0, numberOfProjectDds.get());
		
		final Result<Void> deleteAllResult = moduleServiceProvider.deleteAllModules().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_NO_CONTENT, deleteAllResult.getStatusCode());
		
		final Result<AnnotationPojo[]> annotationsAfterDeleteResult = annotationServiceProvider.findAllAnnotations().setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_OK, annotationsAfterDeleteResult.getStatusCode());
		assertEquals(0, annotationsAfterDeleteResult.getValue().get().length);
		
		assertEquals(Long.valueOf(numberOfDds.get() - numberOfProjectDds.get()), Objects.requireNonNull(dataDictionaryDao).count(q -> {}));
	}


	@Test
	void testTraverseDependenciesWithExplorable() throws IOException {
		try {
			final Result<Void> result2 = featureService.toggleFeature()
					.setFeatureId(DEPENDENCY_GRAPH_EXPLORE)
					.setState(Boolean.TRUE)
					.execute();
			assertEquals(HttpStatus.SC_NO_CONTENT, result2.getStatusCode());

			//test
			final TraverseDependencies traverseDependencies = moduleServiceProvider.traverseDependencies()
					.setMaxDepth(2L)
					.setModuleId(FIRST_CUSTOM_MODULE_ID)
					.setProjectId(ONE);

			final Result<DependencyGraph> depsGraphResult = traverseDependencies.execute();
			assertEquals(HttpStatus.SC_OK, depsGraphResult.getStatusCode());
			final Map<UUID, Long> moduleUidToNid = Assert.assertNotNull(moduleDao, "ModuleDao is required for this test.")
					.findModules(q -> {}).stream()
					.collect(Collectors.toMap(ModulePojo::getUid, m -> m.identity().getNid()));

			final DependencyGraph graph = depsGraphResult.getValue().get();
			final var moduleTypes = new ArrayList<>(graph.getModuleTypes());
			final var modules = graph.getModules().stream()
					.map(ModulePojo::getName)
					.collect(Collectors.toList());
			final var references = graph.getReferences().stream()
					.map(r -> String.format("%s %s %s", moduleUidToNid.get(r.getSrcModule()),
							r.getRelationship(),
							moduleUidToNid.get(r.getDstModule())))
					.collect(Collectors.toList());
			final var relationshipTypes = new ArrayList<>(graph.getRelationshipTypes());
			final var rootModuleIds = new ArrayList<>(graph.getRootModuleIds());

			Collections.sort(moduleTypes);
			Collections.sort(modules);
			Collections.sort(references);
			Collections.sort(relationshipTypes);

			assertThat(moduleTypes, Matchers.equalTo(List.of(
					"COBOL COPYBOOK",
					"COBOL PROGRAM",
					"JCL EXEC PGM",
					"NATURAL PROGRAM",
					"UNKNOWN UTILITY"
			)));
			assertThat(modules, Matchers.equalTo(List.of("DPGM1", "DPGM2", "DPGM3", "IDCAMS", "MMRS7101", "PRG1", "QBGPSLP1MMRS710A.STEP01.MMRS7102")));
			assertThat(references, Matchers.equalTo(List.of(
					"2000 CALLS 2001",
					"2000 CALLS 2002",
					"2000 CALLS 2017",
					"2002 CALLS 2047",
					"2017 CALLS 2018",
					"2017 CALLS 2019"
			)));
			assertThat(relationshipTypes, Matchers.equalTo(List.of("CALLS")));
			assertThat(rootModuleIds, Matchers.equalTo(List.of(2000L)));

		} finally {
			//disable feature again
			final Result<Void> result = featureService.toggleFeature()
					.setFeatureId(DEPENDENCY_GRAPH_EXPLORE)
					.setState(Boolean.FALSE)
					.execute();
			assertEquals(HttpStatus.SC_NO_CONTENT, result.getStatusCode());
		}
	}

	@Test
	void testTraverseDependenciesWithoutExplorable() throws IOException {
		final TraverseDependencies traverseDependencies = moduleServiceProvider.traverseDependencies()
				.setMaxDepth(2L)
				.setModuleId(FIRST_CUSTOM_MODULE_ID)
				.setProjectId(ONE);

		final Result<DependencyGraph> depsGraphResult = traverseDependencies.execute();
		assertEquals(HttpStatus.SC_OK, depsGraphResult.getStatusCode());
		final DependencyGraph graph = depsGraphResult.getValue().get();
		final Map<UUID, Long> moduleUidToNid = Assert.assertNotNull(moduleDao, "ModuleDao is required for this test.")
				.findModules(q -> {}).stream()
				.collect(Collectors.toMap(ModulePojo::getUid, m -> m.identity().getNid()));

		final var moduleTypes = new ArrayList<>(graph.getModuleTypes());
		final var modules = graph.getModules().stream()
				.map(ModulePojo::getName)
				.collect(Collectors.toList());
		final var references = graph.getReferences().stream()
				.map(r -> String.format("%s %s %s", moduleUidToNid.get(r.getSrcModule()),
						r.getRelationship(),
						moduleUidToNid.get(r.getDstModule())))
				.collect(Collectors.toList());
		final var relationshipTypes = new ArrayList<>(graph.getRelationshipTypes());
		final var rootModuleIds = new ArrayList<>(graph.getRootModuleIds());

		Collections.sort(moduleTypes);
		Collections.sort(modules);
		Collections.sort(references);
		Collections.sort(relationshipTypes);

		assertThat(moduleTypes, Matchers.equalTo(List.of(
				"COBOL COPYBOOK",
				"COBOL PROGRAM",
				"JCL EXEC PGM",
				"NATURAL PROGRAM",
				"UNKNOWN UTILITY"
		)));
		assertThat(modules, Matchers.equalTo(List.of("DPGM1", "DPGM2", "DPGM3", "IDCAMS", "MMRS7101", "PRG1", "QBGPSLP1MMRS710A.STEP01.MMRS7102")));
		assertThat(references, Matchers.equalTo(List.of(
				"2000 CALLS 2001",
				"2000 CALLS 2002",
				"2000 CALLS 2017",
				"2002 CALLS 2047",
				"2017 CALLS 2018",
				"2017 CALLS 2019"
				//these two references are added through the explorable flag
//				"2018 CALLS 2020",
//				"2019 CALLS 2021"
		)));
		assertThat(relationshipTypes, Matchers.equalTo(List.of("CALLS")));
		assertThat(rootModuleIds, Matchers.equalTo(List.of(2000L)));
	}
	
	@Test
	void testTraverseDependenciesWithBuiltIn() throws IOException {
		final Result<DependencyGraph> dependencyResult = moduleServiceProvider.traverseDependencies()
				.setMaxDepth(2L)
				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
				.setProjectId(ONE)
				.execute();
		assertEquals(HttpStatus.SC_OK, dependencyResult.getStatusCode());
		boolean containsIdcams = false;
		boolean containsModule2003 = false;
		String actualModuleDescription = "";
		for (final var module : dependencyResult.getValue().get().getModules()) {
			if ("IDCAMS".equals(module.getName())) {
				/* IDCAMS is a system built in and there is a calls reference from the module to IDCAMS */
				containsIdcams = true;
			}
			if ("MMRS7101".equals(module.getName())) {
				actualModuleDescription = module.getDescription().get();
			}
			if (2003 == module.getId().longValue()) {
				/* module 2003 is part of project 2 and should not be found */
				containsModule2003 = true;
			}
		}
		assertTrue(containsIdcams);
		assertFalse(containsModule2003);
		assertEquals(TEST_MODULE_DESCRIPTION, actualModuleDescription);
		
		final Result<DependencyGraph> graphLinksDepthFour = moduleServiceProvider.traverseDependencies()
				.setMaxDepth(4L)
				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
				.setProjectId(ONE)
				.execute();
		boolean containsAbend = false;
		for (final var module : graphLinksDepthFour.getValue().get().getModules()) {
			if ("ABEND".equals(module.getName())) {
				/* ABEND is a system built in and is accessed by module of different project that is also connected to IDCAMS */
				containsAbend = true;
			}
		}
		assertFalse(containsAbend);
	}
	
	@Test
	void testTraverseDependenciesWithCrossReferencedModules() throws IOException {
		final Result<DependencyGraph> graphLinksDepthFour = moduleServiceProvider.traverseDependencies()
				.setMaxDepth(4L)
				.setModuleId(CUSTOM_MODULE_WITH_DD_ID)
				.setProjectId(ONE)
				.execute();
		boolean containsAbend = false;
		for (final var module : graphLinksDepthFour.getValue().get().getModules()) {
			if ("ABEND".equals(module.getName())) {
				/* ABEND is a system built in and is accessed by module of different project that is also connected to IDCAMS */
				containsAbend = true;
			}
		}
		assertFalse(containsAbend);
	}

	@Test
	void testTraverseDependenciesWithoutDistinct() throws IOException {
		assertTraverseWithDuplicates(null);
	}

	@Test
	void testTraverseDependenciesDistinctEnabled() throws IOException {
		assertTraverseWithDuplicates(Boolean.TRUE);
	}

	@Test
	void testTraverseDependenciesDistinctDisabled() throws IOException {
		assertTraverseWithDuplicates(Boolean.FALSE);
	}

	private void assertTraverseWithDuplicates(@Nullable final Boolean distinct) throws IOException {
		final Result<ProjectPojo> resultProject = projectServiceProvider.createProject().setProject(new ProjectPojoPrototype()
				.setName("WDIS-543-Test")
				.setClient(ONE)
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.MINING)))
			).execute();
		assertEquals(HttpStatus.SC_CREATED, resultProject.getStatusCode());
		final EntityId projectId = resultProject.getValue().get().identity();

		final var javaModule = new ModulePojoPrototype()
				.setName("Fields")
				.setPath("Fields.java")
				.setProject(projectId)
				.setTechnology(Technology.JAVA)
				.setType(Type.TYPE)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent("Test Data");
		final Result<ModulePojo> resultFields = moduleServiceProvider.createModule().setModule(javaModule).setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_CREATED, resultFields.getStatusCode());
		final var fields = resultFields.getValue().get();

		final var dSN8ED2Module = new ModulePojoPrototype()
				.setName("DSN8ED2")
				.setPath("DSN8ED2.sql")
				.setProject(projectId)
				.setTechnology(Technology.SQL)
				.setType(Type.STORED_PROCEDURE)
				.setStorage(Storage.DATABASE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent("SP");
		final Result<ModulePojo> resultStoredProcedure = moduleServiceProvider.createModule().setModule(dSN8ED2Module).setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_CREATED, resultStoredProcedure.getStatusCode());
		final var storedProcedure = resultStoredProcedure.getValue().get();

		for (int i = 0; i < 4; i++) {
			final Result<ModuleRelationshipPojo> callRef = createEdge(RelationshipType.CALLS, projectId, storedProcedure.identity(), fields.identity());
			assertEquals(HttpStatus.SC_CREATED, callRef.getStatusCode());

			final Result<ModuleRelationshipPojo> referenceRef = createEdge(RelationshipType.REFERENCES, projectId, storedProcedure.identity(), fields.identity());
			assertEquals(HttpStatus.SC_CREATED, referenceRef.getStatusCode());
		}

		final TraverseDependencies traverseDependencies = moduleServiceProvider.traverseDependencies()
																			 .setMaxDepth(1L)
																			 .setModuleId(fields.identity())
																			 .setProjectId(projectId);
		if (distinct != null) {
			traverseDependencies.setDistinct(distinct);
		}

		final Result<DependencyGraph> depsGraphResult = traverseDependencies.execute();
		assertEquals(HttpStatus.SC_OK, depsGraphResult.getStatusCode());
		final DependencyGraph depsGraph = depsGraphResult.getValue().get();
		assertEquals(2, depsGraph.getModules().size());
		assertEquals(2, depsGraph.getModuleTypes().size());

		if (distinct == Boolean.TRUE) {
			assertEquals(2, depsGraph.getReferences().size());
		}

		int callCounter = 0;
		int refCounter = 0;
		for (var reference : depsGraph.getReferences()) {
			assertEquals(reference.getSrcModule(), fields.getUid());

			switch(reference.getRelationship()) {
				case CALLS:
					callCounter++;
					break;
				case REFERENCES:
					refCounter++;
					break;
				default:
					fail("Unhandled relationship: " + reference.getRelationship());
			}
		}

		if (distinct == Boolean.TRUE) {
			assertEquals(1, callCounter);
			assertEquals(1, refCounter);
		} else {
			assertEquals(4, callCounter);
			assertEquals(4, refCounter);
		}
	}

	@Test
	void testDuplicateMapName() throws IOException {
		
		/* Create 2 mapsets and 2 maps where the both maps have the same names */
		final var mapset1 = new ModulePojoPrototype()
				.setName("mapset1")
				.setPath("src/cobol/maps/mapset1.map")
				.setProject(ONE)
				.setTechnology(Technology.COBOL)
				.setType(Type.BMS_MAPSET)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent("test content for module1");

		final Result<ModulePojo> resultMapset1 = moduleServiceProvider.createModule().setModule(mapset1).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_CREATED, resultMapset1.getStatusCode());
		final EntityId mapset1Id = resultMapset1.getValue().get().identity();
		final var mapset2 = new ModulePojoPrototype()
				.setName("mapset2")
				.setPath("src/cobol/maps/mapset2.map")
				.setProject(ONE)
				.setTechnology(Technology.COBOL)
				.setType(Type.BMS_MAPSET)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent("Test Data");
		final Result<ModulePojo> resultMapset2 = moduleServiceProvider.createModule().setModule(mapset2).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_CREATED, resultMapset2.getStatusCode());
		final EntityId mapset2Id = resultMapset2.getValue().get().identity();
		final var map1 = new ModulePojoPrototype()
				.setName("map")
				.setPath("src/cobol/maps/test.map")
				.setProject(ONE)
				.setTechnology(Technology.COBOL)
				.setType(Type.BMS_MAPSET)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent("Test Data");
		final Result<ModulePojo> resultMap1 = moduleServiceProvider.createModule().setModule(map1).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_CREATED, resultMap1.getStatusCode());
		final EntityId map1Id = resultMap1.getValue().get().identity();
		final var map2 = new ModulePojoPrototype()
				.setName("map")
				.setPath("src/cobol/maps/test1.map")
				.setProject(ONE)
				.setTechnology(Technology.COBOL)
				.setType(Type.BMS_MAP)
				.setStorage(Storage.FILE_SECTION)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setContent("Test Data");
		final Result<ModulePojo> resultMap2 = moduleServiceProvider.createModule().setModule(map2).setProjectId(ONE).execute();
		assertEquals(HttpStatus.SC_CREATED, resultMap2.getStatusCode());
		final EntityId map2Id = resultMap2.getValue().get().identity();
		
		/* Create a reference from each map to a different mapset */
		final var ref1 = new ModuleRelationshipPojoPrototype();
		ref1.setSrcModule(map1Id);
		ref1.setDstModule(mapset1Id);
		ref1.setRelationship(RelationshipType.INCLUDES);
		referenceServiceProvider.createReference().setProjectId(ONE).setModuleId(map1Id).setReference(ref1).execute();
		final ModuleRelationshipPojoPrototype ref2 = new ModuleRelationshipPojoPrototype();
		ref2.setSrcModule(map2Id);
		ref2.setDstModule(mapset2Id);
		ref2.setRelationship(RelationshipType.INCLUDES);
		referenceServiceProvider.createReference().setProjectId(ONE).setModuleId(map2Id).setReference(ref2).execute();
		
		/* Verify that the modules are correctly linked by a reference */
		final Result<ModuleRelationshipPojo[]> resultReference1 = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(ONE)
				.setModuleId(map1Id)
				.setDirection(RelationshipDirection.BOTH)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultReference1.getStatusCode());
		final var realReference1 = resultReference1.getValue().get();
		assertEquals(1, realReference1.length);
		assertEquals(map1Id.getUid(), realReference1[0].getSrcModule());
		assertEquals(mapset1Id.getUid(), realReference1[0].getDstModule());

		final Result<ModuleRelationshipPojo[]> resultReference2 = referenceServiceProvider.findAllReferencesForModule()
				.setProjectId(ONE)
				.setModuleId(mapset2Id)
				.setDirection(RelationshipDirection.BOTH)
				.execute();
		assertEquals(HttpStatus.SC_OK, resultReference2.getStatusCode());
		final ModuleRelationshipPojo[] realReference2 = resultReference2.getValue().get();
		assertEquals(1, realReference2.length);
		assertEquals(map2Id.getUid(), realReference2[0].getSrcModule());
		assertEquals(mapset2Id.getUid(), realReference2[0].getDstModule());
	}	
	
	/**
	 * Tests that the REST service invocation for the Module description identification properly triggers a job that successfully executes.
	 * 
	 * @throws IOException if the REST calls cannot be executed
	 */
	@Test
	void moduleDescriptionIdentification() throws IOException {
		/* submit job */
		final List<String> modulePaths = new ArrayList<>();
		modulePaths.add("src/cobol/programs/EXECSQL.cbl");
		modulePaths.add("src-natural/LibA/PRG1.nsp");
		modulePaths.add("src/cobol/programs/CC1.cpy");
		modulePaths.add("src/cobol/maps/UISCOPE.map");
		
		final Result<String> result = moduleServiceProvider.identifyModuleDescriptions().setProjectId(ONE).setModulePaths(modulePaths).execute();
	
		assertNotNull(result);
		assertEquals(HttpStatus.SC_ACCEPTED, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		
		/* wait for the job to finish */
		final Instant start = Instant.now();
		JobInformation jobInfo = null;
		while (Duration.between(start, Instant.now()).toMinutes() < JOB_TIMEOUT) {
			final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
			assertNotNull(jobInfoResult);
			assertTrue(jobInfoResult.getValue().isPresent());
			jobInfo = jobInfoResult.getValue().get();
			
			final JobStatus status = jobInfo.getStatus();
			if (status != JobStatus.RUNNING && status != JobStatus.SCHEDULED) {
				break;
			}
		}
		
		assertNotNull(jobInfo);
		assertEquals(jobId, jobInfo.getJobId());
		assertEquals(JobStatus.SUCCESS, jobInfo.getStatus());
		/* There are 2 messages containing the number of supported Modules in the path selection and 1 error for missing sources */
		final List<Message> messages = jobInfo.getMessages();
		assertEquals(3, messages.size());
		assertTrue(messages.get(1).getText().contains("3 Module"));
		assertThat(messages.get(2).getText(), containsString("3 module(s) were successful."));
	}
	
	/**
	 * checks the log output {@link JobInformation} of the Module Description identification for a module without sourceCode.
	 * 
	 * @throws IOException if the REST call was not successful
	 */
	@Test
	void testModuleDescriptionIdentificationWithoutSourceCode() throws IOException {
		final String modulePath = "src/cobol/programs/PRGTEST.cbl";
		/* Submit job */
		final Result<String> result = moduleServiceProvider.identifyModuleDescriptions().setProjectId(ONE)
				.setModulePaths(Arrays.asList(modulePath)).execute();
		assertNotNull(result);
		assertEquals(HttpStatus.SC_ACCEPTED, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final String jobId = result.getValue().get();
		final JobInformation jobInformation = JobStatusUtil.checkJobStatus(jobId, jobServiceProvider);
		final List<Message> messages = jobInformation.getMessages();
		assertEquals(3, messages.size());
		final var module = new ModulePgDao(getDataSource()).findAnyModuleId(q -> q.withPath(modulePath)).orElseThrow();
		final var textWithoutUid = messages.get(0).getText().replaceAll("uid=[0-9a-z-]+", "uid=someUid");
		assertEquals(String.format(SOURCE_CODE_NOT_FOUND, module.getNid(), "PRGTEST"), textWithoutUid);
		assertEquals(IDENTIFIED_MODULE_MSG, messages.get(1).getText());
		assertThat(messages.get(2).getText(), containsString("1 module(s) were successful."));
	}

	@Test
	void testHasAstNodes() throws IOException {
		final Result<ModulePojo[]> resultPRGC = moduleServiceProvider
				.findModuleByName()
				.setProjectId(ONE)
				.setName("PRGC")
				.execute();
		assertEquals(HttpStatus.SC_OK, resultPRGC.getStatusCode());
		assertTrue(resultPRGC.getValue().isPresent());
		final ModulePojo modulePRGC = resultPRGC.getValue().get()[0];

		/* Should not have AST Nodes */
		Result<Boolean> hasAstNodesResult = moduleServiceProvider
				.hasAstNodes()
				.setProjectId(ONE.getNid())
				.setModuleId(modulePRGC.identity())
				.execute();
		
		assertFalse(hasAstNodesResult.getValue().get().booleanValue());
		
		final Result<Boolean> storeAstResult = moduleServiceProvider
				.storeAstNodes()
				.setProjectId(ONE)
				.setModuleId(modulePRGC.identity())
				.execute();

		assertEquals(HttpStatus.SC_ACCEPTED, storeAstResult.getStatusCode());
		
		/* Should have AST Nodes */
		hasAstNodesResult = moduleServiceProvider
				.hasAstNodes()
				.setProjectId(ONE)
				.setModuleId(modulePRGC.identity())
				.execute();
		
		assertTrue(hasAstNodesResult.getValue().get().booleanValue());
	}

	private static void testHotSpots(final HotSpot[] hotSpots, final ModulePojo createdModule) {
		final List<HotSpot> hotspotList = Stream.of(hotSpots)
				.filter(hotSpot -> hotSpot.getModule().getId().equals(createdModule.getId()))
				.collect(Collectors.toList());

		assertEquals(1, hotspotList.size());
		final HotSpot onlyHotSpot = hotspotList.get(0);
		assertEquals(createdModule.getName(), onlyHotSpot.getModule().getName());
		assertEquals(createdModule.getTechnology(), onlyHotSpot.getModule().getTechnology());
		assertEquals(createdModule.getType(), onlyHotSpot.getModule().getType());
	}

	private Result<ModuleRelationshipPojo> createEdge(final RelationshipType relationship, final EntityId moduleIdIn, final EntityId moduleIdOut) throws IOException {
		return createEdge(relationship, ONE, moduleIdIn, moduleIdOut);
	}

	private Result<ModuleRelationshipPojo> createEdge(final RelationshipType relationship, final EntityId projectId, final EntityId moduleIdIn,
			final EntityId moduleIdOut) throws IOException {
		final ReferenceServiceProvider referenceServiceProvider = MiningApiClient.referenceService(getConnectionInfo());
		final var reference = new ModuleRelationshipPojoPrototype()	.setRelationship(relationship).setSrcModule(moduleIdOut).setDstModule(moduleIdIn);
		return referenceServiceProvider.createReference().setProjectId(projectId).setModuleId(moduleIdIn).setReference(reference).execute();
	}

	/**
	 * Tests if aggregated utilities are returned for AggregationOperator COUNT.
	 *
	 * @throws IOException While invoking api calls.
	 */
	@Test
	void testAggregatedUtilityValuesForCount() throws IOException {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setGroupBy(Collections.singleton(ModuleFieldName.CATEGORIES));
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT));
		
		final Result<List<AggregationResult<ModuleFieldName>>> aggregatedUtilityValuesResult = moduleServiceProvider
				.getAggregatedUtilityValues()
				.setProjectId(ONE)
				.setAggregationRequest(aggregationRequest)
				.execute();
		assertEquals(HttpStatus.SC_OK, aggregatedUtilityValuesResult.getStatusCode());
		assertTrue("Optional List of aggregation results is not present", aggregatedUtilityValuesResult.getValue().isPresent());
		
		final List<AggregationResult<ModuleFieldName>> values = aggregatedUtilityValuesResult.getValue().get();
		assertTrue("List of aggregation results must not be empty", values.size() >= 1);
		assertNotNull(values.get(0));
		assertEquals(Integer.valueOf(1), values.get(0).getFields().get(ModuleFieldName.ID));
		assertEquals("[File Manipulation]", values.get(0).getGroup().get(ModuleFieldName.CATEGORIES).toString());
	}
	
	/**
	 * Tests if aggregated utilities are returned for AggregationOperator COUNT_DISTINCT.
	 *
	 * @throws IOException While invoking api calls.
	 */
	@Test
	void testAggregatedUtilityValuesForCountDistinct() throws IOException {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setGroupBy(Collections.singleton(ModuleFieldName.CATEGORIES));
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT_DISTINCT));
		
		final Result<List<AggregationResult<ModuleFieldName>>> aggregatedUtilityValuesResult = moduleServiceProvider
				.getAggregatedUtilityValues()
				.setProjectId(ONE)
				.setAggregationRequest(aggregationRequest)
				.execute();
		assertEquals(HttpStatus.SC_OK, aggregatedUtilityValuesResult.getStatusCode());
		assertTrue("Optional List of aggregation results is not present", aggregatedUtilityValuesResult.getValue().isPresent());
		
		final List<AggregationResult<ModuleFieldName>> values = aggregatedUtilityValuesResult.getValue().get();
		assertTrue("List of aggregation results must not be empty", values.size() >= 1);
		assertNotNull(values.get(0));
		assertEquals(Integer.valueOf(1), values.get(0).getFields().get(ModuleFieldName.ID));
		assertEquals("[File Manipulation]", values.get(0).getGroup().get(ModuleFieldName.CATEGORIES).toString());
	}
	
	/**
	 * Tests if aggregated utilities are returned for AggregationOperator SUM.
	 *
	 * @throws IOException While invoking api calls.
	 */
	@Test
	void testAggregatedUtilityValuesForSum() throws IOException {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setGroupBy(Collections.singleton(ModuleFieldName.CATEGORIES));
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.SUM));
		
		final Result<List<AggregationResult<ModuleFieldName>>> aggregatedUtilityValuesResult = moduleServiceProvider
				.getAggregatedUtilityValues()
				.setProjectId(ONE)
				.setAggregationRequest(aggregationRequest)
				.execute();
		assertEquals(HttpStatus.SC_OK, aggregatedUtilityValuesResult.getStatusCode());
		assertTrue("Optional List of aggregation results is not present", aggregatedUtilityValuesResult.getValue().isPresent());
		
		final List<AggregationResult<ModuleFieldName>> values = aggregatedUtilityValuesResult.getValue().get();
		assertTrue("List of aggregation results must not be empty", values.size() >= 1);
		assertNotNull(values.get(0));
		assertEquals(Integer.valueOf(2047), values.get(0).getFields().get(ModuleFieldName.ID));
		assertEquals("[File Manipulation]", values.get(0).getGroup().get(ModuleFieldName.CATEGORIES).toString());
	}
	
	/**
	 * Test when no aggregated utilities are returned for a given project.
	 *
	 * @throws IOException While invoking api calls.
	 */
	@Test
	void testAggregatedUtilityValuesForNoUtilitiesFound() throws IOException {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setGroupBy(Collections.singleton(ModuleFieldName.CATEGORIES));
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.SUM));
		
		final Result<List<AggregationResult<ModuleFieldName>>> aggregatedUtilityValuesResult = moduleServiceProvider
				.getAggregatedUtilityValues()
				.setProjectId(FOUR)
				.setAggregationRequest(aggregationRequest)
				.execute();
		assertEquals(HttpStatus.SC_OK, aggregatedUtilityValuesResult.getStatusCode());
		assertTrue("Optional List of aggregation results is not present", aggregatedUtilityValuesResult.getValue().isPresent());
		
		final List<AggregationResult<ModuleFieldName>> values = aggregatedUtilityValuesResult.getValue().get();
		assertEquals(0, values.size());
	}
	
	/**
	 * Tests if Modules with empty path are created.
	 *
	 * @throws IOException While invoking api calls.
	 */
	@Test
	void testEmptyModulePathString() throws IOException {
		final String moduleName = "EmptyPathCheck";

		final int countBeforeModulesInsert = countModuleByName(moduleName, ONE);
		final var sourceMetrics1 = new SourceMetricsPojoPrototype()
				.setCodeLines(5).setCodeLines(3).setComplexityMcCabe(27).setDeadCodeLines(-1);
		final var firstModuleWithEmptyPath = new ModulePojoPrototype()
				.setName(moduleName)
				.setPath("src/cobol/program/emptyPathCheck.cbl")
				.setProject(ONE)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.UNDEFINED)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setDescription("test description for module")
				.setSourceMetrics(sourceMetrics1)
				.setContent("test content for module1");
		createModule(ONE, firstModuleWithEmptyPath);

		final var sourceMetrics2 = new SourceMetricsPojoPrototype()
				.setCodeLines(5).setCodeLines(3).setComplexityMcCabe(27).setDeadCodeLines(-1);
		final var secondModuleWithEmptyPath = new ModulePojoPrototype()
				.setName(moduleName)
				.setPath("src/cobol/program/PathCheck.cbl")
				.setProject(ONE)
				.setTechnology(Technology.BASIC)
				.setType(Type.PROGRAM)
				.setStorage(Storage.UNDEFINED)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API)
				.setDescription("test description for module")
				.setSourceMetrics(sourceMetrics2)
				.setContent("test content for module2");
		createModule(ONE, secondModuleWithEmptyPath);

		assertEquals(countBeforeModulesInsert + 2, countModuleByName(moduleName, ONE));
	}
	
	@Test
	void testDeleteModuleIncrementPrjSrcCodeRevDelSrcObjIsSet() throws IOException {
		final EntityId projectId = EntityId.of(1L);

		moduleServiceProvider
				.createModule()
				.setProjectId(projectId)
				.setModule(new ModulePojoPrototype()
						.setName("firstModule")
						.setPath("src/firstModule.cbl")
						.setProject(projectId)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API))
				.execute();
		
		final Result<ProjectPojo> projectResult = projectServiceProvider.findProjectById().setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_OK, projectResult.getStatusCode());
		final Long srcCodeRev = projectResult.getValue().get().getSourceCodeRevision();

		/* Trigger deletion of modules with deleteSourceObjects set to true */
		final Result<Void> deletionResult = moduleServiceProvider.deleteAllModules().setProjectId(projectId)
				.setDeleteSourceObjects(Boolean.TRUE)
				.execute();
		
		assertEquals(HttpStatus.SC_NO_CONTENT, deletionResult.getStatusCode());
		
		final Result<ProjectPojo> projectResultAfterModuleDelete = projectServiceProvider.findProjectById().setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_OK, projectResult.getStatusCode());
		
		assertNotNull(srcCodeRev);
		final Long srcCodeRevInc = Long.valueOf(srcCodeRev.longValue() + 1);
		/* Verifies Source code revision increment on module delete when deleteSourceObjects is set to true */
		assertEquals(srcCodeRevInc, projectResultAfterModuleDelete.getValue().get().getSourceCodeRevision());
	}
	
	@Test
	void testDeleteModuleIncrementPrjSrcCodeRevDelSrcObjIsNotSet() throws IOException {
		final EntityId projectId = EntityId.of(1L);

		moduleServiceProvider
				.createModule()
				.setProjectId(projectId)
				.setModule(new ModulePojoPrototype()
						.setName("firstModule")
						.setPath("src/firstModule.cbl")
						.setProject(projectId)
						.setTechnology(Technology.COBOL)
						.setType(Type.PROGRAM)
						.setStorage(Storage.FILE)
						.setIdentification(Identification.IDENTIFIED)
						.setOrigin(Origin.CUSTOM)
						.setCreator(Creator.API))
				.execute();
		
		final Result<ProjectPojo> projectResult = projectServiceProvider.findProjectById().setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_OK, projectResult.getStatusCode());
		final Long srcCodeRev = projectResult.getValue().get().getSourceCodeRevision();

		/* Trigger deletion of modules with deleteSourceObjects set to false */
		final Result<Void> deletionResult = moduleServiceProvider.deleteAllModules().setProjectId(projectId)
				.setDeleteSourceObjects(Boolean.FALSE)
				.execute();
		
		assertEquals(HttpStatus.SC_NO_CONTENT, deletionResult.getStatusCode());
		
		final Result<ProjectPojo> projectResultAfterModuleDelete = projectServiceProvider.findProjectById().setProjectId(projectId).execute();
		assertEquals(HttpStatus.SC_OK, projectResult.getStatusCode());
		
		/* Verifies Source code revision does not increment and remains same on module delete when deleteSourceObjects is set to false */
		assertEquals(srcCodeRev, projectResultAfterModuleDelete.getValue().get().getSourceCodeRevision());
	}
	
	@Test
	void testFindByModuleWithNonExistingId() throws IOException {
		final Result<ModulePojo> result = moduleServiceProvider.findModuleById().setModuleId(NON_EXISTING_ID).setProjectId(ONE)
				.execute();
		assertTrue("Should return error message as Module id not found", result.getExtendedStatusMessage()
				.contains(String.format("MiningEntityNotFoundException thrown from controller while trying to access /api/v1/projects/%s/modules/%s",
						ONE.getNid(), NON_EXISTING_ID.getNid())));
	}
	
	private int countModuleByName(final String moduleName, final EntityId projectId) throws IOException {
		final Result<ModulePojo[]> findCreatedModuleResult = moduleServiceProvider.findModuleByName().setProjectId(projectId).setName(moduleName).execute();

		assertEquals(200, findCreatedModuleResult.getStatusCode());

		final Optional<ModulePojo[]> modules = findCreatedModuleResult.getValue();
		if (modules.isPresent()) {
			return modules.get().length;
		} else {
			return 0;
		}
	}
}

