/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.metadata;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.extensions.metadata.model.MetaDataBackup;
import innowake.mining.extensions.metadata.model.ModuleBackup;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.http.HttpEntity;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Phaser;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link MetaDataImportExtension} to validate the import of {@linkplain MetaDataBackup metadata backup} to the {@linkplain ModuleBackup module
 * backup} {@linkplain AnnotationPojo annotations}, {@linkplain DataDictionaryPojo data dictionaries} and {@linkplain TaxonomyPojo taxonomies}.
 */
@WithMockUser
class MetaDataImportTest extends DatabaseResettingTest {
	
	private static final String SAMPLE_CONTENT_HASH = "123214124";
	private static final String BACKUP_FILE_PATH = "./test-resources/innowake/mining/extensions/import/metadata/";
	private static final File BACKUP_FILE = new File(BACKUP_FILE_PATH + "metadata.json");
	private static final EntityId PROJECT_ID = EntityId.of(1L);
	private static final File BACKUP_FILE_ALTERNATE = new File(BACKUP_FILE_PATH + "metadata_ddeAnno.json");
	private static final File BACKUP_FILE_ALTERNATE2 = new File(BACKUP_FILE_PATH + "metadata_ddeAnno2.json");
	private static final File BACKUP_FILE_ZIPPED = new File(BACKUP_FILE_PATH + "metadata.zip");
	private static final File BACKUP_FILE_ZIPPED_NO_CHANGES = new File(BACKUP_FILE_PATH + "backupWithNoChanges.zip");
	private static final String TAXONOMY_NAME = "Taxonomy 1";
	private static final String DATA_DICTIONARY_NAME = "Data Dictionary 1";
	private static final String ANNOTATION_CATEGORY_NAME = "Annotation Category A";

	private String metadataBackupJson = StringUtils.EMPTY;
	private String metadataBackupJsonAlternate = StringUtils.EMPTY;
	private String metadataBackupJsonAlternate2 = StringUtils.EMPTY;

	@Autowired
	private brave.Tracer tracer;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private MetaDataImportExtension extension;

	@Autowired
	private MetaDataExportExtension extensions;
	
	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private TaxonomyService taxonomyService;

	@Nullable
	private ModulePojo createdModule;

	@Nullable
	private ModulePojo restoredModule;

	@Nullable
	private MetaDataBackup metaDataBackup;
	
	@Autowired
	private BuildProperties buildProperties;
	
	@Autowired
	private FunctionalBlockService functionalBlockService;
	
	@Autowired
	private FunctionalBlockGenerationService functionalBlockGenerationService;
	
	@Autowired
	private JobConfigurationProperties jobConfig;

	private final List<String> jobIds = new ArrayList<>();

	@BeforeEach
	void createModule() throws IOException {
		metadataBackupJson = FileUtils.readFileToString(BACKUP_FILE, StandardCharsets.UTF_8);
		metaDataBackup = objectMapper.readValue(metadataBackupJson, MetaDataBackup.class);
		createdModule = moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID).withPath("/src/MOD 1"))
										.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: /src/MOD 1"));
		
		metadataBackupJsonAlternate = FileUtils.readFileToString(BACKUP_FILE_ALTERNATE, StandardCharsets.UTF_8);
		metadataBackupJsonAlternate2 = FileUtils.readFileToString(BACKUP_FILE_ALTERNATE2, StandardCharsets.UTF_8);
	}

	@SuppressWarnings("unchecked")
	@Test
	void testMetaDataImportJob() throws Throwable {
		final List<ModuleBackup> modules = Objects.requireNonNull(Objects.requireNonNull(metaDataBackup).getModules());
		assertEquals(1, modules.size());
		final Map<String, Object> properties = (Map<String, Object>) Objects.requireNonNull(modules.get(0).getCustomProperties()).get("ModuleCustomProperties");
		assertEquals(2, properties.size());
		assertTrue(properties.containsKey("customMetaInfo1"));
		assertTrue(properties.containsKey("customMetaInfo2"));

		try {
			properties.put("customMetaInfo1", "ABC");
			properties.put("customMetaInfo2", "123");

			final TimeZone cetTimeZone = TimeZone.getTimeZone("UTC");
			final GregorianCalendar date = new GregorianCalendar(cetTimeZone);
			date.set(2021, 9, 07, 17, 05, 34);


			/* keep null values in custom property maps */
			final String backupData = objectMapper.writeValueAsString(metaDataBackup);
			restoreMetaData(PROJECT_ID, backupData);
			/* Validate meta data backup */
			assertEquals(buildProperties.getVersion(), assertNotNull(metaDataBackup).getApiServerVersion());
			assertEquals(date.getTime().toString(), assertNotNull(metaDataBackup).getBackupDate().toString());
			final List<ModuleBackup> backUpModules = assertNotNull(assertNotNull(metaDataBackup).getModules(), "No modules found for back up");
			assertEquals(1, backUpModules.size());
			final ModuleBackup backedupModule = backUpModules.get(0);
			validateRestoredModuleMetaData(backedupModule, assertNotNull(restoredModule));

			/* Validating requiresReview is false during exact module match */
			assertFalse(assertNotNull(restoredModule).isRequiresReview());
		} finally {
			properties.put("customMetaInfo1", null);
			properties.put("customMetaInfo2", null);
		}
	}

	@Test
	void testMetaDataImportJobToCheckContentHashOfRestoredModule() throws Throwable {
		final var contentHash = SAMPLE_CONTENT_HASH.getBytes(StandardCharsets.UTF_8);
		assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).setContentHash(contentHash);


		/* keep null values in custom property maps */
		final String backupData = objectMapper.writeValueAsString(metaDataBackup);
		restoreMetaData(PROJECT_ID, backupData);
		final Optional<BinaryValue> restoredContentHash = assertNotNull(restoredModule).getContentHash();
		assertTrue(restoredContentHash.isPresent());
		assertNotEquals(contentHash, restoredContentHash.get().get(), "Backup module content hash should not be restored");
	}

	@Test
	void testMetaDataImportJobForNoMatch() throws Throwable {
		assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).setName("ModuleNameMismatch");


		/* keep null values in custom property maps */
		final String backupData = objectMapper.writeValueAsString(metaDataBackup);
		restoreMetaData(PROJECT_ID, backupData);
		final List<ModulePojo> modulesFromDB = moduleService.findModules(b -> b.ofProject(PROJECT_ID).withName("ModuleNameMismatch"));
		/* Validating there are no modules present with the backup module name in DB */
		assertEquals(0, modulesFromDB.size());
		/* No restore of metaData if the moduleName is not matched with any modules */
		assertTrue(assertNotNull(restoredModule).getDescription().isEmpty());
	}
	
	@Test
	void testMetaDataImportJobToCheckWrongLinkHash() throws Throwable {
		/* Add wrong linkHash to Json to return null module with linkHash*/
		final String backupData = objectMapper.writeValueAsString(metaDataBackup);
		restoreMetaData(PROJECT_ID, backupData);
		final String name = assertNotNull(restoredModule).getName();
		assertEquals("MOD 1", name, "It should find the module 'MOD 1' with wrong linkHash");

	}

	@Test
	void testMetaDataImportJobForRestoredDataDictionary() throws Throwable {
		final String dataElementNameFromBackup =
				assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getDataDictionaryEntries()).get(0).getName();
		assertEquals(DATA_DICTIONARY_NAME, dataElementNameFromBackup);
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(assertNotNull(createdModule).identity())).size());
		/* Run import repeatedly to ensure that no duplications occur */
		for (int pass = 1; pass <= 2; pass++) {
			try {
				restoreJob(metadataBackupJson, PROJECT_ID);
				/* Validate data dictionaries restored in the matched module */
				final List<DataDictionaryPojo> restoredDataDictionaryEntries =
						dataDictionaryService.find(q -> q.ofModule(assertNotNull(createdModule).identity()));
				assertEquals(1, restoredDataDictionaryEntries.size());
				final List<DataDictionaryPojo> backedupDataDictionaryEntries =
						assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getDataDictionaryEntries());
				assertEquals(backedupDataDictionaryEntries.size(), restoredDataDictionaryEntries.size());
				validateRestoredDataDictionary(backedupDataDictionaryEntries.get(0), restoredDataDictionaryEntries.get(0));
			} catch (final Throwable e) {
				throw new Exception("In pass " + pass, e);
			}
		}
	}

	@Test
	void testMetaDataImportJobForRestoredAnnotations() throws Throwable {
		final String annotationCategoryName = assertNotNull(assertNotNull(metaDataBackup).getAnnotationCategories()).get(0).getName();
		assertEquals(ANNOTATION_CATEGORY_NAME, annotationCategoryName);
		assertEquals(1, annotationService.find(q -> q.ofProject(PROJECT_ID)).size());
		/* Run import repeatedly to ensure that no duplications occur */
		for (int pass = 1; pass <= 2; pass++) {
			try {
				restoreJob(metadataBackupJson, PROJECT_ID);
				/* We need to do .withName("Annotation 1") here since it seems to be a race condition which annotation gets found first */
				final List<AnnotationPojo> restoredAnnotations = annotationService.find(q -> q.ofProject(PROJECT_ID)
						.ofModule(assertNotNull(createdModule).identity())
						.withName("Annotation 1"));
				final List<AnnotationPojo> backedupAnnotations =
						assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getAnnotations());
				assertEquals(backedupAnnotations.size(), restoredAnnotations.size());
				final AnnotationPojo restoredAnnotation = restoredAnnotations.get(0);
				final AnnotationPojo backedupAnnotation = backedupAnnotations.get(0);
				validateRestoredAnnotation(backedupAnnotation, restoredAnnotation);
				assertEquals(backedupAnnotation.getCustomProperties().getSub("AnnotationCustomProperties").size()
						, restoredAnnotation.getCustomProperties().getSub(customPropertiesService.getDefaultClassName(PROJECT_ID.getNid(),
								MiningEnitityNames.ANNOTATION)).size());
				assertEquals(backedupAnnotation.getProjectNid(), restoredAnnotation.getProjectNid());
			} catch (final Throwable e) {
				throw new Exception("In pass " + pass, e);
			}
		}
	}
	
	@Test
	void testMetaDataImportJobForRestoredAnnotationsWithoutAnnotationCategory() throws Throwable {
		metadataBackupJson = FileUtils.readFileToString(new File(BACKUP_FILE_PATH + "annotationWithoutCategory.json"), StandardCharsets.UTF_8);
		metaDataBackup = objectMapper.readValue(metadataBackupJson, MetaDataBackup.class);
		final String annotationCategoryName = assertNotNull(assertNotNull(metaDataBackup).getAnnotationCategories()).get(0).getName();
		assertEquals(ANNOTATION_CATEGORY_NAME, annotationCategoryName);
		assertEquals(1, annotationService.find(q -> q.ofProject(PROJECT_ID)).size());
		restoreJob(metadataBackupJson, PROJECT_ID);
		/* Validate annotations restored in the matched module */
		/* We need to do .withName("Annotation 1") here since it seems to be a race condition which annotation gets found first */
		final AnnotationPojo restoredAnnotation = assertNotNull(annotationService.find(q -> q.ofProject(PROJECT_ID).withName("Annotation 1"))).get(0);
		final AnnotationPojo backedupAnnotation = assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getAnnotations()).get(0);
		validateRestoredAnnotation(backedupAnnotation, restoredAnnotation);
	}

	@Test
	void testMetaDataImportJobForRestoredTaxonomy() throws Throwable {
		restoreAndValidateTaxonomy(PROJECT_ID, assertNotNull(createdModule).identity(), TAXONOMY_NAME);
	}

	@Test
	void testMetaDataImportJobWithExistingTaxonomyInDB() throws Throwable {
		final List<TaxonomyPojo> taxonomiesFromBackup = assertNotNull(metaDataBackup).getTaxonomies();
		final TaxonomyPojo firstTaxonomyPojo = assertNotNull(taxonomiesFromBackup).get(0);
		final String taxonomyName = firstTaxonomyPojo.getName();
		assertEquals(TAXONOMY_NAME, taxonomyName);
		final ModulePojo moduleInDb = assertNotNull(createdModule);
		final TaxonomyCategoryPojo taxonomyCategoryPojo = taxonomyService.findCategories(q -> q.ofProject(PROJECT_ID)).get(0);
		final TaxonomyTypePojoPrototype newType = new TaxonomyTypePojoPrototype();
		newType.setProject(PROJECT_ID);
		newType.setCategoryId(taxonomyCategoryPojo.getId());
		newType.setName(firstTaxonomyPojo.getType().getName());
		//create reference
		final UUID taxonomyId = taxonomyService.createType(newType);
		final TaxonomyPojoPrototype newTaxonomyPojo = new TaxonomyPojoPrototype()
				.setType(taxonomyId)
				.setProject(PROJECT_ID)
				.setName(firstTaxonomyPojo.getName());
		final EntityId taxonomyFromDbId = taxonomyService.create(newTaxonomyPojo);
		taxonomyService.createModuleLink(moduleInDb.getUid(), taxonomyFromDbId);

		final List<TaxonomyPojo> taxonomiesInDb = taxonomyService.find(q -> q.ofProject(PROJECT_ID).ofModule(moduleInDb.identity()));
		assertEquals(1, taxonomiesInDb.size());
		restoreJob(metadataBackupJson, PROJECT_ID);
		/* Validate Taxonomies restored in the matched module */
		final List<TaxonomyPojo> restoredTaxonomies = taxonomyService.find(q -> q.ofProject(PROJECT_ID).ofModule(moduleInDb.identity()));
		assertEquals(1, restoredTaxonomies.size());
		assertEquals(taxonomiesInDb.get(0).getId(), restoredTaxonomies.get(0).getId());
		
		final var taxonomyEdges = taxonomyService.findTaxonomiesPerModule(q -> q.ofProject(PROJECT_ID).ofModule(moduleInDb.identity()));
		assertEquals(1, taxonomyEdges.size());
	}
	
	@Test
	void testMetadataImportWithDifferentBackupProject() throws Throwable {
		final EntityId projectId = EntityId.of(2l);
		assertEquals(0, moduleService.findModulesWithMetaData(projectId).size());
		restoreJob(metadataBackupJson, projectId);
		final var moduleIds = moduleService.findModulesWithMetaData(projectId);
		assertEquals(1, moduleIds.size());
		assertEquals(1, taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleIds.get(0))).size());
		assertEquals(1, annotationService.find(q -> q.ofProject(projectId)).size());
		assertEquals(1, dataDictionaryService.find(q -> q.ofModule(moduleIds.get(0))).size());
	}
	
	@Test
	void testImportWithNoChanges() throws Throwable {
		final EntityId projectId = EntityId.of(3l);
		final var beforeRestoreModuleIds = moduleService.findModulesWithMetaData(projectId);
		assertEquals(1, beforeRestoreModuleIds.size());
		assertEquals(1, taxonomyService.find(q -> q.ofProject(projectId).ofModule(beforeRestoreModuleIds.get(0))).size());
		assertEquals(1, annotationService.find(q -> q.ofProject(projectId).withModulePath("/src/MOD 3")).size());
		assertEquals(2, dataDictionaryService.find(q -> q.ofModule(beforeRestoreModuleIds.get(0))).size());
		metadataBackupJson = FileUtils.readFileToString(new File(BACKUP_FILE_PATH + "backupWithNoChanges.json"), StandardCharsets.UTF_8);
		restoreJob(metadataBackupJson, projectId);
		final var moduleIds = moduleService.findModulesWithMetaData(projectId);
		assertEquals(1, moduleIds.size());
		assertEquals(1, taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleIds.get(0))).size());
		assertEquals(1, annotationService.find(q -> q.ofProject(projectId).withModulePath("/src/MOD 3")).size());
		assertEquals(1, dataDictionaryService.find(q -> q.ofModule(moduleIds.get(0))).size());
	}
	
	@Test
	void testImportWithTaxonomyCategory() throws Throwable {
		assertEquals(2, taxonomyService.findCategories(q -> q.ofProject(PROJECT_ID)).size());
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID).withPath("/src/MOD 1"))
												.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: /src/MOD 1"));
		restoreAndValidateTaxonomy(PROJECT_ID, module.identity(), TAXONOMY_NAME);
		
		/* Validate TaxonomyPojo Category restored in the matched module */
		final List<TaxonomyCategoryPojo> restoredTaxonomyCategories =  taxonomyService.findCategories(q -> q.ofProject(PROJECT_ID));
		assertEquals(2, restoredTaxonomyCategories.size());
		final ProjectPojo project = projectService.get(PROJECT_ID);
		assertEquals(assertNotNull(metaDataBackup).getDefaultTaxonomyCategory().getId(), project.getDefaultTaxonomyCategoryId());
		assertEquals(assertNotNull(metaDataBackup).getTechnicalTaxonomyCategory().getId(), project.getTechnicalTaxonomyCategoryId());
	}
	
	@Test
	void testImportFunctionalBlock() throws Throwable {
		restoreMetaData(PROJECT_ID, metadataBackupJson);

		final Optional<FunctionalBlockPojo> functionalBlock1 = functionalBlockService.find(UUID.fromString("bfc0cbbe-cea9-4647-89ce-c3a344db9ea9"));
		final Optional<FunctionalBlockPojo> functionalBlock2 = functionalBlockService.find(UUID.fromString("43505b91-8d55-4cde-a6f2-ab1e016c9a50"));

		assertFalse(functionalBlock1.isEmpty(), "Functional block should be imported");
		assertFalse(functionalBlock2.isEmpty(), "Functional block should be imported");
		assertEquals("Test Group 1", functionalBlock1.get().getName(), "Name of the Functional Block should be correct after import.");
		assertEquals("Test Unit 1", functionalBlock2.get().getName(), "Name of the Functional Block should be correct after import.");
		assertEquals(1 , functionalBlock1.get().getChildren().size(), "Functional Block children should be imported.");
		assertEquals(functionalBlock2.get().getUid() , functionalBlock1.get().getChildren().get(0), "Functional Block children should be correct.");

	}
	
	@Test
	void testImportFunctionalBlockWithAdditionalData() throws Throwable {
		final File fuctionalBlockFile = new File(BACKUP_FILE_PATH + "FunctionalBlockBackup.json");
		final String metadataBackupJsonFunctionalBlock = FileUtils.readFileToString(fuctionalBlockFile, StandardCharsets.UTF_8);
		metaDataBackup = objectMapper.readValue(metadataBackupJsonFunctionalBlock, MetaDataBackup.class);
		restoreMetaData(PROJECT_ID, metadataBackupJsonFunctionalBlock);

		final Optional<FunctionalBlockPojo> functionalBlock1 = functionalBlockService.find(UUID.fromString("bfc0cbbe-cea9-4647-89ce-c3a344db9ea9"));
		final Optional<FunctionalBlockPojo> functionalBlock2 = functionalBlockService.find(UUID.fromString("43505b91-8d55-4cde-a6f2-ab1e016c9a50"));
		final Optional<FunctionalBlockPojo> functionalBlock3 = functionalBlockService.find(UUID.fromString("61a0a2fc-c6d0-46f5-9ebe-1848999b3c01"));
		final Optional<GeneratedFrom> generatedFormfunctionalBlock3 = functionalBlockService
				.getGeneratedFrom(UUID.fromString("61a0a2fc-c6d0-46f5-9ebe-1848999b3c01"));
		assertFalse(functionalBlock1.isEmpty(), "Functional block should be imported");
		assertFalse(functionalBlock2.isEmpty(), "Functional block should be imported");
		assertEquals("Test Group 1", functionalBlock1.get().getName(), "Name of the Functional Block should be correct after import.");
		assertEquals("Test Unit 1", functionalBlock2.get().getName(), "Name of the Functional Block should be correct after import.");
		assertEquals(2 , functionalBlock1.get().getChildren().size(), "Functional Block children should be imported.");
		assertEquals(functionalBlock2.get().getUid() , functionalBlock1.get().getChildren().get(0), "Functional Block children should be correct.");
		assertEquals(functionalBlock2.get().getUid() , functionalBlock3.get().getChildren().get(0), "Functional Block children should be correct.");
		assertEquals("DummyHash1" , functionalBlock3.get().getModuleParts().get(0).getModuleLinkHash(), "Functional Block module part should be correct.");
		assertFalse(generatedFormfunctionalBlock3.isEmpty(), "Functional Block additional info should be imported.");

		assertEquals("DummyHash1" , generatedFormfunctionalBlock3.get().getModuleLinkHash().get(), "Functional Block additional info should be correct.");

		assertEquals(2 , functionalBlock2.get().getParents().size(), "Functional Block parent size should be correct.");

	}
	
	@Test
	void testImportWithModifiedTaxonomyCategory() throws Throwable {
		final EntityId projectId = EntityId.of(4L);
		metadataBackupJson = FileUtils.readFileToString(new File(BACKUP_FILE_PATH + "differentTaxonomyCategory.json"), StandardCharsets.UTF_8);
		metaDataBackup = objectMapper.readValue(metadataBackupJson, MetaDataBackup.class);
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withPath("/src/MOD 4"))
												.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: /src/MOD 4"));
		assertEquals("Technical Taxonomies", taxonomyService.findTypes(q -> q.ofProject(projectId)).get(0).getCategory().getName());
		restoreAndValidateTaxonomy(projectId, module.identity(), "Taxonomy 2");
		assertEquals("Business Taxonomies", taxonomyService.findTypes(q -> q.ofProject(projectId)).get(0).getCategory().getName());
	}

	@Test
	void testFunctionalBlockBackupRestore() throws Throwable {
		final var moduleId = moduleService.create(new ModulePojoPrototype()
													.setProject(PROJECT_ID)
													.setName("Module A")
													.setTechnology(Technology.COBOL)
													.setType(Type.PROGRAM)
													.setStorage(Storage.FILE)
													.setPath("/src/FuntionalTestBlock")
													.setIdentification(Identification.IDENTIFIED)
													.setOrigin(Origin.CUSTOM)
													.setCreator(Creator.DISCOVERY)
													.setInfo(new HashedMap<>())
													.setDescription("Test data")
													.setContent(""));
		createdModule = moduleService.getModule(moduleId);
		final Long annotationCategory = createAnnotationCategory("test", PROJECT_ID);
		final var annotationOneBeforeRestore = createAnnotation(moduleId, annotationCategory, "Annotation 1");
		final var annotationTwoBeforeRestore = createAnnotation(moduleId, annotationCategory, "Annotation 2");

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> functionalBlockGenerateData = functionalBlockGenerationService
				.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID), moduleId);

		final List<FunctionalBlockPojo> listOffunctionalBlockData = functionalBlockService
				.get(functionalBlockGenerateData.stream().map(Pair::getRight).collect(Collectors.toList()));
		
		final Map<String, AnnotationPojo> annotations = Map.of(annotationOneBeforeRestore.getName(), annotationOneBeforeRestore,
				annotationTwoBeforeRestore.getName(), annotationTwoBeforeRestore);
		
		validateBackupRestoredAnnotaion(annotations, listOffunctionalBlockData);
	}

	@Test
	void testFunctionalBlockRestoreToDifferentProject() throws Throwable {
		functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(PROJECT_ID)
				.setName("Backup Test")
				.setDescription("Test Description"));

		final MetaDataBackup backup = getMetaDataBackup(PROJECT_ID);

		/* This behavior is subject to change in a follow-up ticket: we need to delete the existing FB, as the restore will try to re-create
		 * it with the same UUID. */
		functionalBlockService.deleteAllOfProject(PROJECT_ID);

		final EntityId restoreProjectId = EntityId.of(4L);
		restoreMetaData(restoreProjectId, objectMapper.writeValueAsString(backup));

		final List<FunctionalBlockPojo> restoredBlocks = functionalBlockService.find(q -> q.ofProject(restoreProjectId));
		assertEquals(1, restoredBlocks.size());
		final FunctionalBlockPojo restoredBlock = restoredBlocks.get(0);
		assertEquals(restoreProjectId, restoredBlock.getProject());
		assertEquals("Backup Test", restoredBlock.getName());
		assertEquals("Test Description", restoredBlock.getDescription());
	}

	@Test
	void testFunctionalBlockRestoreToDifferentProjectDuplicateKey() throws Throwable {
		functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(PROJECT_ID)
				.setName("Backup Test")
				.setDescription("Test Description"));

		final MetaDataBackup backup = getMetaDataBackup(PROJECT_ID);

		/* This behavior is subject to change in a follow-up ticket. since the FBs are restored using the UUIDs from the backup,
		 * the restore should fail in case the FB already exists in the database in a different project. Previously, the existing
		 * FBs were moved to the restored project.
		 * For now, this test acts as a safeguard, ensuring that the restore fails instead of modifying existing FBs in different projects.
		 */
		try {
			restoreMetaData(EntityId.of(4L), objectMapper.writeValueAsString(backup));
			fail("Expected restore to fail with duplicate key");
		} catch (final ExecutionException e) {
			assertInstanceOf(DuplicateKeyException.class, e.getCause(), "Expected restore to fail with duplicate key");
		}
	}

	@Test
	void testFunctionalBlockLinkRestore() throws Throwable {
		final UUID childAId = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(PROJECT_ID)
				.setName("Child A")
				.setDescription(""));

		final UUID childBId = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(PROJECT_ID)
				.setName("Child B")
				.setDescription(""));

		final UUID parentBlockId = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(PROJECT_ID)
				.setName("Parent")
				.setDescription("")
				.setChildren(List.of(childAId, childBId)));

		functionalBlockService.setLinks(parentBlockId, List.of(new FunctionalBlockLink(UUID.randomUUID(), parentBlockId, childAId, childBId,
				null, Collections.emptyMap(), null)));

		final MetaDataBackup backup = getMetaDataBackup(PROJECT_ID);

		functionalBlockService.deleteAllOfProject(PROJECT_ID);
		assertEquals(0, functionalBlockService.find(q -> q.ofProject(PROJECT_ID)).size(), "Expected Functional Blocks to be deleted");

		restoreMetaData(PROJECT_ID, objectMapper.writeValueAsString(backup));

		final List<FunctionalBlockPojo> restoredBlocks = functionalBlockService.find(q -> q.ofProject(PROJECT_ID));
		assertEquals(3, restoredBlocks.size(), "Expected all Functional Blocks to be restored");

		UUID restoredParentBlockId = null;
		UUID restoredChildAId = null;
		UUID restoredChildBId = null;

		for (FunctionalBlockPojo restoredBlock : restoredBlocks) {
			if ("Parent".equals(restoredBlock.getName())) {
				restoredParentBlockId = restoredBlock.getUid();
			} else if ("Child A".equals(restoredBlock.getName())) {
				restoredChildAId = restoredBlock.getUid();
			} else if ("Child B".equals(restoredBlock.getName())) {
				restoredChildBId = restoredBlock.getUid();
			} else {
				fail("Unexpected block was restored with name: " + restoredBlock.getName());
			}
		}

		/* assert all blocks were restored */
		assertNotNull(restoredParentBlockId, "Expected Parent Block to be restored");
		assertNotNull(restoredChildAId, "Expected Child A Block to be restored");
		assertNotNull(restoredChildBId, "Expected Child B Block to be restored");

		/* assert children are restored (in correct order) */
		final FunctionalBlockPojo restoredParentBlock = functionalBlockService.find(Objects.requireNonNull(restoredParentBlockId))
				.orElseThrow(() -> new AssertionFailedError("Expected to find the restored parent block"));
		assertEquals(List.of(childAId, childBId), restoredParentBlock.getChildren());

		/* assert link between children is restored */
		final List<FunctionalBlockLink> restoredLinks = functionalBlockService.getLinks(Objects.requireNonNull(restoredParentBlockId));
		assertEquals(1, restoredLinks.size(), "Expected link to be restored");
		final FunctionalBlockLink restoredLink = restoredLinks.get(0);
		assertEquals(restoredChildAId, restoredLink.getChildA());
		assertEquals(restoredChildBId, restoredLink.getChildB());
	}
	
	@Test
	void testImportDDEAnnotationLink() throws Throwable {
		final MetaDataBackup metaDataBackupAlternate = objectMapper.readValue(metadataBackupJsonAlternate, MetaDataBackup.class);

		final DataDictionaryPojo oldDDE = assertNotNull(assertNotNull(assertNotNull(metaDataBackupAlternate).getModules()).get(0).getDataDictionaryEntries()).get(0);
		final AnnotationPojo oldAnno = assertNotNull(assertNotNull(assertNotNull(metaDataBackupAlternate).getModules()).get(0).getAnnotations()).get(0);

		restoreMetaData(PROJECT_ID, metadataBackupJsonAlternate);

		restoredModule = moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID).withName("CPYPST"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with name CPYPST"));
		final DataDictionaryPojo dde = dataDictionaryService.findAny(q -> q.ofModule(assertNotNull(restoredModule).identity()).withDescription("TESTFIELD1"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + assertNotNull(createdModule).identity()));
		final AnnotationPojo anno = annotationService.findAny(q -> q.ofModule(assertNotNull(restoredModule).identity()).withName("Data Validation Rule Candidate [System identified]")
				.withLocation(new ModuleLocation(217, 61)))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + assertNotNull(restoredModule).identity()));
		
		/* Checks if the DDE was linked correctly to the Annotation*/
		final DataDictionaryPojo retrievedDde = dataDictionaryService.findAny(q -> q.ofAnnotation(EntityId.of(anno.getUid())))
				.orElseThrow();
		assertEquals("TESTFIELD1", retrievedDde.getDescription());
		assertEquals(oldDDE.getName(), retrievedDde.getName());
		assertEquals(oldDDE.getLocation(), retrievedDde.getLocation());

		/* Checks if the Annotation was linked correctly to the DDE*/
		final AnnotationPojo retrievedAnno = annotationService.findAny(q -> q.ofDataDictionaryEntry(EntityId.of(dde.getUid())))
				.orElseThrow();
		assertEquals(retrievedAnno.getLocation().get(), new ModuleLocation(217, 61));
		assertEquals(oldAnno.getName(), retrievedAnno.getName());
		assertEquals(oldAnno.getLocation(), retrievedAnno.getLocation());
	}

	@Test
	void testImportDDEAnnotationLinks2() throws Throwable {
		final MetaDataBackup metaDataBackupAlternate2 = objectMapper.readValue(metadataBackupJsonAlternate2, MetaDataBackup.class);

		final DataDictionaryPojo oldDDE = assertNotNull(
				assertNotNull(assertNotNull(metaDataBackupAlternate2).getModules()).get(0).getDataDictionaryEntries()).get(0);
		final AnnotationPojo oldAnno = assertNotNull(assertNotNull(assertNotNull(metaDataBackupAlternate2).getModules()).get(0).getAnnotations()).get(0);

		restoreMetaData(PROJECT_ID, metadataBackupJsonAlternate2);

		final DataDictionaryPojo dde = dataDictionaryService.findAny(q -> q.withDescription("TESTFIELD1"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + assertNotNull(createdModule).identity()));
		final AnnotationPojo anno = annotationService.findAny(
						q -> q.withName("Data Validation Rule Candidate [System identified]").withLocation(new ModuleLocation(217, 61)))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + assertNotNull(createdModule).identity()));

		/* Checks if the DDE was linked correctly to the Annotation*/
		final DataDictionaryPojo retrievedDde = dataDictionaryService.findAny(q -> q.ofAnnotation(EntityId.of(anno.getUid()))).orElseThrow();
		assertEquals(oldDDE.getName(), retrievedDde.getName());
		assertEquals(oldDDE.getLocation(), retrievedDde.getLocation());
		assertEquals("TESTFIELD1", retrievedDde.getDescription());

		/* Checks if the Annotation was linked correctly to the DDE*/
		final AnnotationPojo retrievedAnno = annotationService.findAny(q -> q.ofDataDictionaryEntry(EntityId.of(dde.getUid())))
				.orElseThrow();
		assertEquals(oldAnno.getName(), retrievedAnno.getName());
		assertEquals(oldAnno.getLocation(), retrievedAnno.getLocation());
	}
	
	@Test
	void testImportWithNoChangesZipped() throws Throwable {
		final EntityId projectId = EntityId.of(3l);
		final var beforeRestoreModuleIds = moduleService.findModulesWithMetaData(projectId);
		assertEquals(1, beforeRestoreModuleIds.size());
		assertEquals(1, taxonomyService.find(q -> q.ofProject(projectId).ofModule(beforeRestoreModuleIds.get(0))).size());
		assertEquals(1, annotationService.find(q -> q.ofProject(projectId).withModulePath("/src/MOD 3")).size());
		assertEquals(2, dataDictionaryService.find(q -> q.ofModule(beforeRestoreModuleIds.get(0))).size());
		restoreJob(FileUtils.readFileToByteArray(BACKUP_FILE_ZIPPED_NO_CHANGES), projectId, Map.of("importFormat", List.of("compressed")));
		final var moduleIds = moduleService.findModulesWithMetaData(projectId);
		assertEquals(1, moduleIds.size());
		assertEquals(1, taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleIds.get(0))).size());
		assertEquals(1, annotationService.find(q -> q.ofProject(projectId).withModulePath("/src/MOD 3")).size());
		assertEquals(1, dataDictionaryService.find(q -> q.ofModule(moduleIds.get(0))).size());
	}
	
	@Test
	void testMetaDataImportJobForRestoredDataDictionaryZipped() throws Throwable {
		final String dataElementNameFromBackup =
				assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getDataDictionaryEntries()).get(0).getName();
		assertEquals(DATA_DICTIONARY_NAME, dataElementNameFromBackup);
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(assertNotNull(createdModule).identity())).size());
		/* Run import repeatedly to ensure that no duplications occur */
		for (int pass = 1; pass <= 2; pass++) {
			try {
				restoreJob(FileUtils.readFileToByteArray(BACKUP_FILE_ZIPPED), PROJECT_ID, Map.of("importFormat", List.of("compressed")));
				
				/* Validate data dictionaries restored in the matched module */
				final List<DataDictionaryPojo> restoredDataDictionaryEntries =
						dataDictionaryService.find(q -> q.ofModule(assertNotNull(createdModule).identity()));
				assertEquals(1, restoredDataDictionaryEntries.size());
				final List<DataDictionaryPojo> backedupDataDictionaryEntries =
						assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getDataDictionaryEntries());
				assertEquals(backedupDataDictionaryEntries.size(), restoredDataDictionaryEntries.size());
				validateRestoredDataDictionary(backedupDataDictionaryEntries.get(0), restoredDataDictionaryEntries.get(0));
			} catch (final Throwable e) {
				throw new Exception("In pass " + pass, e);
			}
		}
	}

	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-metadata-import");
	}

	private void restoreAndValidateTaxonomy(final EntityId projectId, final EntityId moduleId, final String expectedTaxonomyName) throws Throwable {
		final String taxonomyName = assertNotNull(assertNotNull(metaDataBackup).getTaxonomies()).get(0).getName();
		assertEquals(expectedTaxonomyName, taxonomyName);
		assertEquals(0, taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleId)).size());
		restoreJob(metadataBackupJson, projectId);
		/* Validate Taxonomies restored in the matched module */
		final List<TaxonomyPojo> restoredTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleId));
		assertEquals(1, restoredTaxonomies.size());
		final List<TaxonomyPojo> backedupTaxonomies = assertNotNull(assertNotNull(assertNotNull(metaDataBackup).getModules()).get(0).getTaxonomies());
		assertEquals(1, backedupTaxonomies.size());
		validateRestoredTaxonomies(projectId, backedupTaxonomies.get(0), restoredTaxonomies.get(0));
	}

	private void restoreMetaData(final EntityId projectId, final String metaDataBackupJson) throws Throwable {
		restoreJob(metaDataBackupJson, projectId);
		restoredModule = moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID).byId(assertNotNull(createdModule).identity()))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + assertNotNull(createdModule).identity()));
	}

	private void restoreJob(final String backupJson, final EntityId projectId) throws Throwable {
		restoreJob(backupJson, projectId, Collections.emptyMap());
	}
	
	private void restoreJob(final String backupString, final EntityId projectId, final Map<String, List<String>> parameters) throws Throwable {
		restoreJob(backupString.getBytes(StandardCharsets.UTF_8), projectId, parameters);
	}

	private void restoreJob(final byte[] backupBytes, final EntityId projectId, final Map<String, List<String>> parameters) throws Throwable {
		final AtomicReference<Throwable> exceptionThrownFromJob = new AtomicReference<>(null);
		final Phaser phaser = new Phaser(2);
		final brave.Span newSpan = tracer.nextSpan().start();
		try (final brave.Tracer.SpanInScope newScope = tracer.withSpanInScope(newSpan)) {
			jobManager.submit(extension.createJob(projectId, parameters, new HttpEntity<>(backupBytes)),
					new JobExecutionCallback() {

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					exceptionThrownFromJob.set(throwable);
					phaser.arrive();
				}

				@Override
				public void onCompletion() {
					phaser.arrive();
				}
			});

			phaser.arriveAndAwaitAdvance();

			if (exceptionThrownFromJob.get() != null) {
				throw exceptionThrownFromJob.get();
			}
		} finally {
			newSpan.finish();
		}
	}

	private MetaDataBackup getMetaDataBackup(final EntityId projectId) throws Throwable {
		final AtomicReference<Throwable> exceptionThrownFromJob = new AtomicReference<>(null);
		final Phaser phaser = new Phaser(2);

		final brave.Span newSpan = tracer.nextSpan().start();
		try (final brave.Tracer.SpanInScope newScope = tracer.withSpanInScope(newSpan)) {
			final String jobId = jobManager.submit(extensions.createJob(projectId, Collections.emptyMap()), new JobExecutionCallback() {

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					exceptionThrownFromJob.set(throwable);
					phaser.arrive();
				}

				@Override
				public void onCompletion() {
					phaser.arrive();
				}
			}).getJobId();

			phaser.arriveAndAwaitAdvance();

			if (exceptionThrownFromJob.get() != null) {
				throw exceptionThrownFromJob.get();
			}

			final String jobResult = FileUtils.readFileToString(Paths.get(jobConfig.getJobResultFolder(), jobId).toFile(), StandardCharsets.UTF_8);
			jobIds.add(jobId);

			final MetaDataBackup backup = objectMapper.readValue(jobResult, MetaDataBackup.class);

			return assertNotNull(backup, "No back up data found");
		} finally {
			newSpan.finish();
		}
	}

	private void validateRestoredModuleMetaData(final ModuleBackup backedUpModule, final ModulePojo restoredModule) {
		final var modules = moduleService.findModulesWithMetaData(PROJECT_ID);
		assertEquals(2, modules.size());
		assertEquals(backedUpModule.getName(), restoredModule.getName());
		assertEquals(backedUpModule.getPath(), restoredModule.getPath().get());
		assertEquals(backedUpModule.getTechnology(), restoredModule.getTechnology());
		final Set<Long> nids = modules.stream().map(m -> m.getNid()).collect(Collectors.toSet());
		assertTrue(nids.contains(restoredModule.getId()));
		assertEquals(backedUpModule.getDescription(), restoredModule.getDescription().get());
		assertEquals(1, restoredModule.getCustomProperties().size());
		final Map<String, Object> customProperties1 = restoredModule.getCustomProperties().getSub(customPropertiesService.getDefaultClassName(PROJECT_ID.getNid(), "Module"));
		assertEquals(2, customProperties1.size());
		assertEquals("ABC", customProperties1.get("customMetaInfo1"));
		assertEquals("123", customProperties1.get("customMetaInfo2"));
	}

	private void validateRestoredTaxonomies(final EntityId projectId, final TaxonomyPojo backedupTaxonomy, final TaxonomyPojo restoredTaxonomy) {
		assertEquals(projectId, restoredTaxonomy.getProject());
		assertEquals(backedupTaxonomy.getName(), restoredTaxonomy.getName());
		assertEquals(backedupTaxonomy.getType().getName(), restoredTaxonomy.getType().getName());
		assertEquals(backedupTaxonomy.getType().getCategory().getName(), restoredTaxonomy.getType().getCategory().getName());
	}

	private void validateBackupRestoredAnnotaion(final Map<String, AnnotationPojo> annotations, final List<FunctionalBlockPojo> listOffunctionalBlockData)
			throws Throwable {
		final List<UUID> functionalBlockIds = new ArrayList<>();
		annotations.forEach((annotationName, annotation) -> {
			final FunctionalBlockPojo functionalBlockData = listOffunctionalBlockData.stream().filter(ele -> ele.getName().equals(annotationName)).findFirst()
					.get();
			assertEquals(Arrays.asList("FUNCTIONAL_UNIT"), functionalBlockData.getFlags().get("TYPE"));
			final UUID functionalBlockId = functionalBlockData.getUid();
			functionalBlockIds.add(functionalBlockId);
			final Optional<GeneratedFrom> generatedFromBeforeRestore = functionalBlockService.getGeneratedFrom(functionalBlockId);
			assertEquals(annotation.getId(), generatedFromBeforeRestore.get().getAnnotationId().get().getNid());
		});
		final MetaDataBackup backup = getMetaDataBackup(PROJECT_ID);
		listOffunctionalBlockData.forEach(functionalBlockElement -> {
			functionalBlockService.delete(functionalBlockElement.getUid());
		});
		final String restoredBackupMetaData = objectMapper.writeValueAsString(backup);
		restoreMetaData(PROJECT_ID, restoredBackupMetaData);
		functionalBlockIds.forEach(functionalBlockId -> {
			final Optional<FunctionalBlockPojo> functionalBlock = functionalBlockService.find(UUID.fromString(functionalBlockId.toString()));
			final Optional<GeneratedFrom> generatedFromAfterRestore = functionalBlockService.getGeneratedFrom(functionalBlock.get().getUid());
			final Optional<AnnotationPojo> annotationAfterRestore = annotationService.findAny(q -> q.ofProject(PROJECT_ID).byId(EntityId.of(generatedFromAfterRestore.get().getAnnotationId().get().getNid())));
			assertTrue(annotationAfterRestore.isPresent());
			assertNotEquals(assertNotNull(annotations.get(annotationAfterRestore.get().getName())).getId(),
					annotationAfterRestore.get().getId());
			assertEquals(assertNotNull(annotations.get(annotationAfterRestore.get().getName())).getCategoryName(),
					annotationAfterRestore.get().getCategoryName());
			assertEquals(assertNotNull(annotations.get(annotationAfterRestore.get().getName())).getName(),
					annotationAfterRestore.get().getName());
			assertEquals(assertNotNull(annotations.get(annotationAfterRestore.get().getName())).getCreatedByUserName(),
					annotationAfterRestore.get().getCreatedByUserName());
			assertEquals(assertNotNull(annotations.get(annotationAfterRestore.get().getName())).getType(),
					annotationAfterRestore.get().getType());
		});
	}

	private void validateRestoredDataDictionary(final DataDictionaryPojo backedupDataDictionaryEntry, final DataDictionaryPojo restoredDataDictionaryEntry) {
		assertEquals(backedupDataDictionaryEntry.getCreatedByUserId(), restoredDataDictionaryEntry.getCreatedByUserId());
		assertEquals(backedupDataDictionaryEntry.getName(), restoredDataDictionaryEntry.getName());
		assertEquals(backedupDataDictionaryEntry.getDescription(), restoredDataDictionaryEntry.getDescription());
		assertEquals(backedupDataDictionaryEntry.getScopes().size(), restoredDataDictionaryEntry.getScopes().size());
		assertEquals(2, restoredDataDictionaryEntry.getScopes().size());
		assertEquals(backedupDataDictionaryEntry.getScopes(), restoredDataDictionaryEntry.getScopes());
		assertEquals(backedupDataDictionaryEntry.getFormat(), restoredDataDictionaryEntry.getFormat());
		assertEquals(backedupDataDictionaryEntry.getLength(), restoredDataDictionaryEntry.getLength());
		assertEquals(backedupDataDictionaryEntry.getUpdatedByUserId(), restoredDataDictionaryEntry.getUpdatedByUserId());
		assertEquals(backedupDataDictionaryEntry.getLocation(), restoredDataDictionaryEntry.getLocation());
		assertNotEquals(backedupDataDictionaryEntry.identity().getNid(), restoredDataDictionaryEntry.identity().getNid());
		assertNotEquals(backedupDataDictionaryEntry.identity().getUid(), restoredDataDictionaryEntry.identity().getUid());
	}

	private void validateRestoredAnnotation(final AnnotationPojo backedUpAnnotation, final AnnotationPojo restoredAnnotation) {
		assertEquals(backedUpAnnotation.getName(), restoredAnnotation.getName());
		assertEquals(backedUpAnnotation.getCategoryName(), restoredAnnotation.getCategoryName());
		assertEquals(backedUpAnnotation.getType(), restoredAnnotation.getType());
		assertEquals(backedUpAnnotation.getState(), restoredAnnotation.getState());
		assertEquals(backedUpAnnotation.getCreatedByUserId(), restoredAnnotation.getCreatedByUserId());
		assertEquals(backedUpAnnotation.getUpdatedByUserId(), restoredAnnotation.getUpdatedByUserId());
		assertEquals(backedUpAnnotation.getSourceAttachment(), restoredAnnotation.getSourceAttachment());
		assertNotEquals(backedUpAnnotation.identity().getNid(), restoredAnnotation.identity().getNid());
	}

	private AnnotationPojo createAnnotation(final EntityId module, final Long category, final String annotaionName) {
		return annotationService.get(annotationService.create(new AnnotationPojoPrototype()
				.setModule(module)
				.setLocation(new ModuleLocation(47, 11))
				.setName(annotaionName)
				.setCategoryId(category)
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setCreatedByUserId("admin")
				.setUpdatedByUserId("admin")));
	}

	private Long createAnnotationCategory(final String name, final EntityId projectId) {
		return annotationService.createCategory(projectId, name, Collections.emptyList());
	}
}
