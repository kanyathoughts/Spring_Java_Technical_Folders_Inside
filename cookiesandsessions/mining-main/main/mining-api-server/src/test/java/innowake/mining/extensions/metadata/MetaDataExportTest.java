/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.metadata;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Phaser;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.fasterxml.jackson.databind.ObjectMapper;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.metadata.model.MetaDataBackup;
import innowake.mining.extensions.metadata.model.ModuleBackup;
import innowake.mining.server.functionalblocks.backup.model.FunctionalBlockBackup;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkCondition;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests for {@link MetaDataExportExtension} to validate the export of metadata such as {@linkplain AnnotationPojo annotations}, {@linkplain DataDictionaryPojo
 * data dictionaries} and {@linkplain TaxonomyPojo taxonomies} associated with the backed up {@linkplain Module module}.
 */
@WithMockUser
class MetaDataExportTest extends DatabaseResettingTest {

	@Autowired
	private Tracer tracer;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private JobConfigurationProperties jobConfig;

	@Autowired
	private MetaDataExportExtension extension;

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private TaxonomyService taxonomyService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private FunctionalBlockService functionalBlockService;
	
	private final List<String> jobIds = new ArrayList<>();
	
	private static final String DEFAULT_TAXONOMY_CATEGORY_NAME = "Business Taxonomies";
	private static final String TECHNICAL_TAXONOMY_CATEGORY_NAME = "Technical Taxonomies";
	private static final String WEEK_TAGS = "weekTags";
	private static final String ANNOTATION = "Annotation";
	
	private EntityId projectId = EntityId.VOID;
	
	@BeforeEach
	void createProject() {
		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY)))
			).identity();
	}
	
	@AfterAll
	void deleteBackedUpJson() {
		jobIds.forEach(jobId -> Paths.get(jobConfig.getJobResultFolder(), jobId).toFile().deleteOnExit());
	}

	@Test
	void testMetaDataExportJobWithDescription() throws Throwable {
		final var module = createModuleWithoutMetadata("MOD 1");
		final MetaDataBackup backup = getMetaDataBackup();
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		assertEquals(DEFAULT_TAXONOMY_CATEGORY_NAME, backup.getDefaultTaxonomyCategory().getName());
		assertEquals(TECHNICAL_TAXONOMY_CATEGORY_NAME, backup.getTechnicalTaxonomyCategory().getName());
		final List<AnnotationCategory> annotationCategories = assertNotNull(backup.getAnnotationCategories());
		assertEquals(11, annotationCategories.size());
		final List<TaxonomyPojo> metadataTaxonomies = assertNotNull(backup.getTaxonomies());
		assertEquals(13, metadataTaxonomies.size());
		assertEquals(0, assertNotNull(backup.getDefaultCategoryTaxonomyTypes()).size());
		assertEquals(4, assertNotNull(backup.getTechnicalCategoryTaxonomyTypes()).size());
		
		final List<ModuleBackup> backUpModules = assertNotNull(backup.getModules(), "No modules found for back up");
		assertEquals(1, backUpModules.size());
		final ModuleBackup backedUpModule = backUpModules.get(0);
		validateModule(module, backedUpModule);

		/* Check Annotations, Taxonomies and Data Dictionaries are empty */
		assertEquals(0, assertNotNull(backedUpModule.getAnnotations()).size());
		assertEquals(0, assertNotNull(backedUpModule.getTaxonomies()).size());
		assertEquals(0, assertNotNull(backedUpModule.getDataDictionaryEntries()).size());
	}

	@Test
	void testMetaDataExportJobWithTaxonomy() throws Throwable {
		final var module = createModuleWithoutMetadata("MOD 1");
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("DataDomain")
				.setProject(projectId));
		final TaxonomyTypePojo type = taxonomyService.getType(typeId);
		final TaxonomyPojo taxonomy = createTaxonomy(type, "TaxonomyPojo 1");
		taxonomyService.createModuleLink(module.getUid(), taxonomy.identity());
		
		final MetaDataBackup backup = getMetaDataBackup();
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		assertEquals(DEFAULT_TAXONOMY_CATEGORY_NAME, backup.getDefaultTaxonomyCategory().getName());
		assertEquals(TECHNICAL_TAXONOMY_CATEGORY_NAME, backup.getTechnicalTaxonomyCategory().getName());
		final List<AnnotationCategory> annotationCategories = assertNotNull(backup.getAnnotationCategories());
		assertEquals(11, annotationCategories.size());
		final List<TaxonomyPojo> metadataTaxonomies = assertNotNull(backup.getTaxonomies());
		assertEquals(14, metadataTaxonomies.size());
		validateBackedupTaxonomies(taxonomy, metadataTaxonomies);
		assertEquals(1, assertNotNull(backup.getDefaultCategoryTaxonomyTypes()).size());
		assertEquals(4, assertNotNull(backup.getTechnicalCategoryTaxonomyTypes()).size());
		
		final List<ModuleBackup> backUpModules = assertNotNull(backup.getModules(), "No modules found for back up");
		assertEquals(1, backUpModules.size());
		final ModuleBackup backedUpModule = backUpModules.get(0);
		validateModule(module, backedUpModule);

		/* Check Annotations and Data Dictionaries are empty */
		assertEquals(0, assertNotNull(backedUpModule.getAnnotations()).size());
		assertEquals(0, assertNotNull(backedUpModule.getDataDictionaryEntries()).size());

		/* Check TaxonomyPojo back up */
		final List<TaxonomyPojo> taxonomies = assertNotNull(backedUpModule.getTaxonomies());
		assertEquals(1, taxonomies.size());
		validateBackedupTaxonomies(taxonomy, taxonomies);
	}
	
	@Test
	void testMetaDataExportJobWithTechnicalTaxonomyCategory() throws Throwable {
		final var module = createModuleWithoutMetadata("MOD 1");
		final Long categoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName(TECHNICAL_TAXONOMY_CATEGORY_NAME).setProject(projectId));
		final var typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId).setCategoryId(categoryId));

		final TaxonomyTypePojo type = taxonomyService.getType(typeId);
		final TaxonomyPojo taxonomy = createTaxonomy(type, "TaxonomyPojo 1");
		taxonomyService.createModuleLink(module.getUid(), taxonomy.identity());
		final MetaDataBackup backup = getMetaDataBackup();
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		assertEquals(DEFAULT_TAXONOMY_CATEGORY_NAME, backup.getDefaultTaxonomyCategory().getName());
		assertEquals(TECHNICAL_TAXONOMY_CATEGORY_NAME, backup.getTechnicalTaxonomyCategory().getName());
		final List<AnnotationCategory> annotationCategories = assertNotNull(backup.getAnnotationCategories());
		assertEquals(11, annotationCategories.size());
		final List<TaxonomyPojo> metadataTaxonomies = assertNotNull(backup.getTaxonomies());
		assertEquals(14, metadataTaxonomies.size());
		validateBackedupTaxonomies(taxonomy, metadataTaxonomies);
		assertEquals(0, assertNotNull(backup.getDefaultCategoryTaxonomyTypes()).size());
		assertEquals(5, assertNotNull(backup.getTechnicalCategoryTaxonomyTypes()).size());
		
		final List<ModuleBackup> backUpModules = assertNotNull(backup.getModules(), "No modules found for back up");
		assertEquals(1, backUpModules.size());
		final ModuleBackup backedUpModule = backUpModules.get(0);
		validateModule(module, backedUpModule);

		/* Check Annotations and Data Dictionaries are empty */
		assertEquals(0, assertNotNull(backedUpModule.getAnnotations()).size());
		assertEquals(0, assertNotNull(backedUpModule.getDataDictionaryEntries()).size());

		/* Check TaxonomyPojo back up */
		final List<TaxonomyPojo> taxonomies = assertNotNull(backedUpModule.getTaxonomies());
		assertEquals(1, taxonomies.size());
		validateBackedupTaxonomies(taxonomy, taxonomies);
	}

	@Test
	void testMetaDataExportJobWithAnnotation() throws Throwable {
		final var module = createModuleWithoutMetadata("MOD 1");
		final AnnotationCategory annotationCategory =  createAnnotationCategory("Annotation Category B");
		final AnnotationPojo annotation = createAnnotation(module, annotationCategory);
		final MetaDataBackup backup = getMetaDataBackup();
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		assertEquals(DEFAULT_TAXONOMY_CATEGORY_NAME, backup.getDefaultTaxonomyCategory().getName());
		assertEquals(TECHNICAL_TAXONOMY_CATEGORY_NAME, backup.getTechnicalTaxonomyCategory().getName());
		final List<AnnotationCategory> annotationCategories = assertNotNull(backup.getAnnotationCategories());
		/* We don't care about the first AnnotationCategories created by the migration V1.2.94 so we sort by ID*/
		annotationCategories.sort((final AnnotationCategory a, final AnnotationCategory b) -> a.getId().compareTo(b.getId()));
		assertEquals(annotationCategory.getName(), annotationCategories.get(annotationCategories.size() - 1).getName());
		final List<TaxonomyPojo> metadataTaxonomies = assertNotNull(backup.getTaxonomies());
		assertEquals(13, metadataTaxonomies.size());
		assertEquals(0, assertNotNull(backup.getDefaultCategoryTaxonomyTypes()).size());
		assertEquals(4, assertNotNull(backup.getTechnicalCategoryTaxonomyTypes()).size());
		
		final List<ModuleBackup> backUpModules = assertNotNull(backup.getModules(), "No modules found for back up");
		assertEquals(1, backUpModules.size());
		final ModuleBackup backedUpModule = backUpModules.get(0);
		validateModule(module, backedUpModule);

		/* Check Taxonomies and Data Dictionaries are empty */
		assertEquals(0, assertNotNull(backedUpModule.getTaxonomies()).size());
		assertEquals(0, assertNotNull(backedUpModule.getDataDictionaryEntries()).size());

		/* Check Annotation back up */
		final List<AnnotationPojo> annotations = assertNotNull(backedUpModule.getAnnotations());
		assertEquals(1, annotations.size());
		final AnnotationPojo backedUpAnnotation = annotations.get(0);
		validateBackedupAnnotation(annotation, backedUpAnnotation);
		
		/* Check Custom Properties Backup */
		final Map<String, List<CustomPropertyMetadata>> entityCustomProperties = 
				assertNotNull(backup.getEntityCustomProperties(), "Map of Entity and Custom Properties should not be null");
		assertEquals(1, entityCustomProperties.size());
		assertEquals(1, entityCustomProperties.get(ANNOTATION).size());
		assertEquals(WEEK_TAGS, entityCustomProperties.get(ANNOTATION).get(0).getName());
	}

	@Test
	void testMetaDataExportJobWithDataDictionary() throws Throwable {
		final var module = createModuleWithoutMetadata("MOD 1");
		final DataDictionaryPojo dataDictionaryEntry = createDataDictionaryEntry(module, "Data Dictionary 1");
		final MetaDataBackup backup = getMetaDataBackup();
		final List<ModuleBackup> backUpModules = assertNotNull(backup.getModules(), "No modules found for back up");
		assertEquals(1, backUpModules.size());
		final ModuleBackup backedUpModule = backUpModules.get(0);
		validateModule(module, backedUpModule);
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		assertEquals(DEFAULT_TAXONOMY_CATEGORY_NAME, backup.getDefaultTaxonomyCategory().getName());
		assertEquals(TECHNICAL_TAXONOMY_CATEGORY_NAME, backup.getTechnicalTaxonomyCategory().getName());
		final List<AnnotationCategory> annotationCategories = assertNotNull(backup.getAnnotationCategories());
		assertEquals(11, annotationCategories.size());
		final List<TaxonomyPojo> metadataTaxonomies = assertNotNull(backup.getTaxonomies());
		assertEquals(13, metadataTaxonomies.size());
		assertEquals(0, assertNotNull(backup.getDefaultCategoryTaxonomyTypes()).size());
		assertEquals(4, assertNotNull(backup.getTechnicalCategoryTaxonomyTypes()).size());
		
		/* Check Taxonomies and Annotations are empty */
		assertEquals(0, assertNotNull(backedUpModule.getTaxonomies()).size());
		assertEquals(0, assertNotNull(backedUpModule.getAnnotations()).size());

		/* Check Data Dictionaries back up */
		final List<DataDictionaryPojo> dataDictionaryEntries = assertNotNull(backedUpModule.getDataDictionaryEntries());
		assertEquals(1, dataDictionaryEntries.size());
		final DataDictionaryPojo backedUpDataDictionary = dataDictionaryEntries.get(0);
		validateBackedupDataDictionaries(dataDictionaryEntry, backedUpDataDictionary);
	}
	@Test
	void testMetaDataExportJobWithFunctionalBlockMetaData() throws Throwable {
		final var module = createModuleWithoutMetadata("FunctionalBlockTestModule");

		final FunctionalBlockPojoPrototype functionalblock1 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)), null, null, module);
		final FunctionalBlockPojoPrototype functionalblock4 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)), null, null, module);
		final List<UUID> childUIDs = new ArrayList<>();
		childUIDs.add(functionalblock1.uid.get());
		childUIDs.add(functionalblock4.uid.get());

		final FunctionalBlockPojoPrototype functionalblock2 = createFunctionalBlockPojoPrototype(projectId, childUIDs, "Test Group 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)),
				Collections.singletonList(new FunctionalBlockLink(null, null, functionalblock1.uid.get(),
						functionalblock4.uid.get(), null, null, new FunctionalBlockLinkCondition(UUID.randomUUID(),"Label1"))),
				null, module);
		
		final FunctionalBlockPojoPrototype functionalblock3 = createFunctionalBlockPojoPrototype(projectId,
				Collections.singletonList(functionalblock2.uid.get()), "Test UNit 3", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)),
				null, null, module);
		
		final MetaDataBackup backup = getMetaDataBackup();
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		
		/* Check Functional Block */
		final List<FunctionalBlockBackup> functionalBlocks = assertNotNull(backup.getFunctionalBlocks());
		assertEquals(4, functionalBlocks.size());
		
		validateBackedupFunctionalBlocks(functionalblock2, assertNotNull(backup.getFunctionalBlocks()));
		validateBackedupFunctionalBlocks(functionalblock3, assertNotNull(backup.getFunctionalBlocks()));

	}
	
	@Test
	void testMetaDataExportJobWithAllMetaData() throws Throwable {
		final var module = createModuleWithoutMetadata("MOD 1");
		final DataDictionaryPojo dataDictionaryEntry = createDataDictionaryEntry(module, "Data Dictionary 1");
		final AnnotationCategory annotationCategory =  createAnnotationCategory("Annotation Category A");
		final AnnotationPojo annotation = createAnnotation(module, annotationCategory);
		final var typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId));
		final TaxonomyTypePojo type = taxonomyService.getType(typeId);
		final TaxonomyPojo taxonomy = createTaxonomy(type, "TaxonomyPojo 1");
		taxonomyService.createModuleLink(module.getUid(), taxonomy.identity());
		final FunctionalBlockPojoPrototype functionalblock1 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)),null, null, module);
		final List<UUID> childUIDs = new ArrayList<>();
		childUIDs.add(functionalblock1.uid.get());

		final FunctionalBlockPojoPrototype functionalblock2 = createFunctionalBlockPojoPrototype(projectId, childUIDs, "Test Group 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)), null, null, module);
		final MetaDataBackup backup = getMetaDataBackup();
		
		/* Validate meta data backup */
		assertNotNull(backup.getApiServerVersion(), "Api version cannot be null");
		assertNotNull(backup.getBackupDate(), "Back up date cannot be null");
		assertEquals(DEFAULT_TAXONOMY_CATEGORY_NAME, backup.getDefaultTaxonomyCategory().getName());
		assertEquals(TECHNICAL_TAXONOMY_CATEGORY_NAME, backup.getTechnicalTaxonomyCategory().getName());
		final List<AnnotationCategory> annotationCategories = assertNotNull(backup.getAnnotationCategories());
		/* We don't care about the first 8 AnnotationCategories created by the migration V1.2.94 */
		annotationCategories.sort((final AnnotationCategory a, final AnnotationCategory b) -> a.getId().compareTo(b.getId()));
		assertEquals(annotationCategory.getName(), annotationCategories.get(annotationCategories.size() - 1).getName());
		final List<TaxonomyPojo> metadataTaxonomies = assertNotNull(backup.getTaxonomies());
		assertEquals(14, metadataTaxonomies.size());
		validateBackedupTaxonomies(taxonomy, metadataTaxonomies);
		assertEquals(1, assertNotNull(backup.getDefaultCategoryTaxonomyTypes()).size());
		assertEquals(4, assertNotNull(backup.getTechnicalCategoryTaxonomyTypes()).size());
		
		final List<ModuleBackup> backUpModules = assertNotNull(backup.getModules(), "No modules found for back up");
		assertEquals(1, backUpModules.size());
		final ModuleBackup backedUpModule = backUpModules.stream()
				.filter(m -> m.getName().equals(module.getName())).findFirst().get();
		validateModule(module, backedUpModule);

		/* Check Data Dictionaries back up */
		final List<DataDictionaryPojo> dataDictionaryEntries = assertNotNull(backedUpModule.getDataDictionaryEntries());
		assertEquals(1, dataDictionaryEntries.size());
		final DataDictionaryPojo backedUpDataDictionary = dataDictionaryEntries.get(0);
		validateBackedupDataDictionaries(dataDictionaryEntry, backedUpDataDictionary);

		/* Check Annotation back up */
		final List<AnnotationPojo> annotations = assertNotNull(backedUpModule.getAnnotations());
		assertEquals(1, annotations.size());
		final AnnotationPojo backedUpAnnotation = annotations.get(0);
		validateBackedupAnnotation(annotation, backedUpAnnotation);

		/* Check TaxonomyPojo back up */
		final List<TaxonomyPojo> taxonomies = assertNotNull(backedUpModule.getTaxonomies());
		assertEquals(1, taxonomies.size());
		validateBackedupTaxonomies(taxonomy, taxonomies);
		
		/* Check Functional Block */
		final List<FunctionalBlockBackup> functionalBlocks = assertNotNull(backup.getFunctionalBlocks());
		assertEquals(2, functionalBlocks.size());
		
		validateBackedupFunctionalBlocks(functionalblock2, assertNotNull(backup.getFunctionalBlocks()));
	}
	
	private MetaDataBackup getMetaDataBackup() throws Throwable {
		final AtomicReference<Throwable> exceptionThrownFromJob = new AtomicReference<>(null);
		final Phaser phaser = new Phaser(2);

		final Span newSpan = tracer.nextSpan().start();
		try (final Tracer.SpanInScope newScope = tracer.withSpanInScope(newSpan)) {
			final String jobId = jobManager.submit(extension.createJob(projectId, Collections.emptyMap()), new JobExecutionCallback() {

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

	private ModulePojo createModuleWithoutMetadata(final String moduleName) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName(moduleName)
				.setPath("/src/" + moduleName)
				.setProject(projectId)
				.setTechnology(Technology.COBOL)
				.setType(Type.UNKNOWN)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				.setDescription("Test data")
				.setContent("");
		
		return moduleService.getModule(moduleService.create(module));
	}

	private TaxonomyPojo createTaxonomy(final TaxonomyTypePojo taxonomyType, final String taxonomyName) {
		final TaxonomyPojoPrototype proto = new TaxonomyPojoPrototype()
					.setName(taxonomyName)
					.setProject(projectId)
					.setType(taxonomyType.getId());
		
		final EntityId id = taxonomyService.create(proto);
		return taxonomyService.get(id);
	}

	private AnnotationPojo createAnnotation(final ModulePojo module, final AnnotationCategory category) {
		/* Create Custom Property record and add in Annotation */
		final String autoCompKey = "weekAutoCompletionKey";
		final String tagClassName = ANNOTATION + "CustomProperties" + projectId.getNid();
		final CustomPropertyMetadata propertyMetaData = createCustomPropertyMetadata(autoCompKey, tagClassName);
		final List<String> weekTags = Arrays.asList("sunday", "monday", "tuesday");
		final Map<String, Set<String>> map = new HashMap<>();
		map.put(autoCompKey, new HashSet<>(weekTags));
		customPropertiesService.putEnumValues(projectId, map);
		assertTrue(customPropertiesService.getEnumValues(projectId, autoCompKey).containsAll(weekTags), "All entries should match for key: " + autoCompKey);
		
		/* Update Custom Property class in Project */
		customPropertiesService.assignProperty(projectId, MiningEnitityNames.ANNOTATION, tagClassName);
		
		return annotationService.get(annotationService.create(new AnnotationPojoPrototype()
				.setModule(module.identity())
				.setLocation(new ModuleLocation(47, 11))
				.setName("Annotation 1")
				.setCreatedByUserId("admin")
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setCategoryId(category.getId())
				.setCustomProperties(new NestedMap()
					.set(tagClassName, propertyMetaData.getName(), Arrays.asList("sunday", "monday")))
			));
	}

	private CustomPropertyMetadata createCustomPropertyMetadata(@Nullable final String autoCompKey, final String className) {
		/* flacky: delete custom properties that might have been created by other tests */
		customPropertiesService.findPropertyDefinitions(q -> q.withParent(className))
			.forEach(metaData -> customPropertiesService.deleteProperty(projectId, ANNOTATION, metaData.getName()));
		
		final CustomPropertyMetadata customPropertyMetadata = new CustomPropertyMetadata();
		customPropertyMetadata.setName(WEEK_TAGS);
		customPropertyMetadata.setLabel("Test Annotation TAGS Custom Label");
		customPropertyMetadata.setDescription("Test Annotation TAGS Custom Label");
		customPropertyMetadata.setDataType("EMBEDDEDLIST");
		customPropertyMetadata.setFieldType(CustomPropertyFieldType.TAG);
		customPropertyMetadata.setCustomViewIndex(1);
		customPropertyMetadata.setPluginVisible(false);
		customPropertyMetadata.setAutoCompletionKey(autoCompKey);
		customPropertiesService.defineProperty(projectId, ANNOTATION, customPropertyMetadata.getName(), customPropertyMetadata);
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(customPropertyMetadata.getName())),
				customPropertyMetadata.getName() + " should exist in " + className);
		
		return customPropertyMetadata;
	}

	private DataDictionaryPojo createDataDictionaryEntry(final ModulePojo module, final String dataDictionaryEntryName) {
		final DataDictionaryPojoPrototype dataDictionaryEntry = new DataDictionaryPojoPrototype()
				.setName(dataDictionaryEntryName)
				.setDescription("Data Dictionary Entry")
				.setCreatedByUserId("admin")
				.setModule(module.identity())
				.setLocation(new ModuleLocation(47, 11))
				.setScopes(Map.of(DataDictionaryVariableScope.FILE, Map.of(), DataDictionaryVariableScope.SQL_DATABASE, Map.of()));
		return dataDictionaryService.create(dataDictionaryEntry);
	}
	
	private AnnotationCategory createAnnotationCategory(final String name) {
		return annotationService.getCategory(projectId, annotationService.createCategory(projectId, name, Collections.emptyList()));
	}

	private void validateModule(final ModulePojo expectedModule, final ModuleBackup backedUpModule) {

		assertEquals(expectedModule.getName(), backedUpModule.getName());
		assertEquals(expectedModule.getPath().get(), backedUpModule.getPath());
		assertEquals(expectedModule.getTechnology(), backedUpModule.getTechnology());
		assertEquals(expectedModule.getDescription().get(), backedUpModule.getDescription());
		assertEquals(expectedModule.getType(), backedUpModule.getType());
		assertEquals(new String(expectedModule.getContentHash().get().get()), new String(backedUpModule.getContentHash()));
		assertNotNull(backedUpModule.getCustomProperties());
	}
	
	private void validateBackedupTaxonomies(final TaxonomyPojo taxonomy, final List<TaxonomyPojo> metadataTaxonomies) {
		final TaxonomyPojo metadataTaxonomy = metadataTaxonomies.stream().filter(metaDataTaxanomy -> metaDataTaxanomy.getName().equals(taxonomy.getName())).findFirst().get();
		assertNotNull(metadataTaxonomy);
		assertEquals(taxonomy.getName(), metadataTaxonomy.getName());
		assertEquals(taxonomy.getProject(), metadataTaxonomy.getProject());
		assertEquals(taxonomy.getType().getName(), metadataTaxonomy.getType().getName());
		assertEquals(taxonomy.getType().getCategory().getName(), metadataTaxonomy.getType().getCategory().getName());
		assertEquals(taxonomy.getType().getCategory().getProject(), metadataTaxonomy.getType().getCategory().getProject());
	}

	private void validateBackedupFunctionalBlocks(final FunctionalBlockPojoPrototype functionalBlock, final List<FunctionalBlockBackup> functionalBlockBackup) {
		final FunctionalBlockPojo functionalBlockPojo = functionalBlockBackup.stream()
				.filter(fBlock -> fBlock.getFunctionalBlock().getName().equals(functionalBlock.name.get())).findFirst().get().getFunctionalBlock();
		assertEquals(assertNotNull(functionalBlock.flags.get()).get("TYPE").toString(), functionalBlockPojo.getFlags().get("TYPE").toString(),
				"Types of functional Block should be as expected.");
		assertEquals(functionalBlock.children.get(), functionalBlockPojo.getChildren(), "Functional Block's childrens should be correct");
		assertEquals(assertNotNull(functionalBlock.moduleParts.get()).size(), functionalBlockPojo.getModuleParts().size(), "modulepart should be similar");
	}

	private void validateBackedupDataDictionaries(final DataDictionaryPojo dataDictionaryEntry, final DataDictionaryPojo backedUpDataDictionary) {
		assertEquals(dataDictionaryEntry.getCreatedByUserId(), backedUpDataDictionary.getCreatedByUserId());
		assertEquals(dataDictionaryEntry.getName(), backedUpDataDictionary.getName());
		assertEquals(dataDictionaryEntry.getDescription(), backedUpDataDictionary.getDescription());
		assertEquals(dataDictionaryEntry.getScopes().size(), backedUpDataDictionary.getScopes().size());
		assertNotNull(backedUpDataDictionary.getLocation());
	}

	private void validateBackedupAnnotation(final AnnotationPojo annotation, final AnnotationPojo backedUpAnnotation) {
		assertEquals(annotation.getName(), backedUpAnnotation.getName());
		assertEquals(annotation.getProjectNid(), backedUpAnnotation.getProjectNid());
		assertEquals(annotation.getCategoryId(), backedUpAnnotation.getCategoryId());
		assertNotNull(backedUpAnnotation.getCategoryName());
		assertEquals(annotation.getId(), backedUpAnnotation.getId());
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final EntityId project, final @Nullable List<UUID> children, final String name,
			final String description, final @Nullable Map<String, Object> flags, @Nullable final List<FunctionalBlockLink> links,
			@Nullable final Map<UUID, FunctionalBlockLinkCondition> conditions, final ModulePojo module) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype= new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(description);	
		functionalBlockPojoPrototype.setModuleParts(Collections.singletonList(new ModulePart(module.getLinkHash(), new ModuleLocation(10, 10))));

		if (children != null) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		final UUID uid = functionalBlockService.create(functionalBlockPojoPrototype);
		functionalBlockPojoPrototype.setUid(uid);
		return functionalBlockPojoPrototype;
	}
	
}
