/* Copyright (c) 2024 Deloitte. All rights reserved. */

package innowake.mining.extensions.export.csv;

import static innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob;
import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.testcontainers.shaded.org.hamcrest.MatcherAssert.assertThat;
import static org.testcontainers.shaded.org.hamcrest.Matchers.containsInAnyOrder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockServiceImpl;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.AnnotationService;
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
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Test Functional Analysis CSV export Job.
 */
@WithMockUser /* required to be able to submit Jobs */
@TestMethodOrder(OrderAnnotation.class)
class FunctionalAnalysisExporterTest extends DatabaseRelatedTest {

	private static final String RESOURCE_PATH = "/test-resources/innowake/mining/server/job/identification/";
	private static final EntityId projectId = EntityId.of(1L);

	@Autowired
	private FunctionalBlockGenerationService functionalBlockGenerationService;
	@Autowired
	private Tracer tracer;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private MiningJobService miningJobService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private FunctionalBlockService functionalBlockService;
	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private FunctionalBlockServiceImpl functionalBlockServiceImpl;

	@Autowired
	private FunctionalBlockAnalysisCsvExporter functionalBlockAnalysisCsvExporter;

	private static final String ENTITY_NAME = "Annotation";
	private final String customPropertyClassName = ENTITY_NAME + "CustomProperties" + projectId.getNid();

	private String stringRepeaterPropertyName = "";
	private List<String> stringRepeaterProperty = Collections.emptyList();

	private String tagTypePropertyName = "";
	private List<String> tagTypeProperty = Collections.emptyList();

	private String inputStringPropertyName = "";
	private String inputStringProperty = "";

	/**
	 * Reset the test data after every test method.
	 * @throws IOException If an IO related error occurs.
	 */
	@AfterAll
	public void resetData() throws IOException {
		resetTestData();
	}

	@BeforeAll
	void init() {
		final CustomPropertyMetadata propertyMetaData1 = createCustomPropertyMetadata("StringRepeaterType",
				"StringRepeaterType", "Test Annotation Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, ENTITY_NAME, customPropertyClassName);
		stringRepeaterPropertyName = propertyMetaData1.getName();
		stringRepeaterProperty = List.of("red", "blue", "green");

		final String autoCompKey = "AnnotationAutoCompletionKey";
		final CustomPropertyMetadata propertyMetaData = createCustomPropertyMetadata("TagType",
				"TagType", "Test Annotation Custom Property for graph ql test", "EMBEDDEDLIST",
				CustomPropertyFieldType.TAG, 1, false, "AnnotationAutoCompletionKey", projectId, ENTITY_NAME, customPropertyClassName);

		final List<String> tags = Arrays.asList("orange", "grape", "apple", "banana", "kiwi");
		final Map<String, Set<String>> map = new HashMap<>();
		map.put(autoCompKey, new HashSet<>(tags));
		customPropertiesService.putEnumValues(projectId, map);
		assertTrue(customPropertiesService.getEnumValues(projectId, autoCompKey).containsAll(tags), "All entries should match for key: " + autoCompKey);

		tagTypePropertyName = propertyMetaData.getName();
		tagTypeProperty = List.of("grape", "kiwi");

		final CustomPropertyMetadata propertyMetaData3 = createCustomPropertyMetadata("StringType",
				"StringType", "Test Annotation Custom Property for graph ql test", "STRING",
				CustomPropertyFieldType.DEFAULT, 1, false, null, projectId, ENTITY_NAME, customPropertyClassName);

		inputStringPropertyName = propertyMetaData3.getName();
		inputStringProperty = "PL Natural";
	}

	@Test
	@Order(1)
	void testExportOfSingleFunctionalBlockWithDepthLevel1() throws IOException {
		final ModulePojo module = createModule(projectId, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS7101.cbl");
		final var moduleId = module.identity();

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), moduleId);

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(moduleId));

		final List<AnnotationPojo> validationRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).collect(Collectors.toList());

		final List<EntityId> validationRuleIds = validationRules.stream().map(AnnotationPojo::identity).collect(Collectors.toList());

		setCustomProperty(validationRules, stringRepeaterPropertyName, stringRepeaterProperty);

		final Map<Long, UUID> mapOfTechnicalRulesWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(validationRuleIds);
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});

		final UUID groupBlockUid = createFunctionBlock("Functional_block_1", "Functional_block_description",
				new ArrayList<>(mapOfTechnicalRulesWithUUIDS.values()), jsonMap, new ArrayList<>(), module);

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));
		final List<String> exp = List.of(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7101\",\"2\",\"Data Validation Rule Candidate [System identified]\","
						+ "\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\"red,blue,green\",\" \",\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7101\",\"3\",\"Data Validation Rule Candidate [System identified]\","
						+ "\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\"red,blue,green\",\" \",\" \"");
		assertThat(actualJobResult, containsInAnyOrder(exp.toArray()));
		final List<UUID> functionalUnits = new ArrayList<>(mapOfTechnicalRulesWithUUIDS.values());
		functionalUnits.add(groupBlockUid);
		deleteFunctionalBlocks(functionalUnits);
	}

	@Test
	@Order(2)
	void testExportWhenNoRootUidsWerePassed() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());

		final Result<?> result = (Result<?>) jobManager.getJobResult(
				submitJob(jobManager, tracer, new FunctionalBlockAnalysisCsvExporterJob(projectId, parameters, SecurityContextHolder.getContext())));
		assertNotNull("result must not be null", result);
		assertEquals("Functional blocks must exist to download the csv", result.status.getMessage());
	}

	@Test
	@Order(3)
	void testExportOfMoreThanOneFunctionalBlockWithDepthLevel1() throws IOException {
		final ModulePojo module = createModule(projectId, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS7111.cbl");

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), EntityId.of(module.getId()));
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));

		final Map<String, List<AnnotationPojo>> groupedAnnotations = annotations.stream()
				.collect(Collectors.groupingBy(annotation -> annotation.getCategoryName().orElse("")));

		setCustomProperty(groupedAnnotations.get("Technical Rule"), inputStringPropertyName, inputStringProperty);
		setCustomProperty(groupedAnnotations.get("Business Rule"), tagTypePropertyName, tagTypeProperty);

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());

		final List<String> rootUids = new ArrayList<>();
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});
		int count = 1;
		final List<UUID> createdBlocks = new ArrayList<>();
		for (final String ruleType : new String[]{"Business Rule", "Validation Rule", "Technical Rule"}) {
			final List<EntityId> ruleIds = groupedAnnotations.get(ruleType).stream().map(AnnotationPojo::getId).map(EntityId::of).collect(Collectors.toList());
			final Map<Long, UUID> mapOfRulesWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(ruleIds);
			createdBlocks.add(createFunctionBlock("Functional_block_" + count, "Functional_block_" + count + "_description",
					new ArrayList<>(mapOfRulesWithUUIDS.values()), jsonMap, rootUids, module));
			count++;
		}

		final List<String> expectedJobResult = List.of("\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\","
						+ "\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"4\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"5\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"6\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"7\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"8\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"9\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_1_description\",\"MMRS7111\",\"10\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\"grape,kiwi\"",
				"\"2\",\"Functional_block_2\",\"Functional_block_2_description\",\"MMRS7111\",\"14\",\"Data Validation Rule Candidate [System identified]\","
						+ "\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"3\",\"Functional_block_3\",\"Functional_block_3_description\",\"MMRS7111\",\"11\",\"Technical Rule Candidate [System identified]\","
						+ "\"RULE\",\"Technical Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\"PL Natural\",\" \"",
				"\"3\",\"Functional_block_3\",\"Functional_block_3_description\",\"MMRS7111\",\"12\",\"Technical Rule Candidate [System identified]\","
						+ "\"RULE\",\"Technical Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\"PL Natural\",\" \"",
				"\"3\",\"Functional_block_3\",\"Functional_block_3_description\",\"MMRS7111\",\"13\",\"Technical Rule Candidate [System identified]\""
						+ ",\"RULE\",\"Technical Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\"PL Natural\",\" \"");

		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));
		assertThat(actualJobResult, containsInAnyOrder(expectedJobResult.toArray()));
		deleteFunctionalBlocks(createdBlocks);
	}

	@Test
	@Order(4)
	void testExportOfFunctionGroupContainingFunctionalGroupWithDepthLevel2() throws IOException {
		final ModulePojo module = createModule(projectId, "MMRS7102", "MMRS7102.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS7102.cbl");
		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), EntityId.of(module.getId()));
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});

		final List<AnnotationPojo> validationRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).collect(Collectors.toList());
		final Map<Long, UUID> mapOfValidationRulesWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(
				validationRules.stream().map(AnnotationPojo::identity).collect(Collectors.toList()));
		final List<AnnotationPojo> technicalRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Technical Rule")).collect(Collectors.toList());
		final Map<Long, UUID> mapOfTechnicalRulesWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(
				technicalRules.stream().map(AnnotationPojo::identity).collect(Collectors.toList()));
		final List<AnnotationPojo> businessRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Business Rule")).collect(Collectors.toList());
		final Map<Long, UUID> mapOfBusinessRulesWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(
				businessRules.stream().map(AnnotationPojo::identity).collect(Collectors.toList()));
		final UUID validationBlock = createFunctionBlock("Functional_block_Validations", "Functional_block_description1",
				new ArrayList<>(mapOfValidationRulesWithUUIDS.values()), jsonMap, new ArrayList<>(), module);
		final UUID technicalBlock = createFunctionBlock("Functional_block_technicals", "Functional_block_description2",
				new ArrayList<>(mapOfTechnicalRulesWithUUIDS.values()), jsonMap, new ArrayList<>(), module);
		final UUID businessBlock = createFunctionBlock("Functional_block_business", "Functional_block_description3",
				new ArrayList<>(mapOfBusinessRulesWithUUIDS.values()), jsonMap, new ArrayList<>(), module);
		final UUID groupBlockUid1 = createFunctionBlock("Functional_block_group_1", "Functional_block_description4",
				Arrays.asList(validationBlock, technicalBlock), jsonMap, new ArrayList<>(), module);
		final UUID groupBlockUid2 = createFunctionBlock("Functional_block_group_2", "Functional_block_description5",
				Arrays.asList(groupBlockUid1, businessBlock), jsonMap, new ArrayList<>(), module);

		final List<String> uids = Arrays.asList(groupBlockUid2.toString(), groupBlockUid1.toString(), businessBlock.toString(), technicalBlock.toString(),
				validationBlock.toString());

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));
		final List<String> exp = List.of(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\","
						+ "\"Functional Block Sequence (Level 2)\",\"Functional Block Name (Level 2)\",\"Functional Block Description (Level 2)\","
						+ "\"Functional Block Sequence (Level 3)\",\"Functional Block Name (Level 3)\",\"Functional Block Description (Level 3)\","
						+ "\"Module\",\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\","
						+ "\"Last Modified By\",\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_group_2\",\"Functional_block_description5\",\"1.1\",\"Functional_block_group_1\",\"Functional_block_description4\","
						+ "\"1.1.1\",\"Functional_block_Validations\",\"Functional_block_description1\",\"MMRS7102\",\"20\","
						+ "\"Data Validation Rule Candidate [System identified]\",\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_group_2\",\"Functional_block_description5\",\"1.1\",\"Functional_block_group_1\",\"Functional_block_description4\","
						+ "\"1.1.1\",\"Functional_block_Validations\",\"Functional_block_description1\",\"MMRS7102\",\"21\","
						+ "\"Data Validation Rule Candidate [System identified]\",\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_group_2\",\"Functional_block_description5\",\"1.1\",\"Functional_block_group_1\",\"Functional_block_description4\","
						+ "\"1.1.1\",\"Functional_block_Validations\",\"Functional_block_description1\",\"MMRS7102\",\"22\","
						+ "\"Data Validation Rule Candidate [System identified]\",\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \""
		);
		assertEquals(exp, actualJobResult);
		deleteFunctionalBlocks(uids.stream().map(UUID::fromString).collect(Collectors.toList()));
	}

	@Test
	@Order(5)
	void testExportWhenFiltersArePassed() throws IOException {
		final ModulePojo moduleA = createModule(projectId, "EXECSQL", "EXECSQL.cbl", RESOURCE_PATH);
		createModule(projectId, "MMRS71Z1", "MMRS71Z1.cbl", RESOURCE_PATH);

		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "EXECSQL.cbl");

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), EntityId.of(moduleA.getId()));
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(moduleA.identity()));
		final List<DataDictionaryPojo> ddes = dataDictionaryService.find(q -> q.ofModule(EntityId.of(moduleA.getId())));

		final List<EntityId> annotationIds = annotations.stream().map(AnnotationPojo::identity).collect(Collectors.toList());
		final Map<Long, UUID> mapOfAnnotationsWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(annotationIds);

		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});

		final UUID rootUid = createFunctionBlock("Functional_block_1", "Functional_block_description", new ArrayList<>(mapOfAnnotationsWithUUIDS.values()),
				jsonMap, new ArrayList<>(), moduleA);

		final UUID block2Uid = createFunctionBlock("Functional_block_1", "Functional_block_description", Collections.emptyList(), Collections.emptyMap(),
				new ArrayList<>(), moduleA);

		functionalBlockService.setGeneratedFrom(rootUid, new GeneratedFrom(moduleA.getLinkHash(),
				null, null, null, null, null, null));
		functionalBlockServiceImpl.setResolvedModuleParts(rootUid, Collections.singleton(new ResolvedModulePart(EntityId.of(moduleA.getId()))));
		functionalBlockServiceImpl.setReferencedDataDictionaries(rootUid, ddes.stream().map(DataDictionaryPojo::getUid).collect(Collectors.toList()));
		functionalBlockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(EntityId.of(moduleA.getId()))));

		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type = taxonomyService
				.createType(new TaxonomyTypePojoPrototype().setCategoryId(taxonomyCategory).setProject(projectId).setName("NewTaxType"));

		final EntityId taxonomy1 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Taxonomy A").setType(type).setProject(projectId));
		taxonomyService.createModuleLink(moduleA.getUid(), taxonomy1);

		final EntityId taxonomy2 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Taxonomy B").setType(type).setProject(projectId));
		taxonomyService.createModuleLink(moduleA.getUid(), taxonomy2);

		final List<String> ddeNames = ddes.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleIds", Collections.singletonList(moduleA.getId().toString()));
		parameters.put("taxonomyIds", Arrays.asList(taxonomy1.getNid().toString(), taxonomy2.getNid().toString()));
		parameters.put("peerUids", Collections.singletonList(block2Uid.toString()));
		parameters.put("ddeNames", ddeNames);
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());

		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));

		final List<String> exp = List.of(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\","
						+ "\"Module\",\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\","
						+ "\"Last Modified By\",\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"EXECSQL\",\"23\",\"System identified Database Query\","
						+ "\"DATABASE\",\"Declare\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"EXECSQL\",\"24\",\"System identified Database Query\","
						+ "\"DATABASE\",\"Declare\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"");
		assertThat(actualJobResult, containsInAnyOrder(exp.toArray()));
	}

	@Test
	@Order(6)
	void testCsvExportSucceedsWithValidProjectId() {

		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = createFunctionalBlockPojoPrototype("Functional_block_1",
				"Functional_block_description", new ModuleLocation(0, 1000), null, jsonMap, projectId, null);
		final UUID blockUid = functionalBlockService.create(functionalBlockPojoPrototype);

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		parameters.put("rootUids", Arrays.asList(blockUid.toString()));

		final Result<?> result = (Result<?>) jobManager.getJobResult(
				submitJob(jobManager, tracer, new FunctionalBlockAnalysisCsvExporterJob(projectId, parameters, SecurityContextHolder.getContext())));
		assertNotNull(result);
		assertEquals(HttpStatus.OK.name(), result.status.getSeverity().toString());
		assertTrue(result.value instanceof FileSystemResult);
		final FileSystemResult fileSystemResult = (FileSystemResult) result.value;
		assertNotNull(fileSystemResult);
		final String fileName = fileSystemResult.getFileName();
		assertTrue(fileName.endsWith(".csv"));
	}

	@Test
	@Order(7)
	void testExportWhenOnlyModuleFilterIsPassed() throws IOException {
		final ModulePojo module = createModule(projectId, "MMRS71Z1WithoutComp", "MMRS71Z1WithoutComp.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS71Z1WithoutComp.cbl");
		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), module.identity());
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));
		assertEquals(3, annotations.size());
		final List<EntityId> annotationIds = annotations.stream().map(AnnotationPojo::identity).collect(Collectors.toList());
		assertEquals(3, annotationIds.size());
		final Map<Long, UUID> mapOfAnnotationsWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(annotationIds);
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});
		final UUID rootUid = createFunctionBlock("Functional_block_1", "Functional_block_description", new ArrayList<>(mapOfAnnotationsWithUUIDS.values()),
				jsonMap, new ArrayList<>(), module);

		final UUID block2Uid = createFunctionBlock("Functional_block_2", "Functional_block_description2", Collections.emptyList(), jsonMap, new ArrayList<>(),
				module);

		functionalBlockService.setGeneratedFrom(rootUid, new GeneratedFrom(module.getLinkHash(),
				null, null, null, null, null, null));
		functionalBlockServiceImpl.setResolvedModuleParts(rootUid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));
		functionalBlockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleIds", Collections.singletonList(module.getId().toString()));
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));

		final List<String> data = Arrays.asList(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS71Z1WithoutComp\",\"25\","
						+ "\"Field Computation Rule Candidate [System identified]\",\"RULE\",\"Field Computation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \","
						+ "\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS71Z1WithoutComp\",\"26\","
						+ "\"Field Computation Rule Candidate [System identified]\",\"RULE\",\"Field Computation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \","
						+ "\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS71Z1WithoutComp\",\"27\","
						+ "\"Data Validation Rule Candidate [System identified]\",\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"");
		assertThat(actualJobResult, containsInAnyOrder(data.toArray()));
	}

	@Test
	@Order(8)
	void testExportWhenDataDictionariesFilterIsPassed() throws IOException {
		final ModulePojo module = createModule(projectId, "MMRS71B1", "MMRS71B1.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS71B1.cbl");

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), module.identity());
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));
		assertEquals(11, annotations.size());
		final List<EntityId> annotationIds = annotations.stream().map(AnnotationPojo::identity).collect(Collectors.toList());
		final Map<Long, UUID> mapOfAnnotationsWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(annotationIds);


		final List<DataDictionaryPojo> ddes = dataDictionaryService.find(q -> q.ofModule(EntityId.of(module.getId()))).stream()
				.filter(DataDictionaryPojo -> DataDictionaryPojo.getName().equals("MYVSAMK-RESP")).collect(Collectors.toList());
		final List<AnnotationPojo> annotationWithDDE = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity())
				.ofDataDictionaryEntry(EntityId.of(ddes.get(0).getUid())));
		final Map<Long, UUID> mapOfAnnotationForDdeWithUUID = functionalBlockService.findGeneratedFromAnnotations(
				annotationWithDDE.stream().map(AnnotationPojo::identity).collect(Collectors.toList()));
		assertEquals(1, mapOfAnnotationForDdeWithUUID.size());
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});

		final UUID rootUid = createFunctionBlock("Functional_block_1", "Functional_block_description", new ArrayList<>(mapOfAnnotationsWithUUIDS.values()),
				jsonMap, new ArrayList<>(), module);
		final UUID blockWithDde = createFunctionBlock("Functional_block_3", "Functional_block_description3", new ArrayList<>(mapOfAnnotationForDdeWithUUID.values()),
				jsonMap, new ArrayList<>(), module);
		final UUID block2Uid = createFunctionBlock("Functional_block_2", "Functional_block_description2", Collections.emptyList(), jsonMap, new ArrayList<>(),
				module);

		functionalBlockService.setGeneratedFrom(rootUid, new GeneratedFrom(module.getLinkHash(), null,
				null, null, null, null, null));
		functionalBlockServiceImpl.setResolvedModuleParts(rootUid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));
		functionalBlockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));
		functionalBlockServiceImpl.setReferencedDataDictionaries(blockWithDde, ddes.stream().map(DataDictionaryPojo::getUid).collect(Collectors.toList()));
		final List<String> ddeNames = ddes.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("ddeNames", Collections.singletonList(ddeNames.get(0)));
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));

		final List<String> data = Arrays.asList(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_3\",\"Functional_block_description3\",\"MMRS71B1\",\"31\",\"Business Rule Candidate [System identified]\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"");
		assertThat(actualJobResult, containsInAnyOrder(data.toArray()));
	}

	@Test
	@Order(9)
	void testExportWhenTaxonomiesFilterIsPassed() throws IOException {
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withPath(RESOURCE_PATH + "MMRS7101.cbl"))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: " + RESOURCE_PATH + "MMRS7101.cbl"));
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS7101.cbl");

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), module.identity());
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));
		final List<EntityId> annotationIds = annotations.stream().map(AnnotationPojo::identity).collect(Collectors.toList());
		final Map<Long, UUID> mapOfAnnotationsWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(annotationIds);
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});

		final UUID rootUid = createFunctionBlock("Functional_block_1", "Functional_block_description", new ArrayList<>(mapOfAnnotationsWithUUIDS.values()),
				jsonMap, new ArrayList<>(), module);

		final UUID block2Uid = createFunctionBlock("Functional_block_2", "Functional_block_description2", Collections.emptyList(), jsonMap, new ArrayList<>(),
				module);

		functionalBlockService.setGeneratedFrom(rootUid, new GeneratedFrom(module.getLinkHash(),
				null, null, null, null, null, null));
		functionalBlockServiceImpl.setResolvedModuleParts(rootUid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));
		functionalBlockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));

		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type = taxonomyService
				.createType(new TaxonomyTypePojoPrototype().setCategoryId(taxonomyCategory).setProject(projectId).setName("NewTaxType2"));

		final EntityId taxonomy1 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Taxonomy C").setType(type).setProject(projectId));
		taxonomyService.createModuleLink(module.getUid(), taxonomy1);

		final EntityId taxonomy2 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Taxonomy D").setType(type).setProject(projectId));
		taxonomyService.createModuleLink(module.getUid(), taxonomy2);

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		parameters.put("taxonomyIds", Arrays.asList(taxonomy1.getNid().toString(), taxonomy2.getNid().toString()));
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));

		final List<String> data = Arrays.asList(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7101\",\"1\",\"Field Computation Rule Candidate [System identified]\","
						+ "\"RULE\",\"Field Computation Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7101\",\"2\",\"Data Validation Rule Candidate [System identified]\","
						+ "\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\"red,blue,green\",\" \",\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7101\",\"3\",\"Data Validation Rule Candidate [System identified]\","
						+ "\"RULE\",\"Validation Rule\",\"CANDIDATE\",\"SYSTEM\",\"red,blue,green\",\" \",\" \"");
		assertThat(actualJobResult, containsInAnyOrder(data.toArray()));
	}

	@Test
	@Order(10)
	void testExportWhenPeerFilterIsPassed() throws IOException {
		final ModulePojo module = createModule(projectId, "MMRS7112", "MMRS7112.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "MMRS7112.cbl");

		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), module.identity());
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));
		final List<EntityId> annotationIds = annotations.stream().filter(annotation -> annotation.getCategoryName().orElse("").equals("Declare"))
				.map(AnnotationPojo::identity).collect(Collectors.toList());
		assertEquals(3, annotationIds.size());
		final Map<Long, UUID> mapOfAnnotationsWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(annotationIds);
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});

		final UUID rootUid = createFunctionBlock("Functional_block_1", "Functional_block_description", new ArrayList<>(mapOfAnnotationsWithUUIDS.values()),
				jsonMap, new ArrayList<>(), module);

		final UUID block2Uid = createFunctionBlock("Functional_block_2", "Functional_block_description2", Collections.emptyList(), jsonMap, new ArrayList<>(),
				module);

		functionalBlockService.setGeneratedFrom(rootUid, new GeneratedFrom(module.getLinkHash(),
				null, null, null, null, null, null));
		functionalBlockServiceImpl.setResolvedModuleParts(rootUid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));
		functionalBlockServiceImpl.setResolvedModuleParts(block2Uid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		parameters.put("peerUids", Collections.singletonList(block2Uid.toString()));
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));

		final List<String> data = Arrays.asList(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7112\",\"39\",\"System identified Database Query\",\"DATABASE\","
						+ "\"Declare\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7112\",\"40\",\"System identified Database Query\",\"DATABASE\","
						+ "\"Declare\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_1\",\"Functional_block_description\",\"MMRS7112\",\"41\",\"System identified Database Query\",\"DATABASE\","
						+ "\"Declare\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"");
		assertThat(actualJobResult, containsInAnyOrder(data.toArray()));
	}

	@Test
	@Order(11)
	void testExportCheckAnnotationModifiedShouldHaveName() throws IOException {
		final ModulePojo module = createModule(projectId, "EXTINPUT", "EXTINPUT.cbl", RESOURCE_PATH);
		submitIdentifyCandidatesJob(projectId, RESOURCE_PATH + "EXTINPUT.cbl");
		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), module.identity());
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(module.identity()));
		final List<AnnotationPojo> businessRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Business Rule")).collect(Collectors.toList());
		assertEquals(6, businessRules.size());
		final List<EntityId> bussinessRulesIds = businessRules.stream().map(AnnotationPojo::identity).collect(Collectors.toList());
		final Map<Long, UUID> mapOfBusinessRulesWithUUIDS = functionalBlockService.findGeneratedFromAnnotations(bussinessRulesIds);
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] {
				"FUNCTIONAL_GROUP"
		});
		final UUID rootUid = createFunctionBlock("Functional_block_business", "Functional_block_description",
				new ArrayList<>(mapOfBusinessRulesWithUUIDS.values()), jsonMap, new ArrayList<>(), module);

		functionalBlockService.setGeneratedFrom(rootUid, new GeneratedFrom(module.getLinkHash(),
				null, null, null, null, null, null));
		functionalBlockServiceImpl.setResolvedModuleParts(rootUid, Collections.singleton(new ResolvedModulePart(EntityId.of(module.getId()))));

		//Now update the description new text ( updated id = annotation id) of the annotation after sorting businessRules by id
		businessRules.sort(Comparator.comparing(AnnotationPojo::getId));
		businessRules.forEach(annotation -> {
			annotationService.update(new AnnotationPojoPrototype()
					.withId(annotation.identity())
					.setName("Updated Desc of " + annotation.getId().toString()));});

		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleIds", Collections.singletonList(module.getId().toString()));
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", new ArrayList<>());
		final List<String> actualJobResult = convertJobResultToString(getJobResult(projectId, parameters));

		final List<String> exp = List.of(
				"\"Functional Block Sequence (Level 1)\",\"Functional Block Name (Level 1)\",\"Functional Block Description (Level 1)\",\"Module\","
						+ "\"Annotation ID\",\"Annotation Description\",\"Annotation Type\",\"Annotation Category\",\"Annotation State\",\"Last Modified By\","
						+ "\"StringRepeaterType\",\"StringType\",\"TagType\"",
				"\"1\",\"Functional_block_business\",\"Functional_block_description\",\"EXTINPUT\",\"49\",\"Updated Desc of 49\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_business\",\"Functional_block_description\",\"EXTINPUT\",\"50\",\"Updated Desc of 50\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_business\",\"Functional_block_description\",\"EXTINPUT\",\"51\",\"Updated Desc of 51\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_business\",\"Functional_block_description\",\"EXTINPUT\",\"52\",\"Updated Desc of 52\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_business\",\"Functional_block_description\",\"EXTINPUT\",\"53\",\"Updated Desc of 53\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \"",
				"\"1\",\"Functional_block_business\",\"Functional_block_description\",\"EXTINPUT\",\"54\",\"Updated Desc of 54\","
						+ "\"RULE\",\"Business Rule\",\"CANDIDATE\",\"SYSTEM\",\" \",\" \",\" \""
		);
		assertThat(actualJobResult, containsInAnyOrder(exp.toArray()));
		final List<UUID> functionalUnits = new ArrayList<>(mapOfBusinessRulesWithUUIDS.values());
		functionalUnits.add(rootUid);
		deleteFunctionalBlocks(functionalUnits);
	}

	private UUID createFunctionBlock(final String name, final String desc, final List<UUID> childs, final Map<String, Object> jsonMap,
			final List<String> rootUids, final ModulePojo module) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype2 = createFunctionalBlockPojoPrototype(name, desc, new ModuleLocation(0, 2000), childs,
				jsonMap, projectId, module);
		final UUID createdFG = functionalBlockService.create(functionalBlockPojoPrototype2);
		rootUids.add(createdFG.toString());
		return createdFG;
	}

	protected void submitIdentifyCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, tracer, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}

	private JobResult getJobResult(final EntityId projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(UUID.fromString(submitJob(jobManager, tracer, functionalBlockAnalysisCsvExporter.createJob(projectId, parameters))))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}

	private ModulePojo createModule(final EntityId projectId, final String name, final String file, final String dir) {
		final String content = getContent(file, dir);
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setPath(dir + file);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.getModule(moduleService.create(module));
	}

	private String getContent(final String file, final String dir) {
		final Path path = Paths.get(System.getProperty("user.dir"), dir, file);
		try {
			return new String(Files.readAllBytes(path), Charset.forName("Cp1252"));
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException("Exception occured while file reading", e);
		}
	}

	private static List<String> convertJobResultToString(final JobResult result) throws IOException {
		final BufferedReader reader = new BufferedReader(new InputStreamReader(result.getContent(), StandardCharsets.UTF_8));
		final List<String> list = new ArrayList<>();
		String line;
		while ((line = reader.readLine()) != null) {
			list.add(line);
		}

		return list;
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, @Nullable final ModuleLocation moduleLocation,
			final @Nullable List<UUID> childs, final @Nullable Map<String, Object> flags, final EntityId projectID, @Nullable final ModulePojo module) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(projectID);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		if (module != null) {
			final List<ModulePart> moduleParts = new ArrayList<>();
			final ModulePart functionalBlockModulePart = new ModulePart(module.getLinkHash(), moduleLocation);
			moduleParts.add(functionalBlockModulePart);
			functionalBlockPojoPrototype.setModuleParts(moduleParts);
		}
		if (childs != null) {
			functionalBlockPojoPrototype.setChildren(childs);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		return functionalBlockPojoPrototype;
	}

	private CustomPropertyMetadata createCustomPropertyMetadata(final String name, final String label, final String description, final String dataType,
			final CustomPropertyFieldType customPropertyFieldType, final int customViewIndex, final boolean pluginVisible, @Nullable final String autoCompKey,
			final EntityId projectId, final String entityName, final String className) {
		final CustomPropertyMetadata customPropertyMetadata = new CustomPropertyMetadata();
		customPropertyMetadata.setName(name);
		customPropertyMetadata.setLabel(label);
		customPropertyMetadata.setDescription(description);
		customPropertyMetadata.setDataType(dataType);
		customPropertyMetadata.setFieldType(customPropertyFieldType);
		customPropertyMetadata.setCustomViewIndex(customViewIndex);
		customPropertyMetadata.setPluginVisible(pluginVisible);
		customPropertyMetadata.setAutoCompletionKey(autoCompKey);
		customPropertiesService.defineProperty(projectId, entityName, customPropertyMetadata.getName(), customPropertyMetadata);
		
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.ofParent(null).withName(className)), className + " should be existing in DB");
		assertEquals(1L, customPropertiesService.countPropertyDefinitions(q -> q.withParent(className).withName(customPropertyMetadata.getName())),
				customPropertyMetadata.getName() + " should exist in " + className);
		
		return customPropertyMetadata;
	}

	private void setCustomProperty(final List<AnnotationPojo> annotations, final String propertyName, final Object propertyValue) {
		for (final AnnotationPojo annotation : annotations) {
			final Map<String, Object> cps = new HashMap<>(annotation.getCustomProperties());
			@SuppressWarnings("unchecked")
			final Map<String, Object> aCpst = (Map<String, Object>) cps.computeIfAbsent(customPropertyClassName, k -> new HashMap<>());
			aCpst.put(propertyName, propertyValue);
			annotationService.update(new AnnotationPojoPrototype().withId(annotation.identity()).setCustomProperties(cps));
		}
	}

	private void deleteFunctionalBlocks(final List<UUID> functionalBlocks) {
		for (final UUID uid : functionalBlocks) {
			functionalBlockService.delete(uid);
		}
	}
}
