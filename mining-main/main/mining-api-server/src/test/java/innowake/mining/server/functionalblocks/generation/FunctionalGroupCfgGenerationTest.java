/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock.DataLineageFunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
import innowake.mining.server.functionalblocks.service.AnnotationToFunctionalBlockService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockToControlFlowGraphService;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.service.UserRoleService;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.BDDMockito.given;

/**
 * Tests for the generation of Control flow graph for functional groups.
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false)
@WithMockUser
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class FunctionalGroupCfgGenerationTest extends AbstractIdentificationTest {

	@Autowired
	private AnnotationToFunctionalBlockService annotationToFunctionalBlockService;

	@Autowired
	private FunctionalBlockService functionalBlockService;

	@Autowired
	private FunctionalBlockGenerationService functionalBlockGenerationService;

	@Autowired
	private FunctionalBlockToControlFlowGraphService functionalBlockToControlFlowGraphService;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private JobManager jobManager;

	@Nullable
	@MockBean
	private UserRoleService userRoleService;

	private ModulePojo moduleA;
	private ModulePojo moduleB;
	private ModulePojo moduleC;

	@BeforeAll
	void init() {
		/* Set up project accesses for the specified project IDs */
		setupProjectAccesses(Arrays.asList(PROJECT_ID_1.getNid(), PROJECT_ID_2.getNid()), Arrays.asList(PROJECT_ID_1.getNid(), PROJECT_ID_2.getNid()));

		/* Create modules MMRS7101, CBACT04C, and MMRS7102 with the specified files */
		moduleA = createModule("MMRS7101", "MMRS7101.cbl");
		moduleB = createModule("CBACT04C", "CBACT04C.cbl");
		moduleC = createModule("MMRS7102", "MMRS7102.cbl");

		/* Submit jobs to identify candidates for the created modules */
		submitIdentifyCandidatesJob(RESOURCE_PATH + "MMRS7101.cbl");
		submitIdentifyCandidatesJob(RESOURCE_PATH + "MMRS7102.cbl");
	}

	/**
	 * Test Control Flow Graph generation for manual functional block.
	 */
	@Test
	@Order(1)
	void testManualFbCfgGeneration() {
		/* Retrieve all annotations for the given module in the specified project and filter to get only those categorized as "Validation Rule" */
		final List<AnnotationPojo> validationRuleAnnotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleA.identity())).stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).toList();

		/* Assert that there are exactly two validation rules and extract their IDs */
		assertEquals(2, validationRuleAnnotations.size(),
				"Expected exactly two validation rules to be present, but found " + validationRuleAnnotations.size());
		final List<EntityId> validationRuleIds = validationRuleAnnotations.stream().map(AnnotationPojo :: identity).toList();
		assertEquals(2, validationRuleIds.size(), "Expected exactly two validation rule IDs to be present, but found " + validationRuleIds.size());

		/* Get functional units for the given validation rule annotations and assert that two functional units are created */
		final Map<Long, UUID> validationRuleFunctionalUnits = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1,
				validationRuleIds);
		assertEquals(2, validationRuleFunctionalUnits.size(),
				"Expected exactly two functional units to be created, but found " + validationRuleFunctionalUnits.size());

		/* Collect the UUIDs of the functional units and prepare a map for the functional block flags */
		final List<UUID> validationRuleFunctionalUnitIds = new ArrayList<>(validationRuleFunctionalUnits.values());
		final Map<String, Object> functionalBlockFlags = new HashMap<>();
		functionalBlockFlags.put("TYPE", new String[] { "FUNCTIONAL_GROUP" });

		/* Create a prototype for the functional block and create the functional block in the service */
		final FunctionalBlockPojoPrototype functionalBlockPrototype = createFunctionalBlockPojoPrototype("ManualFb", "Manual functional block",
				validationRuleFunctionalUnitIds, moduleA, functionalBlockFlags);
		final UUID createdFunctionalBlockId = functionalBlockService.create(functionalBlockPrototype);
		assertNotNull(createdFunctionalBlockId, "Functional block creation failed, expected a valid UUID but got null");

		/* Submit the functional block for computation and retrieve the created functional block from the service */
		submitFunctionalBlockComputation(new HashSet<>(Collections.singleton(createdFunctionalBlockId)));
		final List<FunctionalBlockPojo> createdFunctionalBlocks = functionalBlockService.find(q -> q.ofProject(PROJECT_ID_1).byUid(createdFunctionalBlockId));
		assertEquals(1, createdFunctionalBlocks.size(), "Expected exactly one functional block to be created, but found " + createdFunctionalBlocks.size());

		/* Assert that the functional block has CFG after computation */
		assertValidControlFlowGraph(createdFunctionalBlocks.get(0));
	}

	/**
	 * Test CFG generation for structural functional block.
	 */
	@Test
	@Order(2)
	void testStructuralFbCfgGeneration() {
		/* Submit a job to identify candidates for the given module in the specified project and generate functional blocks for the module */
		submitJob(jobManager,
				new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Collections.singletonList(moduleA.identity()), Collections.emptyList())));
		functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1), moduleA.identity());

		/* Generate structural functional blocks for the module and collect the response */
		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generationResponse = functionalBlockGenerationService.generate(
				StructuralFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1), moduleA.identity());
		assertFalse(generationResponse.isEmpty(), "Expected creation of structural block, but no structural blocks were created");

		/* Retrieve the created structural blocks related to the module and assert that structural blocks are found */
		final List<FunctionalBlockPojo> structuralBlocks = functionalBlockService.find(
				q -> q.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_GROUP)
						.withFlag(FunctionalBlockFlag.GENERATED_BY, "StructuralFunctionalBlockGeneration"));
		assertFalse(structuralBlocks.isEmpty(), "Expected created structural blocks related to the module, but none were found");

		/* Collect the names of the structural blocks and assert that the structural blocks have the expected names */
		final List<String> structuralBlockNames = structuralBlocks.stream().map(FunctionalBlockPojo :: getName).toList();
		assertTrue(structuralBlockNames.containsAll(Arrays.asList("MMRS7101", "TRUNCTEST")),
				"Expected that structural blocks are created for the module with the names MMRS7101 and TRUNCTEST, but they were not found");

		/* Retrieve the functional block for TRUNCTEST and assert that it is created */
		final List<FunctionalBlockPojo> trunctTestFunctionalBlock = functionalBlockService.find(
				q -> q.ofProject(PROJECT_ID_1).withName("TRUNCTEST").withType(FunctionalBlockType.FUNCTIONAL_GROUP));
		assertEquals(1, trunctTestFunctionalBlock.size(), "Functional block for TRUNCTEST should be created, but it was not found");

		/* Submit the functional block for computation and retrieve the functional block for MMRS7101 */
		submitFunctionalBlockComputation(new HashSet<>(Collections.singleton(trunctTestFunctionalBlock.get(0).getUid())));
		final List<FunctionalBlockPojo> mmrs7101FunctionalBlock = functionalBlockService.find(
				q -> q.ofProject(PROJECT_ID_1).withName("MMRS7101").withType(FunctionalBlockType.FUNCTIONAL_GROUP));
		assertEquals(1, mmrs7101FunctionalBlock.size(), "Functional block for MMRS7101 should be created, but it was not found");

		/* Submit the functional block for computation and retrieve the functional block for TRUNCTEST after computation */
		submitFunctionalBlockComputation(new HashSet<>(Collections.singleton(mmrs7101FunctionalBlock.get(0).getUid())));
		final List<FunctionalBlockPojo> trunctTestFunctionalBlockAfterComputation = functionalBlockService.find(
				q -> q.ofProject(PROJECT_ID_1).byUid(trunctTestFunctionalBlock.get(0).getUid()));
		assertValidControlFlowGraph(trunctTestFunctionalBlockAfterComputation.get(0));

		/* Retrieve the functional block for MMRS7101 after computation and assert that it has CFG */
		final List<FunctionalBlockPojo> mmrs7101FunctionalBlockAfterComputation = functionalBlockService.find(
				q -> q.ofProject(PROJECT_ID_1).byUid(mmrs7101FunctionalBlock.get(0).getUid()));
		assertValidControlFlowGraph(mmrs7101FunctionalBlockAfterComputation.get(0));
	}

	/**
	 * Test CFG generation for functional block generated by data lineage.
	 */
	@Test
	@Order(3)
	void testDataLineageFbCfgGeneration() {
		/* Create and save a data dictionary entry with specified attributes */
		final DataDictionaryPojo accountRecordDataDictionary = createVariablesDataDictionaryEntry("ACCOUNT-RECORD", moduleB.identity(), "Group", 6602, 14);

		/* Generationerate functional blocks using DataLineageFunctionalBlockGeneration and assert the generation result */
		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generatedFunctionalBlocks =
				functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(accountRecordDataDictionary.getId())));
		assertNotNull(generatedFunctionalBlocks, "Expected non-null generation result, but it was null");
		assertEquals(45, generatedFunctionalBlocks.size(), "Expected 45 functional blocks to be generated, but found " + generatedFunctionalBlocks.size());

		/* Retrieve and assert the functional block for ACCOUNT-RECORD in CBACT04C */
		final List<FunctionalBlockPojo> accountRecordFunctionalBlocks = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCOUNT-RECORD in CBACT04C"));
		assertEquals(1, accountRecordFunctionalBlocks.size(),
				"Expected exactly one functional block for ACCOUNT-RECORD in CBACT04C, but found " + accountRecordFunctionalBlocks.size());
		assertValidControlFlowGraph(accountRecordFunctionalBlocks.get(0));
	}

	/**
	 * Test CFG generation for manual functional block with annotation from different modules.
	 */
	@Test
	@Order(4)
	void testManualFbAnnotationDiffModules() {
		/* Retrieve annotations and functional units for modules MMRS7101 and MMRS7102, and assert their counts */
		final List<AnnotationPojo> annotationsOfModuleA = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleA.identity()));
		final List<EntityId> annotationIdsOfModuleA = annotationsOfModuleA.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> functionalUnitsOfModuleA = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1,
				annotationIdsOfModuleA);
		assertEquals(3, functionalUnitsOfModuleA.size(),
				"Expected exactly three functional units to be created for module MMRS7101, but found " + functionalUnitsOfModuleA.size());

		final List<AnnotationPojo> annotationsOfModuleC = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleC.identity()));
		final List<EntityId> annotationIdsOfModuleC = annotationsOfModuleC.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> functionalUnitsOfModuleC = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1,
				annotationIdsOfModuleC);
		assertEquals(8, functionalUnitsOfModuleC.size(),
				"Expected exactly eight functional units to be created for module MMRS7102, but found " + functionalUnitsOfModuleC.size());

		/* Create a functional block prototype with combined functional units and assert its creation */
		final List<UUID> combinedFunctionalUnitIds = new ArrayList<>(functionalUnitsOfModuleA.values());
		combinedFunctionalUnitIds.addAll(functionalUnitsOfModuleC.values());
		final Map<String, Object> functionalBlockFlags = new HashMap<>();
		functionalBlockFlags.put("TYPE", new String[] { "FUNCTIONAL_GROUP" });
		final FunctionalBlockPojoPrototype functionalBlockPrototype = createFunctionalBlockPojoPrototype("ManualFbAnnotationDiffModules",
						"Manual functional block with annotation from different modules", combinedFunctionalUnitIds, moduleA,
				functionalBlockFlags);
		final UUID createdFunctionalBlockId = functionalBlockService.create(functionalBlockPrototype);
		assertNotNull(createdFunctionalBlockId, "Functional block creation failed for the combined modules, expected a valid UUID but got null");

		/* Submit the functional block for computation and assert the result */
		submitFunctionalBlockComputation(new HashSet<>(Collections.singleton(createdFunctionalBlockId)));
		final List<FunctionalBlockPojo> createdFunctionalBlocks = functionalBlockService.find(q -> q.ofProject(PROJECT_ID_1).byUid(createdFunctionalBlockId));
		assertEquals(1, createdFunctionalBlocks.size(),
				"Expected exactly one functional block to be created for the combined modules, but found " + createdFunctionalBlocks.size());
		assertFalse(Boolean.parseBoolean(createdFunctionalBlocks.get(0).getFlags().get(FunctionalBlockFlag.HAS_CFG.toString()).toString()),
				"After computation, the manual functional block for the combined modules should not have CFG, but it does.");
	}

	/**
	 * Test CFG generation when annotation of another module is added to functional block generated by data lineage.
	 */
	@Test
	@Order(5)
	void testAnnotationAddedToFb() {
		/* Create and save data dictionary entry for COB-MIN with specified attributes */
		final DataDictionaryPojo cobMinDataDictionary = createVariablesDataDictionaryEntry("COB-MIN", moduleB.identity(), "PICX", 10129, 7);
		assertNotNull(cobMinDataDictionary, "Data dictionary entry for COB-MIN should not be null");
		assertEquals("COB-MIN", cobMinDataDictionary.getName(),
				"Expected data dictionary entry name to be 'COB-MIN', but found " + cobMinDataDictionary.getName());

		/* Generate functional blocks using DataLineageFunctionalBlockGeneration and assert the generation result */
		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generatedFunctionalBlocks =
				functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
							List.of(EntityId.of(cobMinDataDictionary.getId())));
		assertNotNull(generatedFunctionalBlocks, "Expected non-null generation result, but it was null");

		/* Retrieve and assert the functional blocks for COB-MIN in CBACT04C */
		final List<FunctionalBlockPojo> cobMinFunctionalBlocks = functionalBlockService.find(b -> b.ofProject(PROJECT_ID_1)
				.withName("Functional blocks for COB-MIN in CBACT04C"));
		assertEquals(1, cobMinFunctionalBlocks.size(),
				"Expected exactly one functional block for COB-MIN in CBACT04C, but found " + cobMinFunctionalBlocks.size());
		assertValidControlFlowGraph(cobMinFunctionalBlocks.get(0));

		/* Retrieve functional units for "Validation Rule" annotations from moduleA and assert their count */
		final Map<Long, UUID> validationRuleFunctionalUnits = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1,
				annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleA.identity())).stream()
						.filter(annotation -> "Validation Rule".equals(annotation.getCategoryName().orElse(""))).map(AnnotationPojo :: identity).toList());
		assertEquals(2, validationRuleFunctionalUnits.size(),
				"Expected exactly two functional units to be created, but found " + validationRuleFunctionalUnits.size());

		/* Collect the UUIDs of the functional units and prepare a prototype for the functional block */
		final List<UUID> combinedFunctionalUnitIds = functionalBlockService.find(b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT)
								.withParent(p -> p.byUid(cobMinFunctionalBlocks.get(0).getUid())))
									.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList());
		combinedFunctionalUnitIds.addAll(validationRuleFunctionalUnits.values());

		final FunctionalBlockPojoPrototype functionalBlockPrototype = new FunctionalBlockPojoPrototype().setUid(cobMinFunctionalBlocks.get(0).getUid())
				.setChildren(combinedFunctionalUnitIds);
		functionalBlockService.deleteLinks(Assert.assertNotNull(functionalBlockPrototype.uid.get()));
		functionalBlockService.deleteConditionsAndStatementTypeChildren(Assert.assertNotNull(functionalBlockPrototype.uid.get()));
		functionalBlockService.update(functionalBlockPrototype);

		/* Submit the functional block for computation and assert the result */
		submitJob(jobManager, new FunctionalBlockComputationJob(Set.of(cobMinFunctionalBlocks.get(0).getUid())));

		final List<UUID> updatedFunctionalUnitIds = functionalBlockService.find(b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT)
								.withParent(p -> p.byUid(cobMinFunctionalBlocks.get(0).getUid())))
									.stream().map(FunctionalBlockPojo :: getUid).toList();
		assertEquals(combinedFunctionalUnitIds.size(), updatedFunctionalUnitIds.size(),
				"Expected updated children list size to match the combined functional unit IDs size, but found " + updatedFunctionalUnitIds.size());

		final List<FunctionalBlockPojo> cobMinFunctionalBlocksAfterComputation = functionalBlockService.find(b -> b.ofProject(PROJECT_ID_1)
				.byUid(cobMinFunctionalBlocks.get(0).getUid()));
		assertFalse(Boolean.parseBoolean(cobMinFunctionalBlocksAfterComputation.get(0).getFlags().get(FunctionalBlockFlag.HAS_CFG.toString()).toString()),
				"After computation, the manual functional block for the combined modules should not have CFG, but it does.");
	}

	/**
	 * Asserts that the given functional block has a valid Control Flow Graph (CFG) after computation.
	 *
	 * @param functionalBlock the functional block to be checked
	 */
	private void assertValidControlFlowGraph(final FunctionalBlockPojo functionalBlock) {
		assertTrue(Boolean.parseBoolean(functionalBlock.getFlags().get(FunctionalBlockFlag.HAS_CFG.toString()).toString()),
				"Expected the functional block to have a Control Flow Graph (CFG) after computation, but it does not.");
		final ControlFlowGraph controlFlowGraph = functionalBlockToControlFlowGraphService.toControlFlowGraph(functionalBlock);
		assertNotNull(controlFlowGraph, "Control Flow Graph (CFG) should not be null after computation, but it is.");
		assertFalse(controlFlowGraph.nodes.isEmpty(),
				"Nodes should not be empty when a Control Flow Graph (CFG) is available for the functional block, but they are.");
		assertFalse(controlFlowGraph.edges.isEmpty(),
				"Edges should not be empty when a Control Flow Graph (CFG) is available for the functional block, but they are.");
	}

	/**
	 * Creates a prototype for a functional block.
	 *
	 * @param name the name of the functional block
	 * @param desc the description of the functional block
	 * @param children the list of child UUIDs
	 * @param module the module associated with the functional block
	 * @param flags the flags for the functional block
	 * @return the created FunctionalBlockPojoPrototype
	 */
	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, final List<UUID> children,
			final ModulePojo module, final Map<String, Object> flags) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(AbstractIdentificationTest.PROJECT_ID_1);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		functionalBlockPojoPrototype.setFlags(flags);

		final ModulePart functionalBlockModulePart = new ModulePart(module.getLinkHash(), null);
		final List<ModulePart> moduleParts = List.of(functionalBlockModulePart);
		functionalBlockPojoPrototype.setModuleParts(moduleParts);

		if ( children != null ) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		return functionalBlockPojoPrototype;
	}

	/**
	 * Creates a module with the specified name and file.
	 *
	 * @param name the name of the module
	 * @param file the file associated with the module
	 * @return the created ModulePojo
	 */
	private ModulePojo createModule(final String name, final String file) {
		final String content = getContent(file, AbstractIdentificationTest.RESOURCE_PATH);
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(AbstractIdentificationTest.PROJECT_ID_1);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setPath(AbstractIdentificationTest.RESOURCE_PATH + file);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.getModule(moduleService.create(module));
	}

	/**
	 * Creates a data dictionary entry with the specified attributes.
	 * @param dataElementName the name of the data element
	 * @param moduleId the ID of the module
	 * @param format the format of the data element
	 * @param locationOffset the location offset of the data element
	 * @param locationLength the location length of the data element
	 * @return the created DataDictionaryPojo
	 */
	private DataDictionaryPojo createVariablesDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final String format,
			final int locationOffset, final int locationLength) {

		final DataDictionaryPojoPrototype pojoPrototype = new DataDictionaryPojoPrototype().setName(dataElementName).setModule(moduleId)
				.setLocation(new ModuleLocation(locationOffset, locationLength)).setDescription("MY description").setFormat(format).setLength(1L)
				.setCreatedByUserId("admin").setPicClause("TEST").setDefinedLocation(DefinedLocation.PROGRAM).setIsBusiness(true).setFieldLevel(1L)
				.setState(WorkingState.IN_ANALYSIS).setUsage("DISPLAY").setFieldTransformation("TEST TRANSFORMATION").setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT");

		return dataDictionaryService.create(pojoPrototype);
	}

	/**
	 * Submits a job to identify candidates for the specified path.
	 *
	 * @param path the path for which to identify candidates
	 */
	private void submitIdentifyCandidatesJob(final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(AbstractIdentificationTest.PROJECT_ID_1, new ModuleMatcher(Collections.emptyList(), List.of(path))));
	}

	/**
	 * Submits a functional block computation job for the specified UUIDs.
	 *
	 * @param uuids the set of UUIDs for which to submit the computation job
	 */
	private void submitFunctionalBlockComputation(final Set<UUID> uuids) {
		submitJob(jobManager, new FunctionalBlockComputationJob(uuids));
	}

	/**
	 * Sets up project accesses for the specified project IDs.
	 *
	 * @param authProjectIds the list of project IDs for authentication
	 * @param userRoleProjectIds the list of project IDs for user roles
	 */
	private void setupProjectAccesses(final List<Long> authProjectIds, final List<Long> userRoleProjectIds) {
		final UserRoleService userRoleService = Assert.assertNotNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(userRoleProjectIds);

		final List<SimpleGrantedAuthority> authorities = authProjectIds.stream().flatMap(
				projectId -> Stream.of(new SimpleGrantedAuthority(String.format("client-1-project-%d-viewer", projectId)),
						new SimpleGrantedAuthority(String.format("client-1-project-%d-mining", projectId)))).toList();

		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}
}

