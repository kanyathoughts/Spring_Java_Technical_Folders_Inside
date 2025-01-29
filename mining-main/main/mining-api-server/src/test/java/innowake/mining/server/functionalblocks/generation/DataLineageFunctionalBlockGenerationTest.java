/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import brave.Tracer;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock.DataLineageFunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
import innowake.mining.server.functionalblocks.job.datalineagefunctionalblock.DataLineageFunctionalBlockJob;
import innowake.mining.server.functionalblocks.service.AnnotationToFunctionalBlockService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockToControlFlowGraphService;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.functionalblocks.ExcludedBranch;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link DataLineageFunctionalBlockGeneration}.
 */

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@WithMockUser
class DataLineageFunctionalBlockGenerationTest extends AbstractIdentificationTest {

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private FunctionalBlockService functionalBlockService;

	@Autowired
	private AnnotationToFunctionalBlockService annotationToFunctionalBlockService;

	@Autowired
	private FunctionalBlockToControlFlowGraphService functionalBlockToControlFlowGraphService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	@BeforeAll
	void init() {
		createModule(PROJECT_ID_1, "CBACT04C", "CBACT04C.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "PROG1030", "PROG1030.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "MMRS71Z1", "MMRS71Z1.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		final EntityId autoFbModule = createModule(PROJECT_ID_1, "AUTOFB", "AUTOFB.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		final EntityId autoFbCpy = createModule(PROJECT_ID_1, "AUTOFBCP", "AUTOFBCP.cpy", RESOURCE_PATH, Technology.COBOL, Type.COPYBOOK);
		createReference(RelationshipType.INCLUDES, autoFbModule, autoFbCpy);
	}

	@Test
	@Order(1)
	void testGeneratedFbForBusinessVariables() {
		final EntityId moduleId = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ACCOUNT-RECORD", moduleId, "Group", 6602, 14, true,
				DefinedLocation.PROGRAM);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertNotNull(generated);
		assertEquals(45, generated.size());

		final Map<UUID, FunctionalBlockGenerationResult.Operation> generatedUUIDandOperationMap = generated.stream()
				.collect(Collectors.toMap(Pair :: getRight, Pair :: getLeft));

		/* fetch functional group name ACCOUNT-RECORD Computation */

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCOUNT-RECORD in CBACT04C"));
		assertEquals(1, functionalBlockPojo.size());
		assertEquals("Functional blocks for ACCOUNT-RECORD in CBACT04C", functionalBlockPojo.get(0).getName());
		assertEquals(60, functionalBlockPojo.get(0).getChildren().size());


		/* check if the uuid of functionalBlockPojo.get(0) is present and created by generate method */

		Assert.assertTrue(generatedUUIDandOperationMap.containsKey(functionalBlockPojo.get(0).getUid()));
		assertEquals(FunctionalBlockGenerationResult.Operation.CREATE, generatedUUIDandOperationMap.get(functionalBlockPojo.get(0).getUid()));


		/* find the child of functional group name ACCOUNT-RECORD Computation with type Functional_condition and check if it is generated successfully or not*/

		final List<FunctionalBlockPojo> functionalBlockTypeFunctionalConditionChild = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_CONDITION)
						.withParent(p -> p.byUid(functionalBlockPojo.get(0).getUid())));
		assertEquals(28, functionalBlockTypeFunctionalConditionChild.size());
		Assert.assertTrue(generatedUUIDandOperationMap.keySet()
				.containsAll(functionalBlockTypeFunctionalConditionChild.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList())));


		/* find the child of functional group name ACCOUNT-RECORD Computation  with type Functional_unit and check if it is generated successfully or not */

		final List<FunctionalBlockPojo> functionalBlockTypeFunctionalUnitChild = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT).withParent(p -> p.byUid(functionalBlockPojo.get(0).getUid())));
		assertEquals(16, functionalBlockTypeFunctionalUnitChild.size());

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofModule(moduleId));
		assertEquals(16, annotations.size());
		final Map<Long, UUID> annotationToFunctionalUnit = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1,
				annotations.stream().map(AnnotationPojo :: getId).map(EntityId :: of).toList());
		assertEquals(16, annotationToFunctionalUnit.size());
		assertTrue(annotationToFunctionalUnit.values()
				.containsAll(functionalBlockTypeFunctionalUnitChild.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList())));

	}

	@Test
	@Order(2)
	void testGeneratedFbForVariablesFromTwoModules() {
		final EntityId moduleIdA = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojoA = createVariablesDataDictionaryEntry("ACCT-ACTIVE-STATUS", moduleIdA, "PICX", 6693, 18, true,
				DefinedLocation.PROGRAM);

		final EntityId moduleIdB = getModule("MMRS7101.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojoB = createVariablesDataDictionaryEntry("MY-BIN-FIELDS", moduleIdB, "Group", 1250, 13, false,
				DefinedLocation.PROGRAM);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojoA.getId()), EntityId.of(dataDictionaryPojoB.getId())));
		assertNotNull(generated);
		assertEquals(41, generated.size());

		final Map<UUID, FunctionalBlockGenerationResult.Operation> generatedUUIDandOperationMap = generated.stream()
				.collect(Collectors.toMap(Pair :: getRight, Pair :: getLeft));

		/* fetch functional group name ACCT-ACTIVE-STATUS and MY-BIN-FIELDS */
		final List<FunctionalBlockPojo> functionalBlockPojoA = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for MY-BIN-FIELDS in MMRS7101"));
		assertEquals(1, functionalBlockPojoA.size());
		assertEquals("Functional blocks for MY-BIN-FIELDS in MMRS7101", functionalBlockPojoA.get(0).getName());
		assertEquals(14, functionalBlockPojoA.get(0).getChildren().size());
		Assert.assertTrue(generatedUUIDandOperationMap.containsKey(functionalBlockPojoA.get(0).getUid()));
		assertEquals(FunctionalBlockGenerationResult.Operation.CREATE, generatedUUIDandOperationMap.get(functionalBlockPojoA.get(0).getUid()));

		final List<FunctionalBlockPojo> functionalBlockPojoB = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-ACTIVE-STATUS in CBACT04C"));
		assertEquals(1, functionalBlockPojoB.size());
		assertEquals("Functional blocks for ACCT-ACTIVE-STATUS in CBACT04C", functionalBlockPojoB.get(0).getName());
		assertEquals(37, functionalBlockPojoB.get(0).getChildren().size());
		Assert.assertTrue(generatedUUIDandOperationMap.containsKey(functionalBlockPojoB.get(0).getUid()));
		assertEquals(FunctionalBlockGenerationResult.Operation.CREATE, generatedUUIDandOperationMap.get(functionalBlockPojoB.get(0).getUid()));

		functionalBlockService.find(b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_CONDITION)
				.withParent(p -> p.byUid(functionalBlockPojoA.get(0).getUid())));
		final List<FunctionalBlockPojo> functionalBlockTypeFunctionalConditionChildB = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_CONDITION)
						.withParent(p -> p.byUid(functionalBlockPojoB.get(0).getUid())));
		assertEquals(27, functionalBlockTypeFunctionalConditionChildB.size());
		Assert.assertTrue(generatedUUIDandOperationMap.keySet()
				.containsAll(functionalBlockTypeFunctionalConditionChildB.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList())));


		/* find the child of functional group ACCT-ACTIVE-STATUS and MY-BIN-FIELDS with type Functional_unit and check if it is generated successfully or not*/
		final List<FunctionalBlockPojo> functionalBlockTypeFunctionalUnitChildA = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT).withParent(p -> p.byUid(functionalBlockPojoA.get(0).getUid())));
		assertEquals(7, functionalBlockTypeFunctionalUnitChildA.size());

		final List<AnnotationPojo> annotationsA = annotationService.find(q -> q.ofModule(moduleIdB));
		assertEquals(7, annotationsA.size());
		final Map<Long, UUID> annotationToFunctionalUnitA = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1,
				annotationsA.stream().map(AnnotationPojo :: getId).map(EntityId :: of).toList());
		assertEquals(7, annotationToFunctionalUnitA.size());
		assertTrue(annotationToFunctionalUnitA.values()
				.containsAll(functionalBlockTypeFunctionalUnitChildA.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList())));

		final List<FunctionalBlockPojo> functionalBlockTypeFunctionalUnitChildB = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT).withParent(p -> p.byUid(functionalBlockPojoB.get(0).getUid())));
		assertEquals(5, functionalBlockTypeFunctionalUnitChildB.size());
	}

	@Test
	@Order(3)
	void testGeneratedFbForEmptyDdesIds() {
		/* Non-existent dde ids */
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
					new ArrayList<EntityId>());
		});
	}

	@Test
	@Order(4)
	void testGeneratedFbForNullDdeIds() {
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1), null);
		});
	}

	@Test
	@Order(5)
	void testGeneratedFbForEmptyVariables() {
		getModule("MMRS7101.cbl").identity();

		Assertions.assertThrows(IllegalStateException.class,
				() -> functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
						List.of(EntityId.of(20000L))));
	}

	@Test
	@Order(6)
	void testStatementOffsetWithCopybook() {
		/* verify that the correct statements are collected even when using copybooks */
		final EntityId autoFbCpy = getModule("AUTOFBCP.cpy").identity();

		final DataDictionaryPojo startVariable = createVariablesDataDictionaryEntry("WS-TOTAL-INT", autoFbCpy, "PIC9", 154, 43, true,
				DefinedLocation.COPYBOOK);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1), List.of(EntityId.of(startVariable.getId())));
		final List<FunctionalBlockPojo> allFbs = functionalBlockService.get(generated.stream().map(Pair :: getRight).toList());
		final List<FunctionalBlockPojo> generatedFbs = allFbs.stream()
				.filter(fb -> ( (List<?>) fb.getFlags().get(FunctionalBlockFlag.TYPE.name()) ).contains(FunctionalBlockType.FUNCTIONAL_GROUP.name())).toList();
		assertEquals(1, generatedFbs.size(), "Expected one block since only one program uses the variable from the copybook.");

		final List<UUID> statementBlockIds = generatedFbs.get(0).getChildren();
		assertEquals(4, statementBlockIds.size(),
				"Expected the generated functional block to contain 4 statement blocks including entryPoint and returnPoint" + ".");

		final FunctionalBlockPojo firstStatement = functionalBlockService.get(List.of(statementBlockIds.get(0))).get(0);
		final FunctionalBlockPojo secondStatement = functionalBlockService.get(List.of(statementBlockIds.get(1))).get(0);

		assertTrue(FunctionalBlockUtil.hasType(firstStatement, FunctionalBlockType.FUNCTIONAL_UNIT),
				() -> "expected statement block to have TYPE FUNCTIONAL_UNIT, but found: " + firstStatement.getFlags());
		assertTrue(FunctionalBlockUtil.hasType(secondStatement, FunctionalBlockType.FUNCTIONAL_UNIT),
				() -> "expected statement block to have TYPE FUNCTIONAL_UNIT, but found: " + secondStatement.getFlags());

		/* verify the correct statements were collected (verify fix for WMIN-13197 */
		assertEquals("COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200", firstStatement.getName());
		assertEquals("ADD WS-MONTHLY-INT TO WS-TOTAL-INT", secondStatement.getName());
	}

	@Test
	@Order(7)
	void testGeneratedFbAnnotationsTypeIsFunctional() {
		final EntityId moduleId = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ACCT-ID", moduleId, "Group", 6633, 7, true, DefinedLocation.PROGRAM);

		functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-ID in CBACT04C"));
		assertEquals(1, functionalBlockPojo.size());
		assertEquals("Functional blocks for ACCT-ID in CBACT04C", functionalBlockPojo.get(0).getName());
		assertEquals(37, functionalBlockPojo.get(0).getChildren().size());

		final List<FunctionalBlockPojo> childrenFunctionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).byUids(functionalBlockPojo.get(0).getChildren()));

		final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(
				childrenFunctionalBlockPojo.stream().map(FunctionalBlockPojo :: getUid).toList());
		assertEquals(5, generatedFrom.size());

		final List<Optional<EntityId>> annotationIds = generatedFrom.values().stream().map(GeneratedFrom :: getAnnotationId).toList();
		assertEquals(5, annotationIds.size());

		final List<AnnotationPojo> annotations = annotationService.find(
						q -> q.byIds(annotationIds.stream().filter(Optional :: isPresent).map(Optional :: get).toList())).stream()
				.filter(annotation -> annotation.getType().equals(AnnotationType.FUNCTIONAL)).toList();

		assertEquals(5, annotations.size());
	}

	@Test
	@Order(8)
	void testAutoFbBlockUpdateAfterGeneration() {
		final EntityId moduleId = getModule("AUTOFB.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("WS-TOTAL-INT", moduleId, "PIC9", 154, 43, true,
				DefinedLocation.PROGRAM);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertNotNull(generated);

		/* fetch functional group name WS-TOTAL-INT Computation */
		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for WS-TOTAL-INT in AUTOFB"));
		assertEquals(4, functionalBlockPojo.get(0).getChildren().size());

		final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype().setUid(functionalBlockPojo.get(0).getUid())
				.setName("Functional blocks for WS-TOTAL-INT in AUTOFB Updated").setDescription("Updated FB");

		functionalBlockService.update(prototype);

		final List<FunctionalBlockPojo> updatedFunctionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for WS-TOTAL-INT in AUTOFB Updated"));

		assertEquals(1, updatedFunctionalBlockPojo.size());
		assertEquals("Functional blocks for WS-TOTAL-INT in AUTOFB Updated", updatedFunctionalBlockPojo.get(0).getName());
		assertEquals("Updated FB", updatedFunctionalBlockPojo.get(0).getDescription());
		assertEquals(4, updatedFunctionalBlockPojo.get(0).getChildren().size());
	}

	@Test
	@Order(9)
	void testAutoFBChildrenReordered() {
		final EntityId moduleId = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("COB-SS", moduleId, "PICX", 10180, 6, true, DefinedLocation.PROGRAM);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertNotNull(generated);

		/* fetch functional group name COB-SS Computation */
		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for COB-SS in CBACT04C"));
		assertEquals(1, functionalBlockPojo.size());

		List<UUID> children = functionalBlockPojo.get(0).getChildren();

		List<UUID> reorderedChildren = IntStream.range(0, children.size()).boxed().sorted(Comparator.comparingInt(i -> i % 2)).map(children :: get).toList();

		FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype().setUid(functionalBlockPojo.get(0).getUid()).setChildren(reorderedChildren);
		functionalBlockService.update(prototype);

		final List<FunctionalBlockPojo> updatedFunctionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).byUid(functionalBlockPojo.get(0).getUid()));
		assertEquals(1, updatedFunctionalBlockPojo.size());
		List<UUID> updatedAnnotationChildren = updatedFunctionalBlockPojo.get(0).getChildren().subList(0, reorderedChildren.size());
		assertEquals(reorderedChildren, updatedAnnotationChildren);
	}

	@SuppressWarnings("unchecked")
	@Test
	@Order(10)
	void testEmptyAutoGeneratedBlockNotGenerated() {
		final EntityId moduleId = getModule("PROG1030.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ABEND-AREAS", moduleId, "PIC9", 10297, 11, true,
				DefinedLocation.PROGRAM);

		final String jobId = innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob(jobManager, tracer,
				new DataLineageFunctionalBlockJob(PROJECT_ID_1, List.of(EntityId.of(dataDictionaryPojo.getId()))));
		final Result<Serializable> result = (Result<Serializable>) jobManager.getJobResult(jobId);
		assertNotNull(result);
		assertEquals(Severity.WARNING, result.status.getSeverity());
		assertEquals("Some functional blocks could not be identified", result.value);
	}

	@Test
	@Order(11)
	void testGeneratedFbForDdePresentInExcludeTypeAnnotation() {
		final EntityId moduleId = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ACCTFILE-STATUS", moduleId, "Group", 7546, 15, true,
				DefinedLocation.PROGRAM);
		assertNotNull(dataDictionaryPojo);
		assertEquals("ACCTFILE-STATUS", dataDictionaryPojo.getName());

		AnnotationPojo annotationPojo = createExcludedAnnotation(moduleId, 18778, 592);
		assertNotNull(annotationPojo);
		assertEquals("Excluded Code", annotationPojo.getName());

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertFalse(generated.isEmpty());

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCTFILE-STATUS in CBACT04C"));
		assertFalse(functionalBlockPojo.isEmpty());
		assertEquals(1, functionalBlockPojo.size());

		ControlFlowGraph controlFlowGraph = functionalBlockToControlFlowGraphService.toControlFlowGraph(functionalBlockPojo.get(0));
		assertNotNull(controlFlowGraph);

		final AtomicBoolean excludeAnnotationPresent = new AtomicBoolean(false);
		controlFlowGraph.nodes.forEach(astNode -> {
			if ( "HALT".equals(astNode.type) ) {
				excludeAnnotationPresent.set(true);
				assertEquals("Excluded Code", astNode.label);
			}
		});
		assertFalse(controlFlowGraph.nodes.isEmpty());
		assertTrue(excludeAnnotationPresent.get());
	}

	@Test
	@Order(12)
	void testBranchRemovalFromAutoFunctionalBlock() {
		EntityId moduleId = getModule("MMRS7101.cbl").identity();
		DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("MY-PROGRAM-NAME", moduleId, "PICX", 1005, 15, true,
				DefinedLocation.PROGRAM);
		assertNotNull(dataDictionaryPojo);
		assertEquals("MY-PROGRAM-NAME", dataDictionaryPojo.getName());

		functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));

		List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for MY-PROGRAM-NAME in MMRS7101"));
		assertFalse(functionalBlockPojo.isEmpty());

		ExcludedBranch excludedBranch = new ExcludedBranch(new ModuleLocation(6960, 15), "FALSE");

		FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype().setUid(functionalBlockPojo.get(0).getUid())
				.setFlags(Map.of("FB_EXCLUDED_BRANCHES", List.of(excludedBranch)));
		functionalBlockService.update(prototype);

		submitJob(jobManager, new FunctionalBlockComputationJob(Set.of(functionalBlockPojo.get(0).getUid())));

		ControlFlowGraph controlFlowGraph = functionalBlockToControlFlowGraphService.toControlFlowGraph(functionalBlockPojo.get(0));
		assertNotNull(controlFlowGraph);
		assertFalse(controlFlowGraph.nodes.isEmpty());

		AtomicInteger countOfTrueBranches = new AtomicInteger();
		AtomicInteger countOfFalseBranches = new AtomicInteger();
		countOfFalseBranches.set(0);
		controlFlowGraph.edges.forEach(edge -> {
			if ( edge.label != null && edge.label.equals("TRUE") ) {
				countOfTrueBranches.getAndIncrement();
			}
			if ( edge.label != null && edge.label.equals("FALSE") ) {
				countOfFalseBranches.getAndIncrement();
			}
		});
		assertEquals(1, countOfTrueBranches.get());
		assertEquals(1, countOfFalseBranches.get());
	}

	@Order(13)
	@Test
	void testIdentifyFunctionalBlockDoesNotGenerateDuplicateStatements() {
		final EntityId moduleId = getModule("MMRS71Z1.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("MY-HEX-DIGIT", moduleId, "PICX", 997, 12, true,
				DefinedLocation.PROGRAM);
		assertNotNull(dataDictionaryPojo);
		assertEquals("MY-HEX-DIGIT", dataDictionaryPojo.getName());

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertNotNull(generated);

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for MY-HEX-DIGIT in MMRS71Z1"));
		assertFalse(functionalBlockPojo.isEmpty());

		List<UUID> generatedUUIDs = generated.stream().map(Pair :: getRight).collect(Collectors.toList());
		List<FunctionalBlockPojo> fbs = functionalBlockService.find(
				q -> q.ofProject(PROJECT_ID_1).byUids(generatedUUIDs).withType(FunctionalBlockType.FUNCTIONAL_STATEMENT));
		assertEquals(5, fbs.size());

		Set<String> names = new HashSet<>();
		for (FunctionalBlockPojo fb : fbs) {
			if ( ! fb.getName().isEmpty() ) {
				assertFalse(names.contains(fb.getName()), "Duplicate name found: " + fb.getName());
				names.add(fb.getName());
			}
		}
		assertFalse(names.isEmpty());
	}

	@Test
	@Order(14)
	void testGeneratedFbHaveAnnotationFromCrossModuleNotGenerateCFG() {
		final EntityId moduleId = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("COB-MIN", moduleId, "PICX", 10129, 7, true, DefinedLocation.PROGRAM);
		assertNotNull(dataDictionaryPojo);
		assertEquals("COB-MIN", dataDictionaryPojo.getName());

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertNotNull(generated);

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for COB-MIN in CBACT04C"));

		assertEquals(1, functionalBlockPojo.size());

		final ControlFlowGraph controlFlowGraphA = functionalBlockToControlFlowGraphService.toControlFlowGraph(functionalBlockPojo.get(0));
		assertNotNull(controlFlowGraphA);
		assertFalse(controlFlowGraphA.nodes.isEmpty(),
				"Nodes should not be empty when we run computation on Functional Block with annotation from same module");
		assertFalse(controlFlowGraphA.edges.isEmpty(),
				"Edges should not be empty when we run computation on Functional Block with annotation from same module");

		final EntityId moduleIdB = getModule("MMRS7101.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojoB = createVariablesDataDictionaryEntry("MY-HEX-ORIGIN-LEN", moduleIdB, "PIC9", 1862, 17, false,
				DefinedLocation.PROGRAM);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generatedB = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojoB.getId())));

		assertNotNull(generatedB);

		final List<FunctionalBlockPojo> functionalBlockPojoB = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for MY-HEX-ORIGIN-LEN in MMRS7101"));
		assertEquals(1, functionalBlockPojoB.size());

		final ControlFlowGraph controlFlowGraphB = functionalBlockToControlFlowGraphService.toControlFlowGraph(functionalBlockPojoB.get(0));
		assertNotNull(controlFlowGraphB);
		assertFalse(controlFlowGraphB.nodes.isEmpty(),
				"Nodes should not be empty when we run computation on Functional Block with annotation from same module");
		assertFalse(controlFlowGraphB.edges.isEmpty(),
				"Edges should not be empty when we run computation on Functional Block with annotation from same module");

		final List<UUID> functionalUnitChildren = functionalBlockService.find(
						b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT)
								.withParent(p -> p.byUid(functionalBlockPojoB.get(0).getUid())))
								.stream().map(FunctionalBlockPojo :: getUid).toList();

		final List<UUID> functionalUnitChildrenA = functionalBlockService.find(
						b -> b.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT)
								.withParent(p -> p.byUid(functionalBlockPojo.get(0).getUid())))
								.stream().map(FunctionalBlockPojo :: getUid).collect(Collectors.toList());

		functionalUnitChildrenA.addAll(functionalUnitChildren);
		final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype().setUid(functionalBlockPojo.get(0).getUid())
				.setChildren(functionalUnitChildrenA);
		functionalBlockService.deleteLinks(Assert.assertNotNull(prototype.uid.get()));
		functionalBlockService.deleteConditionsAndStatementTypeChildren(Assert.assertNotNull(prototype.uid.get()));
		functionalBlockService.update(prototype);

		submitJob(jobManager, new FunctionalBlockComputationJob(Set.of(functionalBlockPojo.get(0).getUid())));

		final List<UUID> updatedChildrenList = functionalBlockService.find(b -> b.ofProject(PROJECT_ID_1)
						.withType(FunctionalBlockType.FUNCTIONAL_UNIT).withParent(p -> p.byUid(functionalBlockPojo.get(0).getUid())))
						.stream().map(FunctionalBlockPojo :: getUid).toList();

		assertEquals(functionalUnitChildrenA.size(), updatedChildrenList.size());

		final ControlFlowGraph updatedControlFlowGraphA = functionalBlockToControlFlowGraphService.toControlFlowGraph(functionalBlockPojo.get(0));
		assertNotNull(updatedControlFlowGraphA);
		assertTrue(updatedControlFlowGraphA.nodes.isEmpty(),
				"Nodes should be empty when we run computation on Functional Block with annotation from cross module");
		assertTrue(updatedControlFlowGraphA.edges.isEmpty(),
				"Edges should be empty when we run computation on Functional Block with annotation from cross module");
	}

	private DataDictionaryPojo createVariablesDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final String format,
			final int locationOffset, final int locationLength, final boolean business, final DefinedLocation definedLocation) {

		final DataDictionaryPojoPrototype pojoPrototype = new DataDictionaryPojoPrototype().setName(dataElementName).setModule(moduleId)
				.setLocation(new ModuleLocation(locationOffset, locationLength)).setDescription("MY description").setFormat(format).setLength(1L)
				.setCreatedByUserId("admin").setPicClause("TEST").setDefinedLocation(definedLocation).setIsBusiness(business).setFieldLevel(1L)
				.setState(WorkingState.IN_ANALYSIS).setUsage("DISPLAY").setFieldTransformation("TEST TRANSFORMATION").setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT");

		return dataDictionaryService.create(pojoPrototype);
	}

	private AnnotationPojo createExcludedAnnotation(final EntityId moduleId, final Integer offset, final Integer length) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName("Excluded Code");
		annotation.setType(AnnotationType.EXCLUDE);
		annotation.setState(WorkingState.IN_ANALYSIS);
		annotation.setSourceAttachment("Source Attachment");
		annotation.setModule(moduleId);
		annotation.setLocation(new ModuleLocation(offset, length));
		annotation.setCreatedByUserId("admin");
		return annotationService.get(annotationService.create(annotation));
	}

	private ModulePojo getModule(final String fileName) {
		return moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + fileName))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: " + RESOURCE_PATH + fileName));
	}
}
