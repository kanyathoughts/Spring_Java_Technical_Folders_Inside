/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.shared.entities.ProjectPojoPrototype;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock.DataLineageFunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.job.AutomaticGeneratedFunctionalBlockDeletionJob;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
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
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests for {@link AutomaticGeneratedFunctionalBlockDeletionJob}.
 */

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@WithMockUser
class AutomaticGeneratedFunctionalBlockDeletionTest extends AbstractIdentificationTest {

	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;

	@Autowired
	private FunctionalBlockService functionalBlockService;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	@BeforeAll
	void init() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "CBACT04C", "CBACT04C.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createVariablesDataDictionaryEntry("ACCT-ID", moduleId, "Group", 6633, 7);
		createVariablesDataDictionaryEntry("ACCT-ADDR-ZIP", moduleId, "Group", 7253, 13);
	}

	@Test
	@Order(1)
	void testDeleteFunctionalConditions() {
		final EntityId moduleId = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ACCOUNT-RECORD", moduleId, "Group", 6602, 14);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertEquals(45, generated.size());

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCOUNT-RECORD in CBACT04C"));

		assertEquals(1, functionalBlockPojo.size());
		assertEquals("Functional blocks for ACCOUNT-RECORD in CBACT04C", functionalBlockPojo.get(0).getName());
		assertEquals(60, functionalBlockPojo.get(0).getChildren().size());

		final List<FunctionalBlockPojo> children = functionalBlockService.get(functionalBlockPojo.get(0).getChildren());
		final List<FunctionalBlockPojo> functionalConditions = children.stream()
				.filter(child -> ( (List<?>) child.getFlags().get(FunctionalBlockFlag.TYPE.name()) ).contains(FunctionalBlockType.FUNCTIONAL_CONDITION.name()))
				.toList();
		assertEquals(28, functionalConditions.size());

		submitAutomaticGeneratedFunctionalBlockDeletionJob(functionalBlockPojo.get(0).getUid());

		final List<FunctionalBlockPojo> functionalBlockPojoAfterDelete = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCOUNT-RECORD in CBACT04C"));
		assertEquals(0, functionalBlockPojoAfterDelete.size());
		assertEquals(0, functionalBlockService.get(functionalConditions.stream().map(FunctionalBlockPojo :: getUid).toList()).size());
		List<UUID> uuids = generated.stream().map(Pair :: getRight).toList();
		functionalBlockService.delete(uuids);
	}

	@Test
	@Order(2)
	void testNonFunctionalUnitsNotDeleted() {
		final EntityId moduleIdA = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ACCT-ACTIVE-STATUS", moduleIdA, "PICX", 6693, 18);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		assertEquals(33, generated.size());

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-ACTIVE-STATUS in CBACT04C"));

		assertEquals(1, functionalBlockPojo.size());
		assertEquals("Functional blocks for ACCT-ACTIVE-STATUS in CBACT04C", functionalBlockPojo.get(0).getName());
		assertEquals(37, functionalBlockPojo.get(0).getChildren().size());

		final List<FunctionalBlockPojo> children = functionalBlockService.get(functionalBlockPojo.get(0).getChildren());
		final List<FunctionalBlockPojo> functionalUnits = children.stream()
				.filter(child -> ( (List<?>) child.getFlags().get(FunctionalBlockFlag.TYPE.name()) ).contains(FunctionalBlockType.FUNCTIONAL_UNIT.name())
						&& child.getParents().size() > 1).toList();

		assertEquals(5, functionalUnits.size());

		final var childBlock1 = createFunctionalBlockPojoPrototype("childBlock1", "Child Block 1", new ModuleLocation(100, 250), null,
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT)), null);
		final var childBlockUid1 = functionalBlockService.create(childBlock1);

		functionalBlockService.setGeneratedFrom(childBlockUid1, GeneratedFrom.fromAnnotation(getAnnotationPojoPrototype(moduleIdA)));
		final List<UUID> updatedChildren = new ArrayList<>();
		updatedChildren.addAll(functionalBlockPojo.get(0).getChildren());
		updatedChildren.add(childBlockUid1);
		functionalBlockService.update(convertToPrototype(functionalBlockPojo.get(0), updatedChildren));

		submitAutomaticGeneratedFunctionalBlockDeletionJob(functionalBlockPojo.get(0).getUid());

		final List<FunctionalBlockPojo> functionalBlockPojoAfterDelete = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-ACTIVE-STATUS in CBACT04C"));

		assertEquals(0, functionalBlockPojoAfterDelete.size());
		assertEquals(0, functionalBlockService.get(functionalUnits.stream().map(FunctionalBlockPojo :: getUid).toList()).size());
		assertTrue(functionalBlockService.find(childBlockUid1).isPresent(), "Non Functional Unit should not be deleted");
		List<UUID> uuids = generated.stream().map(Pair :: getRight).toList();
		functionalBlockService.delete(uuids);
	}

	@Test
	@Order(3)
	void testNonOrphanedFunctionalUnitsNotDeleted() {
		EntityId module = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = getDataDictionaryPojo("ACCT-ID", module);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
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

		final List<UUID> functionalUnits = childrenFunctionalBlockPojo.stream()
				.filter(child -> ( (List<?>) child.getFlags().get(FunctionalBlockFlag.TYPE.name()) ).contains(FunctionalBlockType.FUNCTIONAL_UNIT.name()))
				.map(FunctionalBlockPojo :: getUid).toList();
		assertEquals(5, functionalUnits.size());

		final var functionalBlock = createFunctionalBlockPojoPrototype("functionalBlock", "functionalBlock Desc", new ModuleLocation(100, 250),
				List.of(functionalUnits.get(0), functionalUnits.get(1)), Map.of(FunctionalBlockFlag.TYPE.name(),
						List.of(FunctionalBlockType.FUNCTIONAL_GROUP)),
				null);
		final UUID uid = functionalBlockService.create(functionalBlock);

		submitAutomaticGeneratedFunctionalBlockDeletionJob(functionalBlockPojo.get(0).getUid());

		assertEquals(2, functionalBlockService.get(List.of(functionalUnits.get(0), functionalUnits.get(1))).size());
		functionalBlockService.delete(uid);
		functionalBlockService.delete(functionalUnits.get(0));
		functionalBlockService.delete(functionalUnits.get(1));
		List<UUID> uuids = generated.stream().map(Pair :: getRight).toList();
		functionalBlockService.delete(uuids);
	}

	@Test
	@Order(4)
	void testDeleteOrphanedFunctionalUnits() {
		EntityId module = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = createVariablesDataDictionaryEntry("ACCT-CURR-CYC-DEBIT", module, "PIC9", 7189, 19);
		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));

		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-CURR-CYC-DEBIT in CBACT04C"));

		assertEquals(1, functionalBlockPojo.size());
		assertEquals("Functional blocks for ACCT-CURR-CYC-DEBIT in CBACT04C", functionalBlockPojo.get(0).getName());
		assertEquals(39, functionalBlockPojo.get(0).getChildren().size());

		final Map<EntityId, UUID> maps = functionalBlockService.getSingleParentFunctionalUnitsByAnnotationType(PROJECT_ID_1,
				functionalBlockPojo.get(0).getUid(), "FUNCTIONAL");

		submitAutomaticGeneratedFunctionalBlockDeletionJob(functionalBlockPojo.get(0).getUid());
		List<UUID> uuids = new ArrayList<>(maps.values());
		assertEquals(0, functionalBlockService.get(uuids).size());
		List<UUID> generatedUids = generated.stream().map(Pair :: getRight).toList();
		functionalBlockService.delete(generatedUids);
	}

	@SuppressWarnings("unchecked")
	@Order(5)
	@Test
	void testDeleteNonExistentFunctionalBlock() {
		final UUID nonExistentFunctionalBlockUid = UUID.randomUUID();

		final String jobId = innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob(jobManager, tracer,
				new AutomaticGeneratedFunctionalBlockDeletionJob(PROJECT_ID_1, nonExistentFunctionalBlockUid));
		final Result<Serializable> result = (Result<Serializable>) jobManager.getJobResult(jobId);

		assertNotNull(result);
		assertEquals(Severity.ERROR, result.status.getSeverity());
		assertEquals("FunctionalBlockPojo identifier not found: " + nonExistentFunctionalBlockUid, result.status.getMessage());
	}

	@Test
	@Order(6)
	void testNonDeletionOfSharedFunctionalUnits() {
		EntityId module = getModule("CBACT04C.cbl").identity();
		final DataDictionaryPojo dataDictionaryPojo = getDataDictionaryPojo("ACCT-ADDR-ZIP", module);
		final DataDictionaryPojo dataDictionaryPojo2 = getDataDictionaryPojo("ACCT-ID", module);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated1 = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo.getId())));
		functionalBlockGenerationService.generate(DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo2.getId())));
		final List<FunctionalBlockPojo> functionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-ADDR-ZIP in CBACT04C"));

		assertTrue(! functionalBlockPojo.isEmpty());
		assertEquals("Functional blocks for ACCT-ADDR-ZIP in CBACT04C", functionalBlockPojo.get(0).getName());
		assertEquals(37, functionalBlockPojo.get(0).getChildren().size());

		final List<FunctionalBlockPojo> functionalBlockPojo2 = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCT-ID in CBACT04C"));

		assertEquals(1, functionalBlockPojo2.size());
		assertEquals("Functional blocks for ACCT-ID in CBACT04C", functionalBlockPojo2.get(0).getName());
		assertEquals(37, functionalBlockPojo2.get(0).getChildren().size());

		final List<FunctionalBlockPojo> childrenFunctionalBlockPojo = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).byUids(functionalBlockPojo.get(0).getChildren()).withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		final List<FunctionalBlockPojo> childrenFunctionalBlockPojo2 = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).byUids(functionalBlockPojo2.get(0).getChildren()).withType(FunctionalBlockType.FUNCTIONAL_UNIT));

		final List<UUID> uids1 = childrenFunctionalBlockPojo.stream().map(FunctionalBlockPojo :: getUid).toList();
		final List<UUID> uids2 = childrenFunctionalBlockPojo2.stream().map(FunctionalBlockPojo :: getUid).toList();

		assertTrue(uids1.containsAll(uids2) && uids2.containsAll(uids1));

		final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(
				childrenFunctionalBlockPojo.stream().map(FunctionalBlockPojo :: getUid).toList());
		assertEquals(5, generatedFrom.size());

		final List<Optional<EntityId>> annotationIds = generatedFrom.values().stream().map(GeneratedFrom :: getAnnotationId).toList();
		assertEquals(5, annotationIds.size());

		final List<AnnotationPojo> annotations = annotationService.find(
						q -> q.byIds(annotationIds.stream().filter(Optional :: isPresent).map(Optional :: get).toList())).stream()
				.filter(annotation -> annotation.getType().equals(AnnotationType.FUNCTIONAL)).toList();

		assertEquals(5, annotations.size());
		final List<UUID> functionalUnits = childrenFunctionalBlockPojo.stream()
				.filter(child -> ( (List<?>) child.getFlags().get(FunctionalBlockFlag.TYPE.name()) ).contains(FunctionalBlockType.FUNCTIONAL_UNIT.name()))
				.map(FunctionalBlockPojo :: getUid).toList();
		assertEquals(5, functionalUnits.size());

		submitAutomaticGeneratedFunctionalBlockDeletionJob(functionalBlockPojo.get(0).getUid());
		assertEquals(5, functionalBlockService.get(functionalUnits).size());
		List<UUID> generatedUids = generated1.stream().map(Pair :: getRight).toList();
		functionalBlockService.delete(generatedUids);
	}

	@Test
	@Order(7)
	void testAutoFBGenerationOnDDEofSimilarModuleButDifferentProjects() {
		final EntityId moduleIdA = getModule("CBACT04C.cbl").identity();
		var project2 = projectService.create(
				new ProjectPojoPrototype().setName("Test Project 2").setClient(EntityId.of(1L)).setNatures(Collections.emptySet()));
		final EntityId moduleIdB = createModule(project2.identity(), "CBACT04C", "CBACT04C.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		final DataDictionaryPojo dataDictionaryPojo1 = createVariablesDataDictionaryEntry("ACCOUNT-FILE", moduleIdA, "Group", 3440, 12);
		final DataDictionaryPojo dataDictionaryPojo2 = createVariablesDataDictionaryEntry("ACCOUNT-FILE", moduleIdB, "Group", 3440, 12);

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated1 = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(PROJECT_ID_1),
				List.of(EntityId.of(dataDictionaryPojo1.getId())));

		assertEquals(42, generated1.size());

		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated2 = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, new FunctionalBlockGenerationContext(project2.identity()),
				List.of(EntityId.of(dataDictionaryPojo2.getId())));

		assertEquals(42, generated2.size());

		final List<FunctionalBlockPojo> functionalBlockPojo1 = functionalBlockService.find(
				b -> b.ofProject(PROJECT_ID_1).withName("Functional blocks for ACCOUNT-FILE in CBACT04C"));

		assertEquals(1, functionalBlockPojo1.size());
		assertEquals("Functional blocks for ACCOUNT-FILE in CBACT04C", functionalBlockPojo1.get(0).getName());

		final List<FunctionalBlockPojo> functionalBlockPojo2 = functionalBlockService.find(
				b -> b.ofProject(project2.identity()).withName("Functional blocks for ACCOUNT-FILE in CBACT04C"));

		assertEquals(1, functionalBlockPojo2.size());
		assertEquals("Functional blocks for ACCOUNT-FILE in CBACT04C", functionalBlockPojo2.get(0).getName());
	}

	private EntityId getAnnotationPojoPrototype(final EntityId moduleIdA) {
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(0);
		dummyLocation.setLength(0);
		final AnnotationPojoPrototype annotation1 = new AnnotationPojoPrototype().setName("Database Annotation 1").setType(AnnotationType.DATABASE)
				.setState(WorkingState.CANDIDATE).setSourceAttachment("This is if-ELSE source attachment \n content").setModule(moduleIdA)
				.setLocation(dummyLocation).setCreatedByUserId("1").setUpdatedByUserId("");
		return annotationService.create(annotation1);
	}

	private DataDictionaryPojo createVariablesDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final String format,
			final int locationOffset, final int locationLength) {
		final DataDictionaryPojoPrototype pojoPrototype = new DataDictionaryPojoPrototype().setName(dataElementName).setModule(moduleId)
				.setLocation(new ModuleLocation(locationOffset, locationLength)).setDescription("MY description").setFormat(format).setLength(1L)
				.setCreatedByUserId("admin").setPicClause("TEST").setDefinedLocation(DefinedLocation.PROGRAM).setIsBusiness(true).setFieldLevel(1L)
				.setState(WorkingState.IN_ANALYSIS).setUsage("DISPLAY").setFieldTransformation("TEST TRANSFORMATION").setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT");

		return dataDictionaryService.create(pojoPrototype);
	}

	private ModulePojo getModule(final String fileName) {
		return moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + fileName))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: " + RESOURCE_PATH + fileName));
	}

	private DataDictionaryPojo getDataDictionaryPojo(final String name, final EntityId moduleId) {
		return dataDictionaryService.get(b -> b.withName(name).ofModule(moduleId));
	}

	protected void submitAutomaticGeneratedFunctionalBlockDeletionJob(final UUID functionalBlockUid) {
		submitJob(jobManager, new AutomaticGeneratedFunctionalBlockDeletionJob(PROJECT_ID_1, functionalBlockUid));
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc,
			@Nullable final ModuleLocation moduleLocation,
			final @Nullable List<UUID> children, final @Nullable Map<String, Object> flags, @Nullable final ModulePojo module) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(AbstractIdentificationTest.PROJECT_ID_1);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		if ( module != null ) {
			final List<ModulePart> moduleParts = new ArrayList<>();
			final ModulePart functionalBlockModulePart = new ModulePart(module.getLinkHash(), moduleLocation);
			moduleParts.add(functionalBlockModulePart);
			functionalBlockPojoPrototype.setModuleParts(moduleParts);
		}
		if ( children != null ) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if ( flags != null ) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		return functionalBlockPojoPrototype;
	}

	private FunctionalBlockPojoPrototype convertToPrototype(final FunctionalBlockPojo functionalBlockPojo, final List<UUID> updatedChildren) {
		final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype();
		prototype.setName(functionalBlockPojo.getName());
		prototype.setUid(functionalBlockPojo.getUid());
		prototype.setDescription(functionalBlockPojo.getDescription());
		prototype.setProject(functionalBlockPojo.getProject());
		prototype.setModuleParts(functionalBlockPojo.getModuleParts());
		prototype.setChildren(updatedChildren);
		prototype.setFlags(functionalBlockPojo.getFlags());
		return prototype;
	}
}
