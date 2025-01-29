package innowake.mining.server.scheduler;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.job.JobUtil;
import innowake.mining.server.job.ControlMImportJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojo;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests for {@link ControlMImportJob}.
 */
@WithMockUser
class ControlMImportJobTest extends DatabaseRelatedTest {

	private final ModuleService moduleService;
	private final SchedulerInfoService schedulerInfoService;
	private final SourceService sourceService;
	private final JobManager jobManager;
	private final Tracer tracer;

	private static final Long ONE = 1L;
	private static final Path BASE_PATH = Paths.get("./test-resources/innowake/mining/server/scheduler");

	@Autowired
	public ControlMImportJobTest(final ModuleService moduleService, final SchedulerInfoService schedulerInfoService, final SourceService sourceService,
			final JobManager jobManager, final Tracer tracer) {
		this.moduleService = moduleService;
		this.schedulerInfoService = schedulerInfoService;
		this.sourceService = sourceService;
		this.jobManager = jobManager;
		this.tracer = tracer;
	}

	@BeforeEach
	void deleteAllModules() {
		moduleService.deleteModules(EntityId.of(ONE), true, false);
	}

	@Test
	void testBareMinimum() throws IOException {
		setupTestDataAndRunImport();

		final List<SchedulerEntryPojo> entries = schedulerInfoService.findEntries(builder -> builder.ofProject(EntityId.of(ONE)));

		assertEquals(13, entries.size(), "Total entry count does not match");
		assertEquals(3, entries.stream()
				.map(SchedulerEntryPojo::getType)
				.filter(SchedulerEntryType.JOB::equals)
				.count(), "Total job entry count does not match");
		assertEquals(6, entries.stream()
				.map(SchedulerEntryPojo::getType)
				.filter(SchedulerEntryType.CONDITION::equals)
				.count(), "Total condition entries does not match");
		assertEquals(2, entries.stream()
				.map(SchedulerEntryPojo::getType)
				.filter(SchedulerEntryType.FOLDER::equals)
				.count(), "Total folder entries does not match");

		final SchedulerEntryPojo root = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("root"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Root entry not found"));

		final SchedulerEntryPojo table = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("DEFTABLE"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Def Table entry not found"));

		final SchedulerEntryPojo smartFolder = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("SMART_FOLDER"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Smart Folder entry not found"));

		final SchedulerEntryPojo subFolder = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("SUB_FOLDER"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("Sub Folder entry not found"));

		final SchedulerEntryPojo anyJob = entries.stream()
				.filter(entry -> entry.getIdentifier()
						.equals("JOB"))
				.findFirst()
				.orElseThrow(() -> new AssertionError("JOB entry not found"));

		assertEquals(root.getUid(), table.getContainedIn(), "Def table not contained in root");
		assertEquals(table.getUid(), smartFolder.getContainedIn(), "Smart Folder not contained in Def table");
		assertEquals(smartFolder.getUid(), subFolder.getContainedIn(), "Sub Folder not contained in Smart Folder");
		assertEquals(subFolder.getUid(), anyJob.getContainedIn(), "Job not contained in Sub Folder");
	}

	@Test
	void testEntryLinkAndRelationshipAreEstablished() throws IOException {
		final UUID job1 = createTestModule("JOB1", "/jcl/jobs/job1.job", EntityId.of(ONE)).getUid();
		final UUID job2 = createTestModule("JOB2", "/jcl/jobs/job2.job", EntityId.of(ONE)).getUid();
		final UUID job3 = createTestModule("JOB3", "/jcl/jobs/job3.job", EntityId.of(ONE)).getUid();

		setupTestDataAndRunImport();

		final List<SchedulerEntryPojo> entries = schedulerInfoService.findEntries(new Pagination(100), builder -> builder.ofProject(EntityId.of(ONE)))
				.getContent();
		final List<SchedulerImportPojo> imports = schedulerInfoService.findImports(Pagination.FIRST, q -> q.ofProject(EntityId.of(ONE))).getContent();

		assertEquals(1, imports.size(), "Import count does not match");
		assertEquals(Map.of(SchedulerImportPojo.PROPERTY_CREATE_MODULE_IF_MISSING, false,
				SchedulerImportPojo.PROPERTY_ESTABLISH_MODULE_RELATIONSHIP, false), imports.get(0).getProperties(), "Import properties do not match");
		assertEquals(1L, entries.stream()
				.map(SchedulerEntryPojo::getModule)
				.filter(job1::equals)
				.count(), "Job1 not linked");
		assertEquals(1L, entries.stream()
				.map(SchedulerEntryPojo::getModule)
				.filter(job2::equals)
				.count(), "Job2 not linked");
		assertEquals(1L, entries.stream()
				.map(SchedulerEntryPojo::getModule)
				.filter(job3::equals)
				.count(), "Job3 not linked");

		final List<SchedulerEntryRelationshipPojo> relationships = schedulerInfoService.findRelationships(new Pagination(100),
						builder -> builder.ofProject(EntityId.of(ONE)))
				.getContent();

		assertEquals(job3, relationships.stream()
				.filter(relationship -> assertNotNull(relationship.getPredecessorModule())
						.equals(job1))
				.findFirst()
				.map(SchedulerEntryRelationshipPojo::getSuccessorModule)
				.orElseThrow(() -> new AssertionError("Job1 -> Job3 relationship not found")), "Job1 -> Job3 relationship not found");

		assertEquals(job3, relationships.stream()
				.filter(relationship -> assertNotNull(relationship.getPredecessorModule())
						.equals(job2))
				.findFirst()
				.map(SchedulerEntryRelationshipPojo::getSuccessorModule)
				.orElseThrow(() -> new AssertionError("Job2 -> Job3 relationship not found")), "Job2 -> Job3 relationship not found");
	}

	@Test
	void testMissingModulesAreCreatedFromImport() throws IOException {
		setupTestDataAndRunImport(true, false);
		final List<SchedulerImportPojo> imports = schedulerInfoService.findImports(Pagination.FIRST, q -> q.ofProject(EntityId.of(ONE))).getContent();

		assertEquals(1, imports.size(), "Import count does not match");
		assertEquals(Map.of(SchedulerImportPojo.PROPERTY_CREATE_MODULE_IF_MISSING, true,
				SchedulerImportPojo.PROPERTY_ESTABLISH_MODULE_RELATIONSHIP, false), imports.get(0).getProperties(), "Import properties do not match");

		final UUID job1 = createTestModule("JOB1", "/jcl/jobs/job1.job", EntityId.of(ONE)).getUid();

		final Optional<ModuleLightweightPojo> job1Module = moduleService.findAnyModuleLightweight(
				x -> x.byUid(job1));
		assertTrue(job1Module.isPresent(), "Job1 not found");

		final List<ModuleLightweightPojo> job2Modules = moduleService.findModulesLightweight(
				x -> x.ofProject(EntityId.of(ONE)).withName("JOB2").withIdentified(false).withTechnology(Technology.JCL).withType(Type.JOB));
		assertEquals(1, job2Modules.size(), job2Modules.isEmpty() ? "Module not created from import" : job2Modules.size() + " modules created from import");
	}

	@Test
	void testModuleRelationshipIsEstablished() throws IOException {
		final UUID job1 = createTestModule("JOB1", "/jcl/jobs/job1.job", EntityId.of(ONE)).getUid();
		final UUID job3 = createTestModule("JOB3", "/jcl/jobs/job3.job", EntityId.of(ONE)).getUid();

		setupTestDataAndRunImport(false, true);
		final List<SchedulerImportPojo> imports = schedulerInfoService.findImports(Pagination.FIRST, q -> q.ofProject(EntityId.of(ONE))).getContent();

		assertEquals(1, imports.size(), "Import count does not match");
		assertEquals(Map.of(SchedulerImportPojo.PROPERTY_CREATE_MODULE_IF_MISSING, false,
				SchedulerImportPojo.PROPERTY_ESTABLISH_MODULE_RELATIONSHIP, true), imports.get(0).getProperties(), "Import properties do not match");

		final List<ModuleRelationshipPojo> relationshipPojoList = moduleService.findRelationship(
				x -> x.ofProject(EntityId.of(ONE)).ofSource(job1).ofDestination(job3).withType(RelationshipType.PRECEDES));
		assertEquals(1, relationshipPojoList.size(),
				relationshipPojoList.isEmpty() ? "Relationship not established" : relationshipPojoList.size() + " relationships established instead of 1");
	}

	private void setupTestDataAndRunImport() throws IOException {
		setupTestDataAndRunImport(false, false);
	}

	private void setupTestDataAndRunImport(final boolean createMissingModule, final boolean establishModuleRelationship) throws IOException {
		final EntityId projectId = EntityId.of(ONE);
		schedulerInfoService.deleteSchedulerImport(builder -> builder.ofProject(projectId));

		final Path path = Paths.get("simple_controlm_import.xml");

		final byte[] content = Files.readAllBytes(BASE_PATH.resolve(path));
		final SchedulerImportPojoPrototype prototype = new SchedulerImportPojoPrototype()
				.setProject(projectId)
				.setSchedulerType(SchedulerType.CONTROL_M)
				.setImporterUsed("default")
				.setIdentifier("controlM")
				.setDescription("ControlM Import")
				.setProperties(Map.of(SchedulerImportPojo.PROPERTY_CREATE_MODULE_IF_MISSING, createMissingModule,
								SchedulerImportPojo.PROPERTY_ESTABLISH_MODULE_RELATIONSHIP, establishModuleRelationship))
				.setSource(sourceService.put(projectId, new BinaryString(content)));
		final ControlMImportJob job = new ControlMImportJob(prototype);
		JobUtil.submitJob(jobManager, tracer, job);
	}

	private ModulePojo createTestModule(final String name, @Nullable final String path, final EntityId projectId) {
		final ModulePojoPrototype jclModule = new ModulePojoPrototype();
		jclModule.setProject(projectId);
		jclModule.setName(name);
		jclModule.setTechnology(Technology.JCL);
		jclModule.setType(Type.JOB);
		jclModule.setOrigin(Origin.CUSTOM);
		jclModule.setStorage(Storage.FILE);
		jclModule.setIdentification(Identification.IDENTIFIED);
		jclModule.setCreator(Creator.DISCOVERY);
		jclModule.setPath(path);
		jclModule.setContent("");
		return moduleService.getModule(moduleService.create(jclModule));
	}
}
