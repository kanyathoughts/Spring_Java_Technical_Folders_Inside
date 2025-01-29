/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.extensions.export.callchain;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.util.List;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.google.common.collect.Sets;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for the {@link CallChainService}.
 */
@WithMockUser
@SuppressWarnings("boxing")
class CallChainServiceTest extends DatabaseRelatedTest {

	private static final EntityId TEST_PROJECT_ID = EntityId.of(4L);

	@Autowired
	private ModuleService moduleService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private GenericConfiguration configProperties;

	@BeforeEach
	void beforeEach() throws IOException {
		/* we want to assert that the same result is produced regardless of where the trace is started, so we must reset before each test */
		resetTestData();
	}
	
	/**
	 * Tests that the {@link CallChainService} uses the default configured value for the maximum amount.
	 */
	@Test
	void testDefaultConfigWithoutParallel() {
		final TestCallChainService service = Mockito.spy(new TestCallChainService(moduleService, taxonomyService, configProperties));
		final Parameters params = new Parameters.Builder()
			.setProjectId(EntityId.of(1L))
			.setStartModuleIds(Arrays.asList(EntityId.of(2001L)))
			.setDirections(Arrays.asList(CallChainDirection.OUT))
			.setParallel(-1)
			.build();

		service.createCallChainGraphs(new NullProgressMonitor(), params);

		final ArgumentCaptor<Integer> countCaptor = ArgumentCaptor.forClass(Integer.class);
		verify(service, times(1)).createExecutorService(countCaptor.capture());

		assertEquals(Integer.valueOf(8), countCaptor.getValue());
	}
	
	/* Test to verify call chain module properties */
	@Test
	void testCallChainModuleProperties() {
		final TestCallChainService callChainService = new TestCallChainService(moduleService, taxonomyService, configProperties);
		final ModulePojo rootJob = createModule("ROOTJOB", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo stepA = createModule("ROOTJOB.STEPA", Technology.JCL, Type.EXEC_PGM, Storage.FILE);
		final ModulePojo stepB = createModule("ROOTJOB.STEPB", Technology.JCL, Type.EXEC_PGM, Storage.FILE);
		createReference(RelationshipType.CALLS, rootJob.identity(), stepA.identity(), false);
		createReference(RelationshipType.CALLS, stepA.identity(), stepB.identity(), false);

		callChainService.init();
		final Parameters params = new Parameters.Builder()
				.setProjectId(TEST_PROJECT_ID)
				.setStartModuleIds(Arrays.asList(rootJob.identity()))
				.setDirections(Arrays.asList(CallChainDirection.OUT, CallChainDirection.IN))
				.setDepth(3)
				.build();

		final Optional<List<CallChainGraph>> callChainGraphList = callChainService.createCallChainGraphs(new NullProgressMonitor(), params);
		assertEquals(2, callChainGraphList.get().size());
		callChainGraphList.ifPresent(list -> list.forEach(callChain -> assertRequiredfields(callChain)));
	}
	
	@Test
	void testCallChainWithEndModuleInput() {
		final TestCallChainService callChainService = new TestCallChainService(moduleService, taxonomyService, configProperties);
		final ModulePojo rootJob = createModule("JOB93405.AGADM213", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo step = createModule("JOB93405.AGADM213.COPY.EXEC_PGM", Technology.JCL, Type.EXEC_PGM, Storage.FILE);
		final ModulePojo file = createModule("*.STRP.STRIP.OUTPUT", Technology.RESOURCE, Type.FILE, Storage.FILE);
		createReference(RelationshipType.CALLS, rootJob.identity(), step.identity(), false);
		createReference(RelationshipType.ACCESSES, step.identity(), file.identity(), false);

		callChainService.init();
		final Parameters params = new Parameters.Builder()
				.setProjectId(TEST_PROJECT_ID)
				.setEndModuleIds(Arrays.asList(file.identity()))
				.setDirections(Arrays.asList(CallChainDirection.OUT))
				.setDepth(-1)
				.setFilteredModuleTypes(Sets.newHashSet(Type.FILE))
				.setCallTypes(Sets.newHashSet(RelationshipType.CALLS, RelationshipType.INCLUDES, RelationshipType.CONTAINS, RelationshipType.REFERENCES,
						RelationshipType.ACCESSES))
				.build();

		final Optional<List<CallChainGraph>> callChainGraphList = callChainService.createCallChainGraphs(new NullProgressMonitor(), params);
		assertEquals(1, callChainGraphList.get().size());
		assertEquals(Arrays.asList(step.getName(), file.getName()),
				callChainGraphList.get().get(0).getTargetMap().keySet().stream().map(ModuleLightweightPojo::getName).collect(Collectors.toList()));
		callChainGraphList.ifPresent(list -> list.forEach(callChain -> assertRequiredfields(callChain)));
	}
	
	@Test
	void testCallChainWithStartModuleInput() {
		final TestCallChainService callChainService = new TestCallChainService(moduleService, taxonomyService, configProperties);
		final ModulePojo rootJob = createModule("JOB93405.AGADM213", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo step = createModule("JOB93405.AGADM213.COPY.EXEC_PGM", Technology.JCL, Type.EXEC_PGM, Storage.FILE);
		final ModulePojo file = createModule("*.STRP.STRIP.OUTPUT", Technology.RESOURCE, Type.FILE, Storage.FILE);
		createReference(RelationshipType.CALLS, rootJob.identity(), step.identity(), false);
		createReference(RelationshipType.ACCESSES, step.identity(), file.identity(), false);

		callChainService.init();
		final Parameters params = new Parameters.Builder()
				.setProjectId(TEST_PROJECT_ID)
				.setEndModuleIds(Arrays.asList(rootJob.identity()))
				.setDirections(Arrays.asList(CallChainDirection.IN))
				.setDepth(-1)
				.setFilteredModuleTypes(Sets.newHashSet(Type.FILE))
				.setCallTypes(Sets.newHashSet(RelationshipType.CALLS, RelationshipType.INCLUDES, RelationshipType.CONTAINS, RelationshipType.REFERENCES,
						RelationshipType.ACCESSES))
				.build();

		final Optional<List<CallChainGraph>> callChainGraphList = callChainService.createCallChainGraphs(new NullProgressMonitor(), params);
		assertEquals(1, callChainGraphList.get().size());
		final CallChainGraph callChainGraph = callChainGraphList.get().get(0);
		assertEquals(step.getName(), callChainGraph.getRoot().getName());
		assertEquals(Arrays.asList(rootJob.getName()),
				callChainGraph.getTargetMap().keySet().stream().map(ModuleLightweightPojo::getName).collect(Collectors.toList()));
		callChainGraphList.ifPresent(list -> list.forEach(callChain -> assertRequiredfields(callChain)));
	}
	
	@Test
	void testCallChainWithStartAndEndModulesInput() {
		final TestCallChainService callChainService = new TestCallChainService(moduleService, taxonomyService, configProperties);
		final ModulePojo rootJob = createModule("JOB93405.AGADM213", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo step = createModule("JOB93405.AGADM213.COPY.EXEC_PGM", Technology.JCL, Type.EXEC_PGM, Storage.FILE);
		final ModulePojo file = createModule("*.STRP.STRIP.OUTPUT", Technology.RESOURCE, Type.FILE, Storage.FILE);
		createReference(RelationshipType.CALLS, rootJob.identity(), step.identity(), false);
		createReference(RelationshipType.ACCESSES, step.identity(), file.identity(), false);

		callChainService.init();
		final Parameters params = new Parameters.Builder()
				.setProjectId(TEST_PROJECT_ID)
				.setStartModuleIds(Arrays.asList(file.identity()))
				.setEndModuleIds(Arrays.asList(rootJob.identity()))
				.setDirections(Arrays.asList(CallChainDirection.IN))
				.setDepth(-1)
				.setFilteredModuleTypes(Sets.newHashSet(Type.FILE))
				.setCallTypes(Sets.newHashSet(RelationshipType.CALLS, RelationshipType.INCLUDES, RelationshipType.CONTAINS, RelationshipType.REFERENCES,
						RelationshipType.ACCESSES))
				.build();

		final Optional<List<CallChainGraph>> callChainGraphList = callChainService.createCallChainGraphs(new NullProgressMonitor(), params);
		assertEquals(1, callChainGraphList.get().size());
		final CallChainGraph callChainGraph = callChainGraphList.get().get(0);
		assertEquals(file.getName(), callChainGraph.getRoot().getName());
		assertEquals(Arrays.asList(step.getName(), rootJob.getName()),
				callChainGraph.getTargetMap().keySet().stream().map(ModuleLightweightPojo::getName).collect(Collectors.toList()));
		callChainGraphList.ifPresent(list -> list.forEach(callChain -> assertRequiredfields(callChain)));
	}
	
	@Test
	void testCallChainWithoutStartAndEndModulesInput() {
		final TestCallChainService callChainService = new TestCallChainService(moduleService, taxonomyService, configProperties);
		final ModulePojo rootJob = createModule("JOB93405.AGADM213", Technology.JCL, Type.JOB, Storage.FILE);
		final ModulePojo step = createModule("JOB93405.AGADM213.COPY.EXEC_PGM", Technology.JCL, Type.EXEC_PGM, Storage.FILE);
		final ModulePojo file = createModule("*.STRP.STRIP.OUTPUT", Technology.RESOURCE, Type.FILE, Storage.FILE);
		createReference(RelationshipType.CALLS, rootJob.identity(), step.identity(), false);
		createReference(RelationshipType.ACCESSES, step.identity(), file.identity(), false);

		callChainService.init();
		final Parameters params = new Parameters.Builder()
				.setProjectId(TEST_PROJECT_ID)
				.setDirections(Arrays.asList(CallChainDirection.OUT))
				.setDepth(-1)
				.setFilteredModuleTypes(Sets.newHashSet(Type.FILE))
				.setCallTypes(Sets.newHashSet(RelationshipType.CALLS, RelationshipType.INCLUDES, RelationshipType.CONTAINS, RelationshipType.REFERENCES,
						RelationshipType.ACCESSES))
				.build();

		final Optional<List<CallChainGraph>> callChainGraphList = callChainService.createCallChainGraphs(new NullProgressMonitor(), params);
		assertEquals(1, callChainGraphList.get().size());
		final CallChainGraph callChainGraph = callChainGraphList.get().get(0);
		assertEquals(rootJob.getName(), callChainGraph.getRoot().getName());
		assertEquals(Arrays.asList(step.getName()),
				callChainGraph.getTargetMap().keySet().stream().map(ModuleLightweightPojo::getName).collect(Collectors.toList()));
		callChainGraphList.ifPresent(list -> list.forEach(callChain -> assertRequiredfields(callChain)));
	}

	/**
	 * Tests that the {@link CallChainService} uses the default value from the {@link GenericConfiguration} when the parallel parameter is higher than the
	 * default.
	 */
	@Test
	void testDefaultConfigWithParallelHigher() {
		final GenericConfigProperties configProperties = new GenericConfigProperties(0, 0, 0, 0, null, 1000000, 0, 0, 0, 0L, true, false, 4, 5_000_000, -1L,
				10D, 100, null, 1000, false, 0, 0, 0, 0, 0, null, 0, 0, 0, 0);
		final TestCallChainService service = Mockito.spy(new TestCallChainService(moduleService, taxonomyService, configProperties));
		final Parameters params = new Parameters.Builder()
			.setProjectId(EntityId.of(1L))
			.setStartModuleIds(Arrays.asList(EntityId.of(2001L)))
			.setDirections(Arrays.asList(CallChainDirection.OUT))
			.setParallel(16)
			.build();

		service.createCallChainGraphs(new NullProgressMonitor(), params);

		final ArgumentCaptor<Integer> countCaptor = ArgumentCaptor.forClass(Integer.class);
		verify(service, times(1)).createExecutorService(countCaptor.capture());

		assertEquals(Integer.valueOf(4), countCaptor.getValue());
	}

	/**
	 * Tests that the {@link CallChainService} uses the parallel parameter when it doesn't exceed the default value from the {@link GenericConfiguration}.
	 */
	@Test
	void testDefaultConfigWithParallelLower() {
		final GenericConfigProperties configProperties = new GenericConfigProperties(0, 0, 0, 0, null, 1000000, 0, 0, 0, 0L, true, false, 4, 5_000_000, -1L,
				10D, 100, null, 1000, false, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		final TestCallChainService service = Mockito.spy(new TestCallChainService(moduleService, taxonomyService, configProperties));
		final Parameters params = new Parameters.Builder()
			.setProjectId(EntityId.of(1L))
			.setStartModuleIds(Arrays.asList(EntityId.of(2001L)))
			.setDirections(Arrays.asList(CallChainDirection.OUT))
			.setParallel(2)
			.build();

		service.createCallChainGraphs(new NullProgressMonitor(), params);

		final ArgumentCaptor<Integer> countCaptor = ArgumentCaptor.forClass(Integer.class);
		verify(service, times(1)).createExecutorService(countCaptor.capture());

		assertEquals(Integer.valueOf(2), countCaptor.getValue());
	}

	/**
	 * Tests that {@code Runtime.getRuntime().availableProcessors()} is used when the parallel parameter is not set and when the default value in the
	 * {@link GenericConfiguration} is {@code 0}.
	 */
	@Test
	void testDefaultConfigWithAvailableProcessors() {
		final GenericConfigProperties configProperties = new GenericConfigProperties(0, 0, 0, 0, null, 1000000, 0, 0, 0, 0L, true, false, 0, 10, -1L, 10D,
				100, null, 10, false, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		final TestCallChainService service = Mockito.spy(new TestCallChainService(moduleService, taxonomyService, configProperties));
		final Parameters params = new Parameters.Builder()
			.setProjectId(EntityId.of(1L))
			.setStartModuleIds(Arrays.asList(EntityId.of(2001L)))
			.setDirections(Arrays.asList(CallChainDirection.OUT))
			.build();

		service.createCallChainGraphs(new NullProgressMonitor(), params);

		final ArgumentCaptor<Integer> countCaptor = ArgumentCaptor.forClass(Integer.class);
		verify(service, times(1)).createExecutorService(countCaptor.capture());

		assertEquals(Integer.valueOf(Runtime.getRuntime().availableProcessors()), countCaptor.getValue());
	}
	
	@Test
	void testFromDeadCodeFalseEstablishesLinkInCallChain() {
		final ModulePojo module1 = createModule("Module 1", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo module2 = createModule("Module 2", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo module3 = createModule("Module 3", Technology.COBOL, Type.PROGRAM, Storage.FILE);
						
		createReference(RelationshipType.CALLS, module1.identity(), module2.identity(), false);
		createReference(RelationshipType.CALLS, module2.identity(), module3.identity(), false);

		final List<CallChainGraph> callChainGraphs = createCallChainGraphs(module1.identity());

		final CallChainGraph callChainGraph = callChainGraphs.stream().findAny().orElseThrow();
		assertEquals(1, callChainGraphs.size());
		assertEquals(2, callChainGraph.getSize());	/* Should result in 2 edges created in the graph */
	}
		
	@Test
	void testFromDeadCodeTrueDoesNotEstablishLinkInCallChain() {
		final ModulePojo module1 = createModule("Module 1", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo module2 = createModule("Module 2", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		final ModulePojo module3 = createModule("Module 3", Technology.COBOL, Type.PROGRAM, Storage.FILE);
		
		createReference(RelationshipType.CALLS, module1.identity(), module2.identity(), false);
		createReference(RelationshipType.CALLS, module2.identity(), module3.identity(), true);

		final List<CallChainGraph> callChainGraphs = createCallChainGraphs(module1.identity());

		final CallChainGraph callChainGraph = callChainGraphs.stream().findAny().orElseThrow();
		assertEquals(1, callChainGraphs.size());
		assertEquals(1, callChainGraph.getSize());	/* Should result in 1 edge created in the graph */
	}
	
	private ModulePojo createModule(final String name, final Technology technology, final Type type, final Storage storage, final String... path) {
		return moduleService.getModule(moduleService.create(new ModulePojoPrototype()
			.setProject(TEST_PROJECT_ID)
			.setName(name)
			.setTechnology(technology)
			.setType(type)
			.setPath(path.toString())
			.setStorage(storage)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setCreator(Creator.DISCOVERY)));
	}
	
	private void createReference(final RelationshipType relationshipType, final EntityId fromId, final EntityId toId, final boolean fromDeadCode) {
		final ModuleRelationshipPojoPrototype relationship = new ModuleRelationshipPojoPrototype()
				.setRelationship(relationshipType)
				.setSrcModule(fromId)
				.setDstModule(toId)
				.setProperties(Map.of("dead_code", fromDeadCode));
		
		
		moduleService.createRelationship(relationship);
	}
	
	private List<CallChainGraph> createCallChainGraphs(EntityId startModule) {
		final Parameters params = new Parameters.Builder()
				.setProjectId(TEST_PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(startModule))
				.setDirections(List.of(CallChainDirection.OUT))
				.setFilteredModuleTypes(Sets.newHashSet(Type.FILE))
				.setCallTypes(Sets.newHashSet(RelationshipType.CALLS, 
											  RelationshipType.INCLUDES, 
											  RelationshipType.CONTAINS, 
											  RelationshipType.REFERENCES, 
											  RelationshipType.ACCESSES))
				.build();

		final TestCallChainService service = new TestCallChainService(moduleService, taxonomyService, configProperties);
		service.init();

		return service.createCallChainGraphs(new NullProgressMonitor(), params).orElseThrow();
	}
	
	private void assertRequiredfields(final CallChainGraph callChain) {
		assertNotNull(callChain.getRoot().getLinkHash());
		assertNotNull(callChain.getRoot().identity());
		assertNotNull(callChain.getRoot().getName());
	}

	private static class TestCallChainService extends CallChainService {

		public TestCallChainService(final ModuleService moduleService, final TaxonomyService taxonomyService, final GenericConfiguration configProperties) {
			super(moduleService, taxonomyService, configProperties);
		}

		@Override
		public ExecutorService createExecutorService(final int nThreads) {
			return super.createExecutorService(nThreads);
		}
	}
}
