/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import brave.internal.Nullable;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.functionalblocks.FunctionalBlockMergeRequest;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisRequest;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import innowake.mining.server.functionalblocks.backup.FunctionalBlockBackupService;

import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static java.util.UUID.randomUUID;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test for {@link FunctionalBlockController}
 */
@AutoConfigureMockMvc
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class FunctionalBlockControllerTests extends DatabaseRelatedTest {
	@Autowired
	private MockMvc mvc;
	@Autowired
	private FunctionalBlockService functionalBlockService;
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private FunctionalBlockComputationService functionalBlockComputationService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private JobManager jobManager;
	@Autowired
	private ObjectMapper objectMapper;
	@Autowired
	private FunctionalBlockBackupService functionalBlockBackupService;
	private ModulePojo startModule1;
	private ModulePojo cobolModule;
	private ModulePojo csdFile;
	private ModulePojo startModule2;
	private ModulePojo naturalModule;
	private ModulePojo resourceFile;
	private ModulePojo resourceGdg;
	private ModulePojo startModule3;
	private ModulePojo javaModule;
	private ModulePojo tableFile;
	private ModulePojo startModule4;
	private ModulePojo accessModule1;
	private ModulePojo imsDbd;
	private final UUID mergeParent = randomUUID();
	private EntityId projectId = EntityId.VOID;
	private ModulePojo module1;
	private ModulePojo module2;
	private ModulePojo module3;
	private ModulePojo module4;
	private EntityId taxonomyId;

	@BeforeAll
	void initialize() {
		projectId = loadProjectAndClient("Mock Project 2", EntityId.of(1L));

		/* This will be ignored since CSD File is not a valid lower bound */
		startModule1 = createModule(projectId, "startModule1", Technology.JCL, Type.JOB, null);
		cobolModule = createModule(projectId, "cobolModule", Technology.COBOL, Type.PROGRAM, null);
		csdFile = createModule(projectId, "CSDFile", Technology.CSD, Type.FILE, null);
		createReference(startModule1.identity(), cobolModule.identity(), RelationshipType.CALLS);
		createReference(cobolModule.identity(), csdFile.identity(), RelationshipType.ACCESSES);

		startModule2 = createModule(projectId, "startModule2", Technology.JCL, Type.JOB, null);
		naturalModule = createModule(projectId, "naturalModule", Technology.NATURAL, Type.PROGRAM, null);
		resourceFile = createModule(projectId, "resourceFile", Technology.RESOURCE, Type.FILE, null);
		resourceGdg = createModule(projectId, "resourceGdg", Technology.RESOURCE, Type.GDG_FILE, null);
		createReference(startModule2.identity(), naturalModule.identity(), RelationshipType.CALLS);
		createReference(naturalModule.identity(), resourceFile.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ.name()));
		createReference(naturalModule.identity(), resourceGdg.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ.name()));

		startModule3 = createModule(projectId, "startModule3", Technology.SQL, Type.STORED_PROCEDURE, null);
		javaModule = createModule(projectId, "javaModule", Technology.JAVA, Type.PROGRAM, null);
		tableFile = createModule(projectId, "tableFile", Technology.SQL, Type.TABLE, null);
		createReference(startModule3.identity(), javaModule.identity(), RelationshipType.CALLS);
		createReference(javaModule.identity(), tableFile.identity(), RelationshipType.ACCESSES);
		createReference(javaModule.identity(), resourceFile.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.WRITE.name()));

		startModule4 = createModule(projectId, "startModule4", Technology.JCL, Type.JOB, null);
		accessModule1 = createModule(projectId, "accessModule1", Technology.NATURAL, Type.PROGRAM, null);
		imsDbd = createModule(projectId, "imsDbd", Technology.IMS, Type.DBD, null);
		createReference(startModule4.identity(), accessModule1.identity(), RelationshipType.CALLS);
		createReference(accessModule1.identity(), imsDbd.identity(), RelationshipType.ACCESSES);
		createReference(accessModule1.identity(), resourceGdg.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.WRITE.name()));

		taxonomyId = createAndAssignTaxonomies(projectId, "New", "Term", List.of(startModule1, startModule2, startModule3, startModule4));
	}

	@Test
	@Order(1)
	void testFunctionalBlockGeneration() throws Exception {
		final var result = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_GENERATION_MODULE_BLOCKS , projectId.getNid()).contentType("application/json")
						.content(PojoMapper.jsonWriter().writeValueAsString(new ModuleMatcher(List.of(startModule1.identity(), cobolModule.identity(), csdFile.identity())
								, null)))).andExpect(status().isAccepted()).andReturn();
		final var jobId = result.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId, jobManager, 100L, TimeUnit.MINUTES);
		final var createdBlocks = functionalBlockService.find(q -> q.ofProject(projectId));
		assertEquals(3, createdBlocks.size());
		final List<FunctionalBlockPojo> moduleBlocks = createdBlocks.parallelStream().filter(b -> !b.getModuleParts().isEmpty()).toList();
		assertEquals(3, moduleBlocks.size());
		assertEquals(Set.of(startModule1.getName(), cobolModule.getName(), csdFile.getName()),
				moduleBlocks.parallelStream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));
	}

	@Test
	@Order(2)
	void testReachabilityBlockComputation() throws Exception {
		reachabilityAnalysisJob(projectId, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));
		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(4, reachabilityBlock.size());
		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlocks(reachabilityBlock.stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList())));
		final Set<ResolvedModulePart> resolvedModuleParts = functionalBlockService.getResolvedModuleParts(reachabilityBlock.stream()
						.map(FunctionalBlockPojo::getUid)
						.collect(Collectors.toList()))
				.values()
				.stream()
				.flatMap(List::stream)
				.collect(Collectors.toSet());

		assertEquals(7, reachabilityData.size());
		assertEquals(Set.of(startModule1.getId(), cobolModule.getId(), csdFile.getId(),
						startModule2.getId(), resourceFile.getId(), naturalModule.getId(), resourceGdg.getId(),
						startModule3.getId(), javaModule.getId(), tableFile.getId(),
						startModule4.getId(), accessModule1.getId(), imsDbd.getId()),
				resolvedModuleParts.parallelStream().map(ResolvedModulePart::getModuleId).map(EntityId::getNid).collect(Collectors.toSet()));
		assertEquals(Set.of(startModule1.getId(), startModule2.getId(), startModule3.getId(), startModule4.getId()),
				reachabilityData.stream().map(rd -> rd.getUpperBoundModuleId().getNid()).collect(Collectors.toSet()));
		assertEquals(Set.of(resourceFile.getId(), resourceGdg.getId(), tableFile.getId(), imsDbd.getId()), reachabilityData.stream()
				.filter(rd -> rd.getLowerBoundModuleId().isPresent()).map(rd -> rd.getLowerBoundModuleId().get().getNid()).collect(Collectors.toSet()));

		final var reachabilityBlockNetwork = functionalBlockService.find(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY_NETWORK));
		assertEquals(1, reachabilityBlockNetwork.size());

		/* as of WMIN-14071, this is no longer done automatically */
		functionalBlockComputationService.compute(List.of(reachabilityBlockNetwork.get(0).getUid()), new NullProgressMonitor());

		final var networkGraph = mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE,
						projectId.getNid(), reachabilityBlockNetwork.get(0).getUid()).contentType("application/json").content("{}"))
				.andExpect(status().isOk());
		final DependencyGraph dependencyGraph = objectMapper.readValue(networkGraph.andReturn().getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, dependencyGraph.getModules().size());

		final var module1 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule1")).findFirst().orElseThrow();
		final var module2 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule2")).findFirst().orElseThrow();
		final var module3 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule3")).findFirst().orElseThrow();
		final var module4 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule4")).findFirst().orElseThrow();

		assertTrue(Set.of(module1.getId(), module2.getId(), module3.getId(), module4.getId())
				.containsAll(dependencyGraph.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(4, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(Set.of(module1.getId(), module2.getId(), module3.getId(), module4.getId())));
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(Set.of(Pair.of(module3.getUid(), module2.getUid()), Pair.of(module4.getUid(), module2.getUid())),
				dependencyGraph.getReferences().stream().map(ref -> Pair.of(ref.getSrcModule(), ref.getDstModule())).collect(Collectors.toSet()));
		assertEquals(Set.of(Optional.of(RelationshipDirection.OUT)),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getDirection).collect(Collectors.toSet()));

		final var filteredNetworkGraph = mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE,
						projectId.getNid(), reachabilityBlockNetwork.get(0).getUid()).contentType("application/json").content("""
								{
									"technologyTypes": [{
										"technology": "RESOURCE",
										"type": "FILE"
									}]
								}"""))
				.andExpect(status().isOk());
		final DependencyGraph filteredGraph = objectMapper.readValue(filteredNetworkGraph.andReturn().getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, filteredGraph.getModules().size());

		assertTrue(Set.of(module1.getId(), module2.getId(), module3.getId(), module4.getId())
				.containsAll(filteredGraph.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(4, filteredGraph.getRootModuleIds().size());
		assertTrue(filteredGraph.getRootModuleIds().containsAll(Set.of(module1.getId(), module2.getId(), module3.getId(), module4.getId())));
		assertEquals(1, filteredGraph.getReferences().size());
		assertEquals(module3.getUid(), filteredGraph.getReferences().get(0).getSrcModule());
		assertEquals(module2.getUid(), filteredGraph.getReferences().get(0).getDstModule());
		assertEquals(RelationshipDirection.OUT, filteredGraph.getReferences().get(0).getDirection().orElseThrow());
	}

	@Test
	@Order(3)
	void testFunctionalBlockMerging() throws Exception {
		reachabilityAnalysisJob(projectId, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));
		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY));
		final var reachabilityBlockNetwork = functionalBlockService.find(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY_NETWORK));

		/* as of WMIN-14071, this is no longer done automatically */
		functionalBlockComputationService.compute(List.of(reachabilityBlockNetwork.get(0).getUid()), new NullProgressMonitor());

		submitMergeFunctionalBlockJob(projectId, mergeParent, "Merge Parent Functional Block 1",
				reachabilityBlock.stream().filter(fb -> fb.getName().equals("startModule1") || fb.getName().equals("startModule2")).collect(Collectors.toList()));
		final var parentPojo = functionalBlockService.find(reachabilityBlockNetwork.get(0).getUid()).orElseThrow();
		final var mergeParentPojo = functionalBlockService.find(mergeParent).orElseThrow();
		assertEquals(2, mergeParentPojo.getChildren().size());
		assertTrue(mergeParentPojo.getFlags().get(FunctionalBlockFlag.TYPE.name()).toString().contains(FunctionalBlockType.MERGE_PARENT.name()));
		assertEquals(3, parentPojo.getChildren().size());

		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlock(mergeParent));
		assertTrue(mergeParentPojo.getChildren().containsAll(reachabilityBlock.stream()
				.filter(fb -> fb.getName().equals("startModule1") || fb.getName().equals("startModule2"))
				.map(FunctionalBlockPojo::getUid).toList()));
		assertTrue(parentPojo.getChildren().contains(mergeParent));
		assertEquals(3, reachabilityData.size());
		assertEquals(Set.of(startModule1.getId(), startModule2.getId()),
				reachabilityData.stream().map(rd -> rd.getUpperBoundModuleId().getNid()).collect(Collectors.toSet()));
		assertEquals(Set.of(resourceFile.getId(), resourceGdg.getId()), reachabilityData.stream()
				.filter(rd -> rd.getLowerBoundModuleId().isPresent()).map(rd -> rd.getLowerBoundModuleId().get().getNid()).collect(Collectors.toSet()));

		assertEquals(1, reachabilityBlockNetwork.size());

		final var networkGraph = mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE,
						projectId.getNid(), reachabilityBlockNetwork.get(0).getUid()).contentType("application/json").content("{}"))
				.andExpect(status().isOk());
		final DependencyGraph dependencyGraph = objectMapper.readValue(networkGraph.andReturn().getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(3, dependencyGraph.getModules().size());

		final var module1 = dependencyGraph.getModules().stream().filter(mod ->
				mod.getName().equals("Merge Parent Functional Block 1")).findFirst().orElseThrow();
		final var module2 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule3")).findFirst().orElseThrow();
		final var module3 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule4")).findFirst().orElseThrow();
		assertTrue(dependencyGraph.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())
				.containsAll(Set.of(module1.getId(), module2.getId(), module3.getId())));
		assertEquals(3, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(Set.of(module1.getId(), module2.getId(), module3.getId())));
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(Set.of(Pair.of(module3.getUid(), module1.getUid()), Pair.of(module2.getUid(), module1.getUid())),
				dependencyGraph.getReferences().stream().map(ref -> Pair.of(ref.getSrcModule(), ref.getDstModule())).collect(Collectors.toSet()));
		assertEquals(Set.of(Optional.of(RelationshipDirection.OUT)),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getDirection).collect(Collectors.toSet()));
	}

	@Test
	@Order(4)
	void testFunctionalBlockUnmerging() throws Exception {
		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY)).
				stream().filter(fb -> fb.getName().equals("startModule2")).toList();
		final var mergeParentChildren = functionalBlockService.find(q -> q.ofProject(projectId).withType(FunctionalBlockType.REACHABILITY)).
						stream().filter(fb -> fb.getName().equals("startModule1"))
				.map(FunctionalBlockPojo::getUid).toList();
		final var reachabilityBlockNetwork = functionalBlockService.find(q -> q.withType(FunctionalBlockType.REACHABILITY_NETWORK));

		final Instant startTime = Instant.now();
		final FunctionalBlockMergeRequest request = new FunctionalBlockMergeRequest(reachabilityBlockNetwork.get(0).getUid(), mergeParent, null,
				reachabilityBlock.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()), false);

		mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_UNMERGE, projectId.getNid())
						.contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(request)))
				.andExpect(status().isOk());

		final var computationJob = JobTestHelper.findJobByLastSubmitTime(jobManager, "Functional Block Computation",
				startTime, 10L).orElseThrow();
		JobTestHelper.waitForJobCompletion(computationJob.getJobId(), jobManager, 10L, TimeUnit.MINUTES);

		final var parentPojo = functionalBlockService.find(reachabilityBlockNetwork.get(0).getUid()).orElseThrow();
		final var mergeParentPojo = functionalBlockService.find(mergeParent).orElseThrow();
		assertEquals(1, mergeParentPojo.getChildren().size());
		assertEquals(4, parentPojo.getChildren().size());
		assertTrue(parentPojo.getChildren().containsAll(List.of(mergeParent, reachabilityBlock.get(0).getUid())));
		assertTrue(mergeParentPojo.getChildren().containsAll(mergeParentChildren));

		/* as of WMIN-14071, this is no longer done automatically */
		functionalBlockComputationService.compute(List.of(reachabilityBlockNetwork.get(0).getUid()), new NullProgressMonitor());

		final var networkGraph = mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE,
						projectId.getNid(), reachabilityBlockNetwork.get(0).getUid()).contentType("application/json").content("{}"))
				.andExpect(status().isOk());
		final DependencyGraph dependencyGraph = objectMapper.readValue(networkGraph.andReturn().getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, dependencyGraph.getModules().size());

		final var module1 = dependencyGraph.getModules().stream().filter(mod -> mod.getName()
				.equals("Merge Parent Functional Block 1")).findFirst().orElseThrow();
		final var module2 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule2")).findFirst().orElseThrow();
		final var module3 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule3")).findFirst().orElseThrow();
		final var module4 = dependencyGraph.getModules().stream().filter(mod -> mod.getName().equals("startModule4")).findFirst().orElseThrow();

		assertTrue(dependencyGraph.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())
				.containsAll(Set.of(module1.getId(), module2.getId(), module3.getId(), module4.getId())));
		assertEquals(4, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(Set.of(module1.getId(), module2.getId(), module3.getId(), module4.getId())));
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(Set.of(Pair.of(module3.getUid(), module2.getUid()), Pair.of(module4.getUid(), module2.getUid())),
				dependencyGraph.getReferences().stream().map(ref -> Pair.of(ref.getSrcModule(), ref.getDstModule())).collect(Collectors.toSet()));
		assertEquals(Set.of(Optional.of(RelationshipDirection.OUT)),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getDirection).collect(Collectors.toSet()));
	}

	@Test
	void testGetDependencyGraphForFunctionalBlock() throws Exception {

		module1 = createModule(projectId, "MOD1", Technology.COBOL, Type.PROGRAM, null);
		module2 = createModule(projectId, "MOD2", Technology.COBOL, Type.PROGRAM, null);
		module3 = createModule(projectId, "MOD3", Technology.COBOL, Type.PROGRAM, null);
		module4 = createModule(projectId, "MOD4", Technology.COBOL, Type.PROGRAM, null);
		createReference(module1.identity(), module2.identity(), RelationshipType.CALLS);

		final UUID groupUId = createFunctionalBlocks();

		final MvcResult result = mvc
				.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE, projectId.getNid(), groupUId))
				.andDo(print()).andExpect(status().isOk()).andReturn();

		final DependencyGraph dependencyGraph = objectMapper.readValue(result.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, dependencyGraph.getModules().size());
		assertEquals(2 ,dependencyGraph.getRootModuleIds().size());
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(1, dependencyGraph.getModuleTypes().size());
		assertEquals("COBOL PROGRAM", dependencyGraph.getModuleTypes().get(0));
		assertEquals(2, dependencyGraph.getReferences().size());
		assertEquals(Set.of(RelationshipType.INCLUDES, RelationshipType.CALLS),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));

		final Map<String, Integer> peerCounts = new HashMap<>();
		dependencyGraph.getModules().forEach(module -> {
			if (module.getInfo().isPresent()) {
				final String peerCount =
						module.getInfo().get().containsKey("peerCount") ? module.getInfo().get().get("peerCount").toString() : null;
				peerCounts.put(module.getName(), peerCount != null ? Integer.parseInt(peerCount) : null);
			}});
		assertEquals(2, peerCounts.get("MOD1"));
		assertEquals(1, peerCounts.get("MOD2"));
		Assertions.assertNull(peerCounts.get("MOD3"));
		assertNull(peerCounts.get("MOD4"));
	}

	@Test
	void testReachabilityBlockComputationHavingNoLowerBound() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 3", EntityId.of(1L));
		final var startModule4 = createModule(projectId1, "ModuleHavingNoLowerBound", Technology.JCL, Type.JOB, null);
		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "test", "Term", List.of(startModule4));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var functionalBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY)).stream()
				.filter(fb -> fb.getName().equals("ModuleHavingNoLowerBound")).toList();
		assertEquals(1, functionalBlock.size());
		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlock(functionalBlock.get(0).getUid()));
		assertEquals(1, reachabilityData.size());
		assertEquals(Set.of(startModule4.getId()), reachabilityData.stream().map(rd -> rd.getUpperBoundModuleId().getNid()).collect(Collectors.toSet()));
	}

	@Test
	void testGetDependencyGraphForMergedFunctionalBlockHavingBlocksAndEdgesInCommon() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 4", EntityId.of(1L));
		final ModulePojo startModulePojo1 = createModule(projectId1, "startModulePojo1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessModulePojo1 = createModule(projectId1, "accessModulePojo1", Technology.NATURAL, Type.PROGRAM, null);
		final ModulePojo accessModulePojo2 = createModule(projectId1, "accessModulePojo2", Technology.NATURAL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModulePojo1 = createModule(projectId1, "lowerBoundModulePojo1", Technology.RESOURCE, Type.FILE, null);
		final ModulePojo lowerBoundModulePojo2 = createModule(projectId1, "lowerBoundModulePojo2", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo startModulePojo2 = createModule(projectId1, "startModulePojo2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessModulePojo3 = createModule(projectId1, "accessModulePojo3", Technology.NATURAL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModulePojo3 = createModule(projectId1, "lowerBoundModulePojo3", Technology.RESOURCE, Type.FILE, null);

		createReference(startModulePojo1.identity(), accessModulePojo1.identity(), RelationshipType.CALLS);
		createReference(startModulePojo1.identity(), accessModulePojo2.identity(), RelationshipType.CALLS);
		createReference(accessModulePojo1.identity(), lowerBoundModulePojo1.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.toString(), List.of(ModelAttributeValue.FileAccess.READ.toString())));
		createReference(accessModulePojo2.identity(), lowerBoundModulePojo2.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.DB_ACCESS_TYPE.toString(), List.of(DatabaseAccessType.STORE.toString())));
		createReference(startModulePojo2.identity(), accessModulePojo2.identity(), RelationshipType.CALLS);
		createReference(startModulePojo2.identity(), accessModulePojo3.identity(), RelationshipType.CALLS);
		createReference(accessModulePojo3.identity(), lowerBoundModulePojo3.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.FILE_ACCESS_TYPE.toString(), List.of(ModelAttributeValue.FileAccess.WRITE.toString())));
		createReference(accessModulePojo2.identity(), lowerBoundModulePojo2.identity(), RelationshipType.ACCESSES,
				Map.of(ModelAttributeKey.DB_ACCESS_TYPE.toString(), List.of(DatabaseAccessType.UPDATE.toString())));

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "Dependency", "Term", List.of(startModulePojo1, startModulePojo2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final List<FunctionalBlockPojo> reachabilityBlocks = functionalBlockService.find(q -> q.ofProject(projectId1)
						.withType(FunctionalBlockType.REACHABILITY)).stream()
				.filter(fb -> fb.getName().equals("startModulePojo1") || fb.getName().equals("startModulePojo2")).collect(Collectors.toList());
		final var mergeParent1 = UUID.randomUUID();
		submitMergeFunctionalBlockJob(projectId1, mergeParent1, "Merge Parent Functional Block 2", reachabilityBlocks);

		final var mergeParentPojo = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withName("Merge Parent Functional Block 2")).stream().findFirst().orElseThrow();

		final MvcResult result = mvc
				.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE, projectId1.getNid(), mergeParentPojo.getUid()))
				.andDo(print()).andExpect(status().isOk()).andReturn();

		final DependencyGraph dependencyGraph = objectMapper.readValue(result.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(8, dependencyGraph.getModules().size());
		assertEquals(2 ,dependencyGraph.getRootModuleIds().size());
		assertEquals(Set.of(startModulePojo1.getId(), startModulePojo2.getId()), new HashSet<>(dependencyGraph.getRootModuleIds()));
		assertEquals(2, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());
		assertEquals(Set.of("RESOURCE FILE", "NATURAL PROGRAM", "JCL JOB"), new HashSet<>(dependencyGraph.getModuleTypes()));
		assertEquals(7, dependencyGraph.getReferences().size());
		assertEquals(Set.of(RelationshipType.ACCESSES, RelationshipType.CALLS),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
		final var accessProps = dependencyGraph.getReferences().stream()
				.map(ModuleRelationshipPojo::getProperties)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.map(Objects::toString)
				.collect(Collectors.joining("|"));
		assertEquals(3, accessProps.split("\\|").length);
		assertTrue(accessProps.contains(ModelAttributeKey.FILE_ACCESS_TYPE.toString()));
		assertTrue(accessProps.contains(ModelAttributeKey.DB_ACCESS_TYPE.toString()));
		assertTrue(accessProps.contains("READ"));
		assertTrue(accessProps.contains("WRITE"));
		assertTrue(accessProps.contains("STORE"));
		assertTrue(accessProps.contains("UPDATE"));
	}

	@Test
	void testGetDependencyGraphForMergedFunctionalBlockHavingNoBlockOrEdgesInCommon() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 5", EntityId.of(1L));
		final ModulePojo startModulePojo1 = createModule(projectId1, "startPojo1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessModulePojo1 = createModule(projectId1, "accessPojo1", Technology.NATURAL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModulePojo1 = createModule(projectId1, "lowerPojo1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo startModulePojo2 = createModule(projectId1, "startPojo2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessModulePojo2 = createModule(projectId1, "accessPojo3", Technology.NATURAL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModulePojo2 = createModule(projectId1, "lowerPojo3", Technology.RESOURCE, Type.FILE, null);

		createReference(startModulePojo1.identity(), accessModulePojo1.identity(), RelationshipType.CALLS);
		createReference(accessModulePojo1.identity(), lowerBoundModulePojo1.identity(), RelationshipType.ACCESSES);
		createReference(startModulePojo2.identity(), accessModulePojo2.identity(), RelationshipType.CALLS);
		createReference(accessModulePojo2.identity(), lowerBoundModulePojo2.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "DependencyWithNoCommonBlocks", "Term", List.of(startModulePojo1, startModulePojo2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY_NETWORK)).stream().findFirst().orElseThrow();
		final List<FunctionalBlockPojo> reachabilityBlocks = functionalBlockService.find(q -> q.ofProject(projectId1)
						.withType(FunctionalBlockType.REACHABILITY)).stream()
				.filter(fb -> fb.getName().equals("startPojo1") || fb.getName().equals("startPojo2")).collect(Collectors.toList());

		final var mergeParent1 = UUID.randomUUID();
		submitMergeFunctionalBlockJob(projectId1, mergeParent1, "Merge Block", reachabilityBlocks);

		final var mergeParentPojo = functionalBlockService.find(q -> q.ofProject(projectId1).withName("Merge Block")).stream().findFirst().orElseThrow();

		final MvcResult result = mvc
				.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE, projectId1.getNid(), mergeParentPojo.getUid()))
				.andDo(print()).andExpect(status().isOk()).andReturn();

		final DependencyGraph dependencyGraph = objectMapper.readValue(result.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(6, dependencyGraph.getModules().size());
		assertEquals(2 ,dependencyGraph.getRootModuleIds().size());
		assertEquals(Set.of(startModulePojo1.getId(), startModulePojo2.getId()), new HashSet<>(dependencyGraph.getRootModuleIds()));
		assertEquals(2, dependencyGraph.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph.getModuleTypes().size());
		assertEquals(Set.of("RESOURCE FILE", "NATURAL PROGRAM", "JCL JOB"), new HashSet<>(dependencyGraph.getModuleTypes()));
		assertEquals(4, dependencyGraph.getReferences().size());
		assertEquals(Set.of(RelationshipType.ACCESSES, RelationshipType.CALLS),
				dependencyGraph.getReferences().stream().map(ModuleRelationshipPojo::getRelationship).collect(Collectors.toSet()));
	}

	@Test
	void testRecalculateOutDatedBlockComputation() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 6", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundOutDated1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundOutDated1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundOutDated1", Technology.RESOURCE, Type.FILE, null);

		ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundOutDated2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundOutDated2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundOutDated2", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule3 = createModule(projectId1, "UpperBoundOutDated3", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule3 = createModule(projectId1, "AccessBoundOutDated3", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule3 = createModule(projectId1, "LowerBoundOutDated3", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule3.identity(), accessBoundModule3.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "Outdated", "Term", List.of(upperBoundModule1, upperBoundModule2, upperBoundModule3));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withTypes(List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)));
		assertEquals(3, reachabilityBlock.size());
		assertTrue(reachabilityBlock.stream().allMatch(fb -> fb.getFlags().containsKey(FunctionalBlockFlag.OUTDATED.name())));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(0).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(1).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(2).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));

		/* Updating the Upper bound module and check whether the Block is outdated or not */
		moduleService.deleteModule(upperBoundModule2.identity(), true);
		upperBoundModule2 = createModule(projectId1, "UpperBoundOutDated2", Technology.JCL, Type.JOB, "7777771");
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);

		/* deleting the access module and check whether the Block is outdated or not */
		moduleService.deleteModule(accessBoundModule3.identity(), true);

		final var allFunctionalBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1));

		final var moduleFunctionalBlocks = allFunctionalBlocks1.stream()
				.filter(block -> FunctionalBlockUtil.hasType(block, FunctionalBlockType.MODULE))
				.map(block -> block.getUid())
				.collect(Collectors.toSet());

		final var singleReachabilityBlocks = allFunctionalBlocks1.stream()
				.filter(block -> ! FunctionalBlockUtil.hasType(block, List.of(FunctionalBlockType.MODULE,
						FunctionalBlockType.MERGE_PARENT)))
				.map(block -> block.getUid())
				.collect(Collectors.toSet());

		final var mergedReachabilityBlock = allFunctionalBlocks1.stream()
				.filter(block -> FunctionalBlockUtil.hasType(block, FunctionalBlockType.MERGE_PARENT))
				.map(block -> block.getUid())
				.collect(Collectors.toSet());

		final var result2 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(moduleFunctionalBlocks))).andExpect(status().isOk()).andReturn();
		final var jobId2 = result2.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId2, jobManager, 10L, TimeUnit.MINUTES);

		final var result3 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(singleReachabilityBlocks))).andExpect(status().isOk()).andReturn();
		final var jobId3 = result3.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId3, jobManager, 10L, TimeUnit.MINUTES);

		final var result4 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(mergedReachabilityBlock))).andExpect(status().isOk()).andReturn();
		final var jobId4 = result4.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId4, jobManager, 10L, TimeUnit.MINUTES);

		final var outDatedBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(2, outDatedBlocks1.size());
		assertEquals(Set.of("UpperBoundOutDated2", "UpperBoundOutDated3"), Set.of(outDatedBlocks1.get(0).getName(), outDatedBlocks1.get(1).getName()));

		/* Checks if the deleted module is marked as deleted in the functional block */
		final Object accessBoundModuleDeleted = functionalBlockService.find(q -> q.ofProject(projectId1).withName("AccessBoundOutDated3")).get(0).getFlags().get("DELETED");
		assertNotNull(accessBoundModuleDeleted);
		assertTrue((boolean) accessBoundModuleDeleted);
		final FunctionalBlockPojo callChainBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withName("UpperBoundOutDated3" + " " + FunctionalBlockType.CALL_CHAIN)).get(0);
		/*Checks the deleted data in graph's module info */
		final MvcResult mvcResult = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE, projectId1.getNid(), callChainBlock.getUid()).contentType(
						"application/json").content("{}")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(3, dependencyGraph.getModules().size());
		final Optional<ModulePojo> accessBoundOutDated3 = dependencyGraph.getModules().stream().filter(module -> module.getName().equals("AccessBoundOutDated3")).findFirst();
		assertTrue(accessBoundOutDated3.isPresent());
		assertTrue(accessBoundOutDated3.get().getInfo().isPresent());
		assertTrue(accessBoundOutDated3.get().getInfo().get().containsKey("DELETED"));
		assertTrue((boolean) accessBoundOutDated3.get().getInfo().get().get("DELETED"));

		final var result5 = mvc.perform(
				put("/api" + FunctionalBlockController.RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL, projectId1.getNid())
						.contentType("application/json")).andExpect(status().isOk()).andReturn();
		final var jobId5 = result5.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId5, jobManager, 10L, TimeUnit.MINUTES);

		final var outDatedBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(0, outDatedBlocks2.size());
	}

	@Test
	void testRecalcualteOutDatedBlockComputationForMergeBlocks() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 7", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundMergeOutDated1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundMergeOutDated1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundMergeOutDated1", Technology.RESOURCE, Type.FILE, null);

		ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundMergeOutDated2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundMergeOutDated2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundMergeOutDated2", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "MergeOutdated", "Term", List.of(upperBoundModule1, upperBoundModule2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withTypes(List.of(FunctionalBlockType.REACHABILITY, FunctionalBlockType.RA_TOP_DOWN)));
		assertEquals(2, reachabilityBlock.size());
		assertTrue(reachabilityBlock.stream().allMatch(fb -> fb.getFlags().containsKey(FunctionalBlockFlag.OUTDATED.name())));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(0).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(1).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));

		final UUID mergeParentUUID = UUID.randomUUID();
		submitMergeFunctionalBlockJob(projectId1, mergeParentUUID, "MergeOutdatedBlock", reachabilityBlock);

		final var mergeParentPojo1 = functionalBlockService.find(mergeParentUUID).orElseThrow();
		assertEquals(2, mergeParentPojo1.getChildren().size());
		assertFalse((boolean) mergeParentPojo1.getFlags().get(FunctionalBlockFlag.OUTDATED.name()));

		/* Updating the Upper bound module and check whether the Block is outdated or not */
		moduleService.deleteModule(upperBoundModule2.identity(), true);
		upperBoundModule2 = createModule(projectId1, "UpperBoundMergeOutDated2", Technology.JCL, Type.JOB, "7777771");
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);

		final var allFunctionalBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());

		final var result2 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks1))).andExpect(status().isOk()).andReturn();
		final var jobId2 = result2.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId2, jobManager, 100L, TimeUnit.MINUTES);

		final var outDatedBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(2, outDatedBlocks1.size());
		assertEquals(Set.of("UpperBoundMergeOutDated2", "MergeOutdatedBlock"), Set.of(outDatedBlocks1.get(0).getName(), outDatedBlocks1.get(1).getName()));

		final var result3 = mvc.perform(
				put("/api" + FunctionalBlockController.RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL, projectId1.getNid())
						.contentType("application/json")).andExpect(status().isOk()).andReturn();
		final var jobId3 = result3.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId3, jobManager, 100L, TimeUnit.MINUTES);

		final var outDatedBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(0, outDatedBlocks2.size());
	}

	@Test
	void testCreateReachabilityBlockForTopDownComputation() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 8", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundTopDown1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundTopDown1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundTopDown1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundTopDown2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundTopDown2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundTopDown2", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);

		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName("CreateTopDown")
				.setProject(projectId1));
		final var createdTaxonomy = taxonomyService.create(new TaxonomyPojoPrototype()
				.setName("Term")
				.setType(type)
				.setProject(projectId1));

		taxonomyService.createModuleLink(upperBoundModule1.getUid(), createdTaxonomy);
		taxonomyService.createModuleLink(upperBoundModule2.getUid(), createdTaxonomy);

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(createdTaxonomy), Collections.emptySet(), false));
		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(2, reachabilityBlock.size());
		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlocks(reachabilityBlock.stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList())));
		final Set<ResolvedModulePart> resolvedModuleParts = functionalBlockService.getResolvedModuleParts(reachabilityBlock.stream()
						.map(FunctionalBlockPojo::getUid)
						.collect(Collectors.toList()))
				.values()
				.stream()
				.flatMap(List::stream)
				.collect(Collectors.toSet());

		assertEquals(2, reachabilityData.size());
		assertEquals(Set.of(upperBoundModule1.getId(), upperBoundModule2.getId(), lowerBoundModule1.getId(), lowerBoundModule2.getId(), accessBoundModule1.getId(),
						accessBoundModule2.getId()),
				resolvedModuleParts.parallelStream().map(ResolvedModulePart::getModuleId).map(EntityId::getNid).collect(Collectors.toSet()));
		assertEquals(Set.of(upperBoundModule1.getId(), upperBoundModule2.getId()),
				reachabilityData.stream().map(rd -> rd.getUpperBoundModuleId().getNid()).collect(Collectors.toSet()));
		assertEquals(Set.of(lowerBoundModule1.getId(), lowerBoundModule2.getId()), reachabilityData.stream()
				.filter(rd -> rd.getLowerBoundModuleId().isPresent()).map(rd -> rd.getLowerBoundModuleId().get().getNid()).collect(Collectors.toSet()));
	}

	@Test
	void testCreateReachabilityBlockBottomUpComputation() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 9", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundBottomUp1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundBottomUp1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundBottomUp1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundBottomUp2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundBottomUp2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundBottomUp2", Technology.RESOURCE, Type.FILE, null);

		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("CreateBottomUp").setProject(projectId1));
		final var createdTaxonomy = taxonomyService.create(new TaxonomyPojoPrototype().setName("Term").setType(type).setProject(projectId1));

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);

		taxonomyService.createModuleLink(lowerBoundModule1.getUid(), createdTaxonomy);
		taxonomyService.createModuleLink(lowerBoundModule2.getUid(), createdTaxonomy);

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.BOTTOM_UP,
				Set.of(createdTaxonomy), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(4, reachabilityBlock.size());
		assertEquals(2, reachabilityBlock.stream().filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_BOTTOM_UP)).count());
		assertEquals(2, reachabilityBlock.stream().filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_TOP_DOWN)).count());
	}

	@Test
	void testRemoveDeleteFunctionalBlockComputation() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 10", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundDeleted1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundDeleted1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundDeleted1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundDeleted2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundDeleted2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundDeleted2", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "DeletedUpperBound", "Term",
				List.of(upperBoundModule1, upperBoundModule2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.REACHABILITY).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(0, reachabilityBlock.size());

		moduleService.deleteModule(upperBoundModule2.identity(), true);
		moduleService.deleteModule(accessBoundModule1.identity(), true);

		final var moduleFunctionalBlocks = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());

		final var result2 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(moduleFunctionalBlocks))).andExpect(status().isOk()).andReturn();
		final var jobId2 = result2.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId2, jobManager, 10L, TimeUnit.MINUTES);

		final var outdatedFunctionalBlocks = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true)
				.withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(2, outdatedFunctionalBlocks.size());

		final var deletedFunctionalBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true)
				.withFlag(FunctionalBlockFlag.DELETED, true)
				.withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(1, deletedFunctionalBlocks1.size());
		assertEquals("UpperBoundDeleted2", deletedFunctionalBlocks1.get(0).getName());

		final var result3 = mvc.perform(delete("/api" + FunctionalBlockController.REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL, projectId1.getNid())
				.contentType("application/json")).andExpect(status().isOk()).andReturn();
		final var jobId3 = result3.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId3, jobManager, 10L, TimeUnit.MINUTES);

		final var deletedFunctionalBlock2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true)
				.withFlag(FunctionalBlockFlag.DELETED, true)
				.withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(0, deletedFunctionalBlock2.size());
		assertTrue(functionalBlockService.getLinks(deletedFunctionalBlocks1.get(0).getUid()).isEmpty());
	}

	/*
	 *                         MergedBlock
	 *     ub1  ----------> am1 -----------> lb1
	 *     ub2  ----------> am2 -----------> lb2
	 *     ub3  ----------> am3 -----------> lb3
	 */
	@DisplayName("Test Remove Delete Functional Block Computation For Merge Blocks When Single Block Is Deleted")
	@Test
	void testMergeBlocksWhenSingleBlockIsDeleted() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 11", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundMergeSingleDeleted1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundMergeSingleDeleted1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundMergeSingleDeleted1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundMergeSingleDeleted2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundMergeSingleDeleted2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundMergeSingleDeleted2", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule3 = createModule(projectId1, "UpperBoundMergeSingleDeleted3", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule3 = createModule(projectId1, "AccessBoundMergeSingleDeleted3", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule3 = createModule(projectId1, "LowerBoundMergeSingleDeleted3", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule3.identity(), accessBoundModule3.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "MergeDeletedSingle", "Term",
				List.of(upperBoundModule1, upperBoundModule2, upperBoundModule3));

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		/* Assert No deleted or updated blocks yet */
		final var reachabilityBlocks = functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(3, reachabilityBlocks.size());
		assertFalse(Boolean.parseBoolean(reachabilityBlocks.get(0).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlocks.get(1).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlocks.get(2).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));

		final UUID mergeParentUUID = UUID.randomUUID();
		submitMergeFunctionalBlockJob(projectId1, mergeParentUUID, "MergeSingleDeletedBlock", reachabilityBlocks);

		final var mergeParentPojo1 = functionalBlockService.find(mergeParentUUID).orElseThrow();
		assertEquals(3, mergeParentPojo1.getChildren().size());
		assertFalse((boolean) mergeParentPojo1.getFlags().get(FunctionalBlockFlag.OUTDATED.name()));
		assertFalse((boolean) mergeParentPojo1.getFlags().get(FunctionalBlockFlag.DELETED.name()));

		/* delete UB2 and asser rb2 is deleted and no outdated blocks yet */
		moduleService.deleteModule(upperBoundModule2.identity(), true);
		final var allFunctionalBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());

		final var result2 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks1))).andExpect(status().isOk()).andReturn();
		final var jobId2 = result2.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId2, jobManager, 10L, TimeUnit.MINUTES);

		final var deletedBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(1, deletedBlocks1.size());
		assertEquals("UpperBoundMergeSingleDeleted2", deletedBlocks1.get(0).getName());

		final var outdatedBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.MERGE_PARENT));
		assertEquals(0, outdatedBlocks1.size());

		/* Delete am1 and assert that rb1 and merged blocks is outdated */
		moduleService.deleteModule(accessBoundModule1.identity(), true);

		final var allFunctionalBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());

		final var result3 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks2))).andExpect(status().isOk()).andReturn();
		final var jobId3 = result3.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId3, jobManager, 10L, TimeUnit.MINUTES);

		final var deletedBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(1, deletedBlocks2.size());
		assertEquals("UpperBoundMergeSingleDeleted2", deletedBlocks2.get(0).getName());

		final var outdatedBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.MERGE_PARENT));
		assertEquals(1, outdatedBlocks2.size());

		/* Perform deleted blocks removal */
		final var result4 = mvc.perform(delete("/api" + FunctionalBlockController.REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL, projectId1.getNid())
				.contentType("application/json")).andExpect(status().isOk()).andReturn();
		final var jobId4 = result4.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId4, jobManager, 10L, TimeUnit.MINUTES);

		final var deletedBlocks3 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(0, deletedBlocks3.size());
		final FunctionalBlockPojo mergeParentPojo2 = functionalBlockService.find(mergeParentUUID).orElseThrow();
		assertFalse((boolean) mergeParentPojo2.getFlags().get(FunctionalBlockFlag.DELETED.name()));
		assertEquals(2, mergeParentPojo2.getChildren().size());
		assertTrue(functionalBlockService.getLinks(deletedBlocks1.get(0).getUid()).isEmpty());
		final List<UUID> deletedChildBlocks = deletedBlocks1.stream().flatMap(block -> block.getChildren().stream()).collect(Collectors.toList());
		assertTrue(functionalBlockService.get(deletedChildBlocks).isEmpty());
		assertFalse(functionalBlockService.getLinks(mergeParentPojo2.getChildren()).isEmpty());

		/* Delete ub3 and perform deleted blocks removal and assert that merged block no longer exists */
		moduleService.deleteModule(upperBoundModule3.identity(), true);

		final var allFunctionalBlocks3 = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());

		final var result5 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks3))).andExpect(status().isOk()).andReturn();
		final var jobId5 = result5.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId5, jobManager, 10L, TimeUnit.MINUTES);

		final var deletedBlocks4 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(1, deletedBlocks4.size());
		assertEquals("UpperBoundMergeSingleDeleted3", deletedBlocks4.get(0).getName());

		mvc.perform(delete("/api" + FunctionalBlockController.REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL, projectId1.getNid())
				.contentType("application/json")).andExpect(status().isOk());

		final var deletedBlocks5 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(0, deletedBlocks5.size());
		assertTrue(functionalBlockService.find(mergeParentUUID).isEmpty());
	}

	@DisplayName("Test Remove Delete Functional Block Computation For Merge Blocks When All Blocks Are Deleted")
	@Test
	void testRemoveDeleteFunctionalBlockComputationForMergeBlocks() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 12", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundMergeDeleted1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundMergeDeleted1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundMergeDeleted1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundMergeDeleted2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundMergeDeleted2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundMergeDeleted2", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "MergeDeleted", "Term", List.of(upperBoundModule1, upperBoundModule2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withTypes(List.of(FunctionalBlockType.REACHABILITY, FunctionalBlockType.RA_TOP_DOWN)));
		assertEquals(2, reachabilityBlock.size());
		assertTrue(reachabilityBlock.stream().allMatch(fb -> fb.getFlags().containsKey(FunctionalBlockFlag.OUTDATED.name())));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(0).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(1).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));

		final UUID mergeParentUUID = UUID.randomUUID();
		submitMergeFunctionalBlockJob(projectId1, mergeParentUUID, "MergeDeletedBlock", reachabilityBlock);

		final var mergeParentPojo1 = functionalBlockService.find(mergeParentUUID).orElseThrow();
		assertEquals(2, mergeParentPojo1.getChildren().size());
		assertFalse((boolean) mergeParentPojo1.getFlags().get(FunctionalBlockFlag.OUTDATED.name()));
		assertFalse((boolean) mergeParentPojo1.getFlags().get(FunctionalBlockFlag.DELETED.name()));

		final var result1 = mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE,
						projectId1.getNid(), mergeParentUUID))
				.andExpect(status().isOk());
		final DependencyGraph dependencyGraph1 = objectMapper.readValue(result1.andReturn().getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(6, dependencyGraph1.getModules().size());
		assertEquals(2, dependencyGraph1.getRootModuleIds().size());
		assertTrue(Set.of(upperBoundModule1.getId(), upperBoundModule2.getId()).containsAll(dependencyGraph1.getRootModuleIds()));
		assertEquals(4, dependencyGraph1.getReferences().size());
		assertEquals(2, dependencyGraph1.getRelationshipTypes().size());
		assertEquals(3, dependencyGraph1.getModuleTypes().size());

		moduleService.deleteModule(upperBoundModule2.identity(), true);
		moduleService.deleteModule(upperBoundModule1.identity(), true);

		final var allFunctionalBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());

		final var result2 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks1))).andExpect(status().isOk()).andReturn();
		final var jobId2 = result2.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId2, jobManager, 10L, TimeUnit.MINUTES);

		final var deletedBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(3, deletedBlocks1.size());
		assertEquals(Set.of("UpperBoundMergeDeleted2", "MergeDeletedBlock", "UpperBoundMergeDeleted1"),
				Set.of(deletedBlocks1.get(0).getName(), deletedBlocks1.get(1).getName(), deletedBlocks1.get(2).getName()));
		final var result3 = mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_DEPENDENCY_GRAPH_RETRIEVE,
						projectId1.getNid(), mergeParentUUID))
				.andExpect(status().isOk());

		/* Dependency graph should be empty as all the blocks are deleted */
		final DependencyGraph dependencyGraph2 = objectMapper.readValue(result3.andReturn().getResponse().getContentAsString(), DependencyGraph.class);
		assertTrue(dependencyGraph2.getModules().isEmpty());
		assertTrue(dependencyGraph2.getRootModuleIds().isEmpty());
		assertTrue(dependencyGraph2.getReferences().isEmpty());
		assertTrue(dependencyGraph2.getRelationshipTypes().isEmpty());
		assertTrue(dependencyGraph2.getModuleTypes().isEmpty());

		final var result4 = mvc.perform(delete("/api" + FunctionalBlockController.REMOVE_FUNCTIONAL_BLOCKS_WITH_NO_UPPER_MODULE_URL, projectId1.getNid())
				.contentType("application/json")).andExpect(status().isOk()).andReturn();
		final var jobId4 = result4.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId4, jobManager, 100L, TimeUnit.MINUTES);
		final var deletedBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, true));
		assertEquals(0, deletedBlocks2.size());
		final List<UUID> deletedChildBlocks = deletedBlocks1.stream().flatMap(block -> block.getChildren().stream()).collect(Collectors.toList());
		assertTrue(functionalBlockService.get(deletedChildBlocks).isEmpty());
		assertTrue(functionalBlockService.getLinks(mergeParentUUID).isEmpty());
		assertTrue(functionalBlockService.find(mergeParentUUID).isEmpty());
		final Map<UUID, List<FunctionalBlockLink>> functionalBlockLinks = functionalBlockService.getLinks(deletedBlocks1.stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList()));
		assertTrue(functionalBlockLinks.values().isEmpty());
	}

	@Test
	void testFunctionalBlockWhenModuleDependencyIsChanged() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 13", EntityId.of(1L));
		final ModulePojo upperBoundModule1 = createModule(projectId1, "UpperBoundDependencyModule1", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule1 = createModule(projectId1, "AccessBoundDependencyModule1", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule1 = createModule(projectId1, "LowerBoundDependencyModule1", Technology.RESOURCE, Type.FILE, null);

		final ModulePojo upperBoundModule2 = createModule(projectId1, "UpperBoundDependencyModule2", Technology.JCL, Type.JOB, null);
		final ModulePojo accessBoundModule2 = createModule(projectId1, "AccessBoundDependencyModule2", Technology.COBOL, Type.PROGRAM, null);
		final ModulePojo lowerBoundModule2 = createModule(projectId1, "LowerBoundDependencyModule2", Technology.RESOURCE, Type.FILE, null);
		final ModulePojo lowerBoundModule3 = createModule(projectId1, "LowerBoundDependencyModule3", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);
		moduleService.updateModuleDependencyHashes(projectId1);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "DependencyChanged", "Term", List.of(upperBoundModule1, upperBoundModule2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withTypes(List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)));
		assertEquals(2, reachabilityBlock.size());
		assertTrue(reachabilityBlock.stream().allMatch(fb -> fb.getFlags().containsKey(FunctionalBlockFlag.OUTDATED.name())));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(0).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));
		assertFalse(Boolean.parseBoolean(reachabilityBlock.get(1).getFlags().get(FunctionalBlockFlag.OUTDATED.name()).toString()));

		createReference(accessBoundModule1.identity(), lowerBoundModule3.identity(), RelationshipType.ACCESSES);
		moduleService.updateModuleDependencyHashes(projectId1);

		final var allFunctionalBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());
		final var result2 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks1))).andExpect(status().isOk()).andReturn();
		final var jobId2 = result2.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId2, jobManager, 10L, TimeUnit.MINUTES);

		final var outDatedBlocks1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(1, outDatedBlocks1.size());
		assertEquals("UpperBoundDependencyModule1", outDatedBlocks1.get(0).getName());

		final var result3 = mvc.perform(
				put("/api" + FunctionalBlockController.RECALCULATE_OUTDATED_FUNCTIONAL_BLOCKS_URL, projectId1.getNid())
						.contentType("application/json")).andExpect(status().isOk()).andReturn();
		final var jobId3 = result3.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId3, jobManager, 10L, TimeUnit.MINUTES);

		final var outDatedBlocks2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withType(FunctionalBlockType.RA_TOP_DOWN));
		assertEquals(0, outDatedBlocks2.size());
	}


	/*
	 *   rb1 ----------> rb2 -------> rb3
	 *    |                            |S
	 *    +------> rb4 ---S---> rb5 ---+----> rb6
	 */
	@Test
	void testReachabilityNetworkGraphWithFilterObject() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 14", EntityId.of(1L));
		final var rb1 = createFunctionalBlockPojo(projectId1, List.of(), List.of(), "reachabilityBlock1", "reachabilityBlock1",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name(), FunctionalBlockType.RA_TOP_DOWN.name())));
		final var rb2 = createFunctionalBlockPojo(projectId1, List.of(), List.of(), "reachabilityBlock2", "reachabilityBlock2",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name(), FunctionalBlockType.RA_TOP_DOWN.name())));
		final var rb3 = createFunctionalBlockPojo(projectId1, List.of(), List.of(), "reachabilityBlock3", "reachabilityBlock3",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name(), FunctionalBlockType.RA_TOP_DOWN.name()),
						FunctionalBlockFlag.STATUS.name(), FunctionalBlockStatus.INACTIVE.name()));
		final var rb4 = createFunctionalBlockPojo(projectId1, List.of(), List.of(), "reachabilityBlock4", "reachabilityBlock4",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name(), FunctionalBlockType.RA_TOP_DOWN.name())));
		final var rb5 = createFunctionalBlockPojo(projectId1, List.of(), List.of(), "reachabilityBlock5", "reachabilityBlock5",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name(), FunctionalBlockType.RA_TOP_DOWN.name())));
		final var rb6 = createFunctionalBlockPojo(projectId1, List.of(), List.of(), "reachabilityBlock6", "reachabilityBlock6",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY.name(), FunctionalBlockType.RA_TOP_DOWN.name())));

		final var networkLinks = List.of(new FunctionalBlockLink(null, null, rb1, rb2, null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
						List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name())), null),
				new FunctionalBlockLink(null, null, rb2, rb3, null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
						List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name())), null),
				new FunctionalBlockLink(null, null, rb1, rb4, null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
						List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name())), null),
				new FunctionalBlockLink(null, null, rb4, rb5, null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
						List.of(FunctionalBlockLinkType.RA_FROM_SCHEDULER_INFO.name())), null),
				new FunctionalBlockLink(null, null, rb5, rb6, null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
						List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name())), null),
				new FunctionalBlockLink(null, null, rb3, rb6, null, Map.of(FunctionalBlockLinkFlag.TYPE.name(),
						List.of(FunctionalBlockLinkType.RA_FROM_SCHEDULER_INFO.name())), null));

		final var networkBlock = createFunctionalBlockPojo(projectId1, List.of(rb1, rb2, rb3, rb4, rb5, rb6), networkLinks, "reachabilityNetwork", "reachabilityNetwork",
				Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name())));

		/* empty filter object */
		final MvcResult mvcResult = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("{}")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(6, dependencyGraph.getModules().size());
		assertTrue(dependencyGraph.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb1, rb2, rb3, rb4, rb5, rb6)));
		assertEquals(6, dependencyGraph.getRootModuleIds().size());
		assertTrue(dependencyGraph.getRootModuleIds().containsAll(dependencyGraph.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(6, dependencyGraph.getReferences().size());
		assertTrue(dependencyGraph.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb1, rb2), Pair.of(rb2, rb3), Pair.of(rb1, rb4), Pair.of(rb4, rb5), Pair.of(rb5, rb6), Pair.of(rb3, rb6))));

		/* filter object to exclude rb6 */
		final MvcResult mvcResult2 = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						 	"filterObject":	{
								"content_name": {
									"in": [
										"reachabilityBlock1",
										"reachabilityBlock2",
										"reachabilityBlock4",
										"reachabilityBlock5",
										"reachabilityBlock3"
									]
								}
							}
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph2 = objectMapper.readValue(mvcResult2.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(5, dependencyGraph2.getModules().size());
		assertTrue(dependencyGraph2.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb1, rb2, rb3, rb4, rb5)));
		assertEquals(5, dependencyGraph2.getRootModuleIds().size());
		assertTrue(dependencyGraph2.getRootModuleIds().containsAll(dependencyGraph2.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(4, dependencyGraph2.getReferences().size());
		assertTrue(dependencyGraph2.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb1, rb2), Pair.of(rb2, rb3), Pair.of(rb1, rb4), Pair.of(rb4, rb5))));

		/* filter object to exclude rb3 */
		final MvcResult mvcResult3 = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						 	"filterObject":	{
								"content_name": {
									"in": [
										"reachabilityBlock1",
										"reachabilityBlock2",
										"reachabilityBlock4",
										"reachabilityBlock5",
										"reachabilityBlock6"
									]
						        }
						    }
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph3 = objectMapper.readValue(mvcResult3.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(5, dependencyGraph3.getModules().size());
		assertTrue(dependencyGraph3.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb1, rb2, rb4, rb5, rb6)));
		assertEquals(5, dependencyGraph3.getRootModuleIds().size());
		assertTrue(dependencyGraph3.getRootModuleIds().containsAll(dependencyGraph3.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(5, dependencyGraph3.getReferences().size());
		assertTrue(dependencyGraph3.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb1, rb2), Pair.of(rb2, rb6), Pair.of(rb1, rb4), Pair.of(rb4, rb5), Pair.of(rb5, rb6))));

		/* filter object to exclude rb3 */
		final MvcResult excludeInactiveResult = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						  "filterObject": {
						    "content_status": {
						        "notEq": "INACTIVE"
						    }
						  }
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph excludeInactiveGraph = objectMapper.readValue(excludeInactiveResult.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(5, excludeInactiveGraph.getModules().size());
		assertTrue(excludeInactiveGraph.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb1, rb2, rb4, rb5, rb6)));
		assertEquals(5, excludeInactiveGraph.getRootModuleIds().size());
		assertTrue(excludeInactiveGraph.getRootModuleIds().containsAll(excludeInactiveGraph.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(4, excludeInactiveGraph.getReferences().size());
		assertTrue(excludeInactiveGraph.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb1, rb2), Pair.of(rb1, rb4), Pair.of(rb4, rb5), Pair.of(rb5, rb6))));

		/* filter object to exclude rb1 */
		final MvcResult mvcResult4 = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						 	"filterObject":	{
								"content_name": {
									"in": [
										"reachabilityBlock2",
										"reachabilityBlock3",
										"reachabilityBlock4",
										"reachabilityBlock5",
										"reachabilityBlock6"
									]
								}
						    }
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph4 = objectMapper.readValue(mvcResult4.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(5, dependencyGraph4.getModules().size());
		assertTrue(dependencyGraph4.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb2, rb3, rb4, rb5, rb6)));
		assertEquals(5, dependencyGraph4.getRootModuleIds().size());
		assertTrue(dependencyGraph4.getRootModuleIds().containsAll(dependencyGraph4.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(4, dependencyGraph4.getReferences().size());
		assertTrue(dependencyGraph4.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb2, rb3), Pair.of(rb3, rb6), Pair.of(rb4, rb5), Pair.of(rb5, rb6))));

		/* filter object to exclude rb2 and rb3 */
		final MvcResult mvcResult5 = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						 	"filterObject":	{
								"content_name": {
									"in": [
										"reachabilityBlock1",
										"reachabilityBlock4",
										"reachabilityBlock5",
										"reachabilityBlock6"
									]
						        }
						    }
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph5 = objectMapper.readValue(mvcResult5.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, dependencyGraph5.getModules().size());
		assertTrue(dependencyGraph5.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb1, rb4, rb5, rb6)));
		assertEquals(4, dependencyGraph5.getRootModuleIds().size());
		assertTrue(dependencyGraph5.getRootModuleIds().containsAll(dependencyGraph5.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(4, dependencyGraph5.getReferences().size());
		assertTrue(dependencyGraph5.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb1, rb4), Pair.of(rb1, rb6), Pair.of(rb4, rb5), Pair.of(rb5, rb6))));

		/* filter for type RA_FROM_SCHEDULER_INFO */
		final MvcResult mvcResult6 = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						    "functionalBlockLinkType": "RA_FROM_SCHEDULER_INFO"
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph6 = objectMapper.readValue(mvcResult6.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, dependencyGraph6.getModules().size());
		assertTrue(dependencyGraph6.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb4, rb5, rb6, rb3)));
		assertEquals(4, dependencyGraph6.getRootModuleIds().size());
		assertTrue(dependencyGraph6.getRootModuleIds().containsAll(dependencyGraph6.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(2, dependencyGraph6.getReferences().size());
		assertTrue(dependencyGraph6.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb3, rb6), Pair.of(rb4, rb5))));

		/* filter for type RA_SHARED_RESOURCE and exclude rb1 */
		final MvcResult mvcResult7 = mvc.perform(
				post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_NETWORK_GRAPH_RETRIEVE, projectId1.getNid(), networkBlock).contentType(
						"application/json").content("""
						{
						    "functionalBlockLinkType": "RA_SHARED_RESOURCE",
						    "filterObject": {
								"content_name": {
									"in": [
										"reachabilityBlock2",
										"reachabilityBlock3",
										"reachabilityBlock4",
										"reachabilityBlock5",
										"reachabilityBlock6"
									]
						        }
						    }
						}""")).andExpect(status().isOk()).andReturn();
		final DependencyGraph dependencyGraph7 = objectMapper.readValue(mvcResult7.getResponse().getContentAsString(), DependencyGraph.class);
		assertEquals(4, dependencyGraph7.getModules().size());
		assertTrue(dependencyGraph7.getModules().stream().map(ModulePojo::getUid).collect(Collectors.toSet()).containsAll(
				Set.of(rb2, rb3, rb5, rb6)));
		assertEquals(4, dependencyGraph7.getRootModuleIds().size());
		assertTrue(dependencyGraph7.getRootModuleIds().containsAll(dependencyGraph7.getModules().stream().map(ModulePojo::getId).collect(Collectors.toSet())));
		assertEquals(2, dependencyGraph7.getReferences().size());
		assertTrue(dependencyGraph7.getReferences().stream().map(r -> Pair.of(r.getSrcModule(), r.getDstModule())).collect(Collectors.toSet())
				.containsAll(Set.of(Pair.of(rb2, rb3), Pair.of(rb5, rb6))));
	}

	@Test
	void testUpdateFunctionalBlocksStatus() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Mock Project 15", EntityId.of(1L));
		final UUID functionalBlockPojo1 = createFunctionalBlockPojo(projectId1, null, null, "FunctionalBlockPojo1",
				"FunctionalBlockPojo1Description", Map.of(FunctionalBlockFlag.STATUS.name(), FunctionalBlockStatus.ACTIVE.name()));
		final UUID functionalBlockPojo2 = createFunctionalBlockPojo(projectId1, null, null, "FunctionalBlockPojo2",
				"FunctionalBlockPojo2Description", Map.of(FunctionalBlockFlag.STATUS.name(), FunctionalBlockStatus.ACTIVE.name()));
		createFunctionalBlockPojo(projectId1, null, null, "FunctionalBlockPojo3",
				"FunctionalBlockPojo3Description", null);

		mvc.perform(put("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_STATUS_UPDATE, projectId1.getNid(), FunctionalBlockStatus.INACTIVE)
								.contentType("application/json")
								.content(objectMapper.writeValueAsString(List.of(functionalBlockPojo1, functionalBlockPojo2))))
				.andExpect(status().isOk())
				.andReturn();

		final var inActiveFunctionalBlockList = functionalBlockService.find(q -> q.ofProject(projectId1).withStatus(FunctionalBlockStatus.INACTIVE));
		assertEquals(2, inActiveFunctionalBlockList.size());
		assertEquals(Set.of(functionalBlockPojo1, functionalBlockPojo2),
				inActiveFunctionalBlockList.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet()));
		assertTrue(inActiveFunctionalBlockList.stream().allMatch(fb -> fb.getFlags().containsKey(FunctionalBlockFlag.STATUS.name())));
		assertTrue(inActiveFunctionalBlockList.stream().allMatch(fb ->
				FunctionalBlockStatus.INACTIVE.name().equals(fb.getFlags().get(FunctionalBlockFlag.STATUS.name()))));

		mvc.perform(put("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_STATUS_UPDATE, projectId1.getNid(), FunctionalBlockStatus.ACTIVE)
						.contentType("application/json")
						.content(objectMapper.writeValueAsString(List.of(functionalBlockPojo1))))
				.andExpect(status().isOk())
				.andReturn();
		final var activeFunctionalBlockList = functionalBlockService.find(q -> q.ofProject(projectId1).notWithStatus(FunctionalBlockStatus.INACTIVE));
		assertEquals(2, activeFunctionalBlockList.size());
		assertFalse(activeFunctionalBlockList.stream().allMatch(fb -> fb.getFlags().containsKey(FunctionalBlockFlag.STATUS.name())));
	}

	@Test
	void testShouldThrowExceptionWhenChildrenAreNotPresentInSameProjectAsParentForCreate() throws Exception {
		final EntityId project1 = loadProjectAndClient("Test Same Project 1", EntityId.of(1L));
		final EntityId project2 = loadProjectAndClient("Test Same Project 2", EntityId.of(1L));

		final UUID child1 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(project1)
				.setDescription("")
				.setName("Child 1"));

		final UUID child2 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(project1)
				.setDescription("")
				.setName("Child 2"));

		final FunctionalBlockPojoPrototype parent = new FunctionalBlockPojoPrototype()
				.setProject(project2)
				.setDescription("")
				.setName("Parent")
				.setChildren(List.of(child1, child2));

		mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCKS_COLLECTION_BY_PROJECT_URL, projectId.getUid())
				.contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(parent)))
				.andExpect(status().isBadRequest());
	}

	@Test
	void testShouldThrowExceptionWhenChildrenAreNotPresentInSameProjectAsParentForUpdate() throws Exception {
		final EntityId project1 = loadProjectAndClient("Test Same Project 3", EntityId.of(1L));
		final EntityId project2 = loadProjectAndClient("Test Same Project 4", EntityId.of(1L));

		final UUID child1 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(project1)
				.setDescription("")
				.setName("Child 1"));

		final UUID child2 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(project1)
				.setDescription("")
				.setName("Child 2"));

		final UUID parent = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(project2)
				.setDescription("")
				.setName("Parent"));

		final FunctionalBlockPojoPrototype parentUpdated = new FunctionalBlockPojoPrototype()
				.setUid(parent)
				.setChildren(List.of(child1, child2));

		mvc.perform(put("/api" + FunctionalBlockController.FUNCTIONAL_BLOCKS_BY_UID_URL, projectId.getUid(), parent)
						.contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(parentUpdated)))
				.andExpect(status().isBadRequest());
	}
	
	@Test
	void testEmptyAutoFbDeletion() throws Exception {

		final UUID child1 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(projectId)
				.setDescription("Functional type child")
				.setName("Child").setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT.name()))));

		final EntityId annotationId = createAnnotation("Sample Name");

		functionalBlockService.setGeneratedFrom(child1, GeneratedFrom.fromAnnotation(annotationId));
		
		final UUID parent = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(projectId)
				.setDescription("")
				.setName("Parent").setChildren(List.of(child1)));

		final FunctionalBlockPojoPrototype parentUpdated = new FunctionalBlockPojoPrototype()
				.setUid(parent)
				.setChildren(new ArrayList<>());

		mvc.perform(put("/api" + FunctionalBlockController.FUNCTIONAL_BLOCKS_BY_UID_URL, projectId.getUid(), parent)
						.contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(parentUpdated)))
				.andExpect(status().isOk());
		assertTrue(functionalBlockService.find(child1).isPresent());
		mvc.perform(delete("/api" + AnnotationToFunctionalBlockController.DELETE_NO_PARENT_FUNCTIONAL_TYPE_BLOCKS, projectId.getUid(), AnnotationType.FUNCTIONAL)
						.contentType("application/json"))
				.andExpect(status().isNoContent()).andReturn();
		assertTrue(functionalBlockService.find(child1).isEmpty());

	}

	@Test
	void testReachabilityAnalysisOnUnSupportedUpperBoundOrLowerBound() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Test Reachability Analysis On Unsupported Upper Bound Or Lower Bound", EntityId.of(1L));
		final var upperBoundModule1 = createModule(projectId1, "RAUpperBoundModule1", Technology.COBOL, Type.PROGRAM, null);
		final var upperBoundModule2 = createModule(projectId1, "RAUpperBoundModule2", Technology.JCL, Type.JOB, null);
		final var upperBoundModule3 = createModule(projectId1, "RAUpperBoundModule3", Technology.JCL, Type.JOB, null);
		final var accessBoundModule1 = createModule(projectId1, "RAAccessBoundModule1", Technology.COBOL, Type.PROGRAM, null);
		final var accessBoundModule2 = createModule(projectId1, "RAAccessBoundModule2", Technology.COBOL, Type.PROGRAM, null);
		final var accessBoundModule3 = createModule(projectId1, "RAAccessBoundModule3", Technology.COBOL, Type.PROGRAM, null);
		final var lowerBoundModule1 = createModule(projectId1, "RALowerBoundModule1", Technology.RESOURCE, Type.FILE, null);
		final var lowerBoundModule2 = createModule(projectId1, "RALowerBoundModule2", Technology.RESOURCE, Type.FILE, null);
		final var lowerBoundModule3 = createModule(projectId1, "RALowerBoundModule3", Technology.SQL, Type.VIEW, null);

		createReference(upperBoundModule1.identity(), accessBoundModule1.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule2.identity(), accessBoundModule2.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.ACCESSES);
		createReference(upperBoundModule3.identity(), accessBoundModule3.identity(), RelationshipType.CALLS);
		createReference(accessBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.ACCESSES);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "Unsupported", "Term",
				List.of(upperBoundModule1, upperBoundModule2, upperBoundModule3));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withTypes(List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)));
		assertEquals(2, reachabilityBlock.size());
		assertEquals(Set.of("RAUpperBoundModule2", "RAUpperBoundModule3"),
				reachabilityBlock.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));

		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlocks(reachabilityBlock.stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList())));
		assertEquals(2, reachabilityData.size());
		assertEquals(Set.of(upperBoundModule2.identity(), upperBoundModule3.identity()), reachabilityData.stream()
				.map(ReachabilityDataPojo::getUpperBoundModuleId).collect(Collectors.toSet()));
		assertEquals(Set.of(accessBoundModule2.identity()), reachabilityData.stream().flatMap(pojo -> pojo.getAccessModuleIds().stream())
				.collect(Collectors.toSet()));
		assertEquals(Set.of(lowerBoundModule2.identity()), reachabilityData.stream().map(ReachabilityDataPojo::getLowerBoundModuleId)
				.filter(Optional::isPresent).map(Optional::get).collect(Collectors.toSet()));
	}

	@Test
	void testTopDownReachabilityAnalysisOnDefaultType() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Test Reachability Analysis On Default Type", EntityId.of(1L));
		final var upperBoundModule1 = createModule(projectId1, "UpperBoundModule1", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule1 = createModule(projectId1, "LowerBoundModule1", Technology.RESOURCE, Type.FILE, null);
		final var upperBoundModule2 = createModule(projectId1, "UpperBoundModule2", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule2 = createModule(projectId1, "LowerBoundModule2", Technology.RESOURCE, Type.FILE, null);
		final var upperBoundModule3 = createModule(projectId1, "UpperBoundModule3", Technology.COBOL, Type.PROGRAM, null);
		final var lowerBoundModule3 = createModule(projectId1, "LowerBoundModule3", Technology.RESOURCE, Type.FILE, null);

		createReference(upperBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.CALLS);

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Collections.emptySet(), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withTypes(List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)));
		assertEquals(2, reachabilityBlock.size());
		assertEquals(Set.of("UpperBoundModule1", "UpperBoundModule2"), reachabilityBlock.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));

		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlocks(reachabilityBlock.stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList())));
		assertEquals(2, reachabilityData.size());
		assertEquals(Set.of(upperBoundModule1.identity(), upperBoundModule2.identity()), reachabilityData.stream().map(ReachabilityDataPojo::getUpperBoundModuleId).collect(
				Collectors.toSet()));
		assertEquals(Set.of(lowerBoundModule1.identity(), lowerBoundModule2.identity()), reachabilityData.stream().map(ReachabilityDataPojo::getLowerBoundModuleId).filter(Optional::isPresent)
				.map(Optional::get).collect(Collectors.toSet()));
	}

	@Test
	void testReachabilityTopDownAnalysisOnTaxonomiesAndType() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Test Reachability Analysis On Default Type And Taxonomies", EntityId.of(1L));
		final var upperBoundModule1 = createModule(projectId1, "UpperBoundModule1", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule1 = createModule(projectId1, "LowerBoundModule1", Technology.IMS, Type.MFS, null);
		final var upperBoundModule2 = createModule(projectId1, "UpperBoundModule2", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule2 = createModule(projectId1, "LowerBoundModule2", Technology.IMS, Type.DBD, null);
		final var upperBoundModule3 = createModule(projectId1, "UpperBoundModule3", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule3 = createModule(projectId1, "LowerBoundModule3", Technology.CICS, Type.BMS_MAP, null);

		createReference(upperBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.CALLS);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "Taxonomies", "Test", List.of(upperBoundModule1));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withTypes(List.of(FunctionalBlockType.RA_TOP_DOWN, FunctionalBlockType.REACHABILITY)));
		assertEquals(1, reachabilityBlock.size());
		assertEquals(Set.of("UpperBoundModule1"), reachabilityBlock.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));

		final var reachabilityData = functionalBlockService.findReachabilityData(q -> q.ofFunctionalBlocks(reachabilityBlock.stream()
				.map(FunctionalBlockPojo::getUid).collect(Collectors.toList())));
		assertEquals(1, reachabilityData.size());
		assertEquals(Set.of(upperBoundModule1.identity()), reachabilityData.stream().map(ReachabilityDataPojo::getUpperBoundModuleId).collect(
				Collectors.toSet()));
		assertEquals(Set.of(lowerBoundModule1.identity()), reachabilityData.stream().map(ReachabilityDataPojo::getLowerBoundModuleId).filter(Optional::isPresent)
				.map(Optional::get).collect(Collectors.toSet()));
	}

	@Test
	void testBottomUpReachabilityAnalysisOnDefaultType() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Test Bottom Up Analysis On Default Type", EntityId.of(1L));
		final var upperBoundModule1 = createModule(projectId1, "UpperBoundModule1", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule1 = createModule(projectId1, "LowerBoundModule1", Technology.CICS, Type.TDQ, null);
		final var upperBoundModule2 = createModule(projectId1, "UpperBoundModule2", Technology.SQL, Type.STORED_PROCEDURE, null);
		final var lowerBoundModule2 = createModule(projectId1, "LowerBoundModule2", Technology.CICS, Type.TSQ, null);
		final var upperBoundModule3 = createModule(projectId1, "UpperBoundModule3", Technology.SERVICE, Type.SERVICE_REQUEST_ID, null);
		final var lowerBoundModule3 = createModule(projectId1, "LowerBoundModule3", Technology.RESOURCE, Type.GDG_FILE, null);

		createReference(upperBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.CALLS);

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.BOTTOM_UP,
				Collections.emptySet(), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(6, reachabilityBlock.size());
		assertEquals(3, reachabilityBlock.stream().filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_BOTTOM_UP)).count());
		assertEquals(3, reachabilityBlock.stream().filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_TOP_DOWN)).count());
	}

	@Test
	void testReachabilityBottomUpAnalysisOnTaxonomiesAndType() throws Exception {
		final EntityId projectId1 = loadProjectAndClient("Test Reachability Analysis On Default Types And Taxonomies", EntityId.of(1L));
		final var upperBoundModule1 = createModule(projectId1, "UpperBoundModule1", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule1 = createModule(projectId1, "LowerBoundModule1", Technology.RESOURCE, Type.FILE, null);
		final var upperBoundModule2 = createModule(projectId1, "UpperBoundModule2", Technology.ECL, Type.ECL_JOB, null);
		final var lowerBoundModule2 = createModule(projectId1, "LowerBoundModule2", Technology.RESOURCE, Type.VSAM_FILE, null);
		final var upperBoundModule3 = createModule(projectId1, "UpperBoundModule3", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule3 = createModule(projectId1, "LowerBoundModule3", Technology.SQL, Type.TABLE, null);

		createReference(upperBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.CALLS);

		final EntityId taxonomyId = createAndAssignTaxonomies(projectId1, "BottomUp", "Test", List.of(lowerBoundModule1, lowerBoundModule2));
		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.BOTTOM_UP,
						Set.of(taxonomyId), Collections.emptySet(), false));

		final var reachabilityBlock = functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(4, reachabilityBlock.size());
		assertEquals(2, reachabilityBlock.stream().filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_BOTTOM_UP)).count());
		assertEquals(Set.of("LowerBoundModule1", "LowerBoundModule2"), reachabilityBlock.stream()
				.filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_BOTTOM_UP)).map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));
		assertEquals(2, reachabilityBlock.stream().filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_TOP_DOWN)).count());
		assertEquals(Set.of("UpperBoundModule1", "UpperBoundModule2"), reachabilityBlock.stream()
				.filter(fb -> FunctionalBlockUtil.hasType(fb, FunctionalBlockType.RA_TOP_DOWN)).map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));
	}

	@Test
	void testRecalculateOnSelectedFunctionalBlocks() throws Exception {
		final var projectId1 = loadProjectAndClient("Test Recalculate On Selected Functional Blocks", EntityId.of(1L));
		var upperBoundModule1 = createModule(projectId1, "UpperBoundModule1", Technology.JCL, Type.JOB, null);
		final var lowerBoundModule1 = createModule(projectId1, "LowerBoundModule1", Technology.RESOURCE, Type.FILE, null);
		final var upperBoundModule2 = createModule(projectId1, "UpperBoundModule2", Technology.ECL, Type.ECL_JOB, null);
		final var lowerBoundModule2 = createModule(projectId1, "LowerBoundModule2", Technology.RESOURCE, Type.VSAM_FILE, null);
		final var upperBoundModule3 = createModule(projectId1, "UpperBoundModule3", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule3 = createModule(projectId1, "LowerBoundModule3", Technology.SQL, Type.TABLE, null);
		final var upperBoundModule4 = createModule(projectId1, "UpperBoundModule4", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule4 = createModule(projectId1, "LowerBoundModule4", Technology.SQL, Type.TABLE, null);
		var upperBoundModule5 = createModule(projectId1, "UpperBoundModule5", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule5 = createModule(projectId1, "LowerBoundModule5", Technology.SQL, Type.TABLE, null);
		final var upperBoundModule6 = createModule(projectId1, "UpperBoundModule6", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule6 = createModule(projectId1, "LowerBoundModule6", Technology.SQL, Type.TABLE, null);
		var upperBoundModule7 = createModule(projectId1, "UpperBoundModule7", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule7 = createModule(projectId1, "LowerBoundModule7", Technology.SQL, Type.TABLE, null);
		final var upperBoundModule8 = createModule(projectId1, "UpperBoundModule8", Technology.JAVA, Type.TYPE, null);
		final var lowerBoundModule8 = createModule(projectId1, "LowerBoundModule8", Technology.SQL, Type.TABLE, null);

		createReference(upperBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule2.identity(), lowerBoundModule2.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule3.identity(), lowerBoundModule3.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule4.identity(), lowerBoundModule4.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule5.identity(), lowerBoundModule5.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule6.identity(), lowerBoundModule6.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule7.identity(), lowerBoundModule7.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule8.identity(), lowerBoundModule8.identity(), RelationshipType.CALLS);

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Collections.emptySet(), Collections.emptySet(), false));
		submitMergeFunctionalBlockJob(projectId1, UUID.randomUUID(), "MergedBlock1",
				functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY)
						.withNames(List.of("UpperBoundModule1", "UpperBoundModule2"))));
		submitMergeFunctionalBlockJob(projectId1, UUID.randomUUID(), "MergedBlock2",
				functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY)
						.withNames(List.of("UpperBoundModule3", "UpperBoundModule4"))));
		submitMergeFunctionalBlockJob(projectId1, UUID.randomUUID(), "MergedBlock3",
				functionalBlockService.find(q -> q.ofProject(projectId1).withType(FunctionalBlockType.REACHABILITY)
						.withNames(List.of("UpperBoundModule5", "UpperBoundModule6"))));

		/* Updating the Upper bound module and check whether the Block is outdated or not */
		moduleService.deleteModules(q -> q.ofProject(projectId1).withNames(List.of("UpperBoundModule1",
				"UpperBoundModule3", "UpperBoundModule4", "UpperBoundModule5", "UpperBoundModule7", "UpperBoundModule8")));
		upperBoundModule1 = createModule(projectId1, "UpperBoundModule1", Technology.JCL, Type.JOB, "7777771");
		upperBoundModule5 = createModule(projectId1, "UpperBoundModule5", Technology.JAVA, Type.TYPE, "7777773");
		upperBoundModule7 = createModule(projectId1, "UpperBoundModule7", Technology.JAVA, Type.TYPE, "7777774");

		createReference(upperBoundModule1.identity(), lowerBoundModule1.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule5.identity(), lowerBoundModule5.identity(), RelationshipType.CALLS);
		createReference(upperBoundModule7.identity(), lowerBoundModule7.identity(), RelationshipType.CALLS);

		final var allFunctionalBlocks = functionalBlockService.find(q -> q.ofProject(projectId1))
				.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet());
		final var result1 = mvc.perform(
				post("/api" + FunctionalBlockController.COMPUTE_FUNCTIONAL_BLOCKS_URL, projectId1.getNid()).contentType("application/json")
						.content(objectMapper.writeValueAsString(allFunctionalBlocks))).andExpect(status().isOk()).andReturn();
		final var jobId1 = result1.getResponse().getContentAsString().replace("\"", "");
		JobTestHelper.waitForJobCompletion(jobId1, jobManager, 10L, TimeUnit.MINUTES);

		final List<FunctionalBlockPojo> outdatedBlock1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withFlag(FunctionalBlockFlag.DELETED, false)
				.withType(FunctionalBlockType.REACHABILITY));
		assertEquals(5, outdatedBlock1.size());
		assertEquals(Set.of("MergedBlock1", "MergedBlock3", "UpperBoundModule1", "UpperBoundModule5", "UpperBoundModule7"),
				outdatedBlock1.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));

		final List<FunctionalBlockPojo> deletedBlock1 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.DELETED, true).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(4, deletedBlock1.size());
		assertEquals(Set.of("MergedBlock2", "UpperBoundModule3", "UpperBoundModule4", "UpperBoundModule8"),
				deletedBlock1.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));

		final Set<EntityId> selectedBlock1 = functionalBlockService.find(q -> q.ofProject(projectId1)
						.withFlag(FunctionalBlockFlag.OUTDATED, true).withFlag(FunctionalBlockFlag.DELETED, false)
						.withParent(p -> p.notWithType(FunctionalBlockType.MERGE_PARENT))).stream()
				.map(FunctionalBlockPojo::identity).collect(Collectors.toSet());

		reachabilityAnalysisJob(projectId1, new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN,
				Collections.emptySet(), selectedBlock1, true));

		/* since recalculate has been done only on outdated module so it should be zero*/
		final List<FunctionalBlockPojo> outdatedBlock2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.OUTDATED, true).withFlag(FunctionalBlockFlag.DELETED, false)
				.withType(FunctionalBlockType.REACHABILITY));
		assertEquals(0, outdatedBlock2.size());

		final List<FunctionalBlockPojo> deletedBlock2 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.DELETED, true).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(4, deletedBlock2.size());
		assertEquals(Set.of("MergedBlock2", "UpperBoundModule3", "UpperBoundModule4", "UpperBoundModule8"),
				deletedBlock1.stream().map(FunctionalBlockPojo::getName).collect(Collectors.toSet()));

		/* recalulation only on the deleted functional Blocks */
		final Set<EntityId> selectedBlock2 = functionalBlockService.find(q -> q.ofProject(projectId1)
						.withNames(List.of("MergedBlock2", "UpperBoundModule8"))
						.withType(FunctionalBlockType.REACHABILITY)).stream()
				.map(FunctionalBlockPojo::identity).collect(Collectors.toSet());

		reachabilityAnalysisJob(projectId1,
				new ReachabilityAnalysisRequest(ReachabilityAnalysisRequest.AnalysisType.TOP_DOWN, Collections.emptySet(), selectedBlock2, true));
		final List<FunctionalBlockPojo> deletedBlock3 = functionalBlockService.find(q -> q.ofProject(projectId1)
				.withFlag(FunctionalBlockFlag.DELETED, true).withType(FunctionalBlockType.REACHABILITY));
		assertEquals(0, deletedBlock3.size());
	}

	@Test
	void testDeleteFbOnUnGroup() throws Exception {

		final UUID child1 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(projectId)
				.setDescription("Functional type child")
				.setName("Child").setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT.name()))));

		final EntityId annotationId = createAnnotation("Sample Name");

		functionalBlockService.setGeneratedFrom(child1, GeneratedFrom.fromAnnotation(annotationId));
		
		final UUID subParent = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(projectId)
				.setDescription("")
				.setName("Parent").setChildren(List.of(child1)));

		final UUID child2 = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(projectId)
				.setDescription("Functional type child2")
				.setName("Child2").setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_UNIT.name()))));

		final EntityId annotationId2 = createAnnotation("Sample Name2");

		functionalBlockService.setGeneratedFrom(child2, GeneratedFrom.fromAnnotation(annotationId2));
		
		
		final UUID parent = functionalBlockService.create(new FunctionalBlockPojoPrototype()
				.setProject(projectId)
				.setDescription("")
				.setName("Parent").setChildren(List.of(subParent, child2)));

		mvc.perform(delete("/api" + FunctionalBlockController.UNGROUP_FUNCTIONAL_BLOCKS_URL, projectId.getUid(), parent, subParent)
						.contentType("application/json"))
				.andExpect(status().isNoContent()).andReturn();
		assertTrue(functionalBlockService.find(subParent).isEmpty(), "subParent must be deleted");
		final Optional<FunctionalBlockPojo> parentPojo = functionalBlockService.find(parent);
		assertTrue(parentPojo.isPresent(), "parent must exist");
		assertEquals(2, parentPojo.get().getChildren().size(), "parent must contain 2 children");
		assertTrue(parentPojo.get().getChildren().contains(child1), "parent must contain child1 in its children");
		assertTrue(parentPojo.get().getChildren().contains(child2), "parent must contain child2 in its children");
	}
	
	private void reachabilityAnalysisJob(final EntityId projectId, final ReachabilityAnalysisRequest reachabilityAnalysisRequest) throws Exception {
		final var result = mvc.perform(
						post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_GENERATION_REACHABILITY_ANALYSIS, projectId.getNid())
								.contentType("application/json").content(objectMapper.writeValueAsString(reachabilityAnalysisRequest)))
				.andExpect(status().isOk()).andReturn();
		final var jobId = result.getResponse().getContentAsString().replace("\"", "");
		final var job = jobManager.getJobs(q -> q.byId(UUID.fromString(jobId)))
				.stream().findAny().orElseThrow();
		JobTestHelper.waitForJobCompletion(jobId, jobManager, 10L, TimeUnit.MINUTES);
		final var computationJob1 = JobTestHelper.findJobByLastSubmitTime(jobManager, "Functional Block Computation",
				Objects.requireNonNull(job.getSubmitTime()), 10L).orElseThrow();
		JobTestHelper.waitForJobCompletion(computationJob1.getJobId(), jobManager, 10L, TimeUnit.MINUTES);

		if (reachabilityAnalysisRequest.getAnalysisType() == ReachabilityAnalysisRequest.AnalysisType.BOTTOM_UP) {
			final List<JobInformation> jobInformation = jobManager.getJobs(q -> q.withStartTime(Comperator.GREATER_OR_EQUAL,
					Objects.requireNonNull(computationJob1.getStartTime())));
			jobInformation.forEach(jobInfo -> JobTestHelper.waitForJobCompletion(jobInfo.getJobId(), jobManager, 10L, TimeUnit.MINUTES));

			final var computationJob2 = JobTestHelper.findJobByLastSubmitTime(jobManager, "Functional Block Computation",
					computationJob1.getSubmitTime(), 10L).orElseThrow();
			JobTestHelper.waitForJobCompletion(computationJob2.getJobId(), jobManager, 10L, TimeUnit.MINUTES);
		}
	}

	private void submitMergeFunctionalBlockJob(final EntityId project, final UUID mergeParentUUID, final String mergeBlockName,
			final List<FunctionalBlockPojo> reachabilityBlock) throws Exception {
		final Instant startTime = Instant.now();
		final var reachabilityBlockNetwork = functionalBlockService.find(q -> q.ofProject(project).withType(FunctionalBlockType.REACHABILITY_NETWORK));
		final var request = new FunctionalBlockMergeRequest(reachabilityBlockNetwork.get(0).getUid(), null, new FunctionalBlockPojoPrototype()
				.setUid(mergeParentUUID).setProject(project).setName(mergeBlockName)
				.setDescription(mergeBlockName + "Description")
				.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.RA_TOP_DOWN.name(), FunctionalBlockType.REACHABILITY.name()))),
				reachabilityBlock.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toList()), false);
		mvc.perform(post("/api" + FunctionalBlockController.FUNCTIONAL_BLOCK_MERGE, project.getUid())
						.contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(request)))
				.andExpect(status().isOk());
		final var computationJob = JobTestHelper.findJobByLastSubmitTime(jobManager, "Functional Block Computation", startTime, 10L).orElseThrow();
		JobTestHelper.waitForJobCompletion(computationJob.getJobId(), jobManager, 10L, TimeUnit.MINUTES);
	}

	private EntityId createAndAssignTaxonomies(final EntityId projectId, final String typeName, final String taxonomyName, final List<ModulePojo> modules) {
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setName(typeName)
				.setProject(projectId));
		final var createdTaxonomy = taxonomyService.create(new TaxonomyPojoPrototype()
				.setName(taxonomyName)
				.setType(type)
				.setProject(projectId));

		modules.stream().map(ModulePojo::getUid).forEach(module -> taxonomyService.createModuleLink(module, createdTaxonomy));
		return createdTaxonomy;
	}

	private ModuleRelationshipPojo createReference(final EntityId fromRid, final EntityId toRid, final RelationshipType relationshipType,
			final Map<String, Object> attributes) {
		final int fromOffset = 0;
		final int fromLength = 42;
		final int toOffset = 666;
		final int toLength = 1337;
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype();
		reference.setRelationship(relationshipType);
		reference.setSrcModule(fromRid);
		reference.setDstModule(toRid);
		reference.setSrcLocation(new ModuleLocation(fromOffset, fromLength));
		reference.setDstLocation(new ModuleLocation(toOffset, toLength));
		reference.setProperties(attributes);
		try {
			reference.setDependencyAttributes(objectMapper.writeValueAsString(attributes));
		} catch (final JsonProcessingException e) {
			throw new IllegalStateException(e);
		}
		return moduleService.getRelationship(moduleService.createRelationship(reference));
	}

	private ModuleRelationshipPojo createReference(final EntityId fromRid, final EntityId toRid, final RelationshipType relationshipType) {
		return createReference(fromRid, toRid, relationshipType, Map.of());
	}

	private ModulePojo createModule(final EntityId projectId, final String name, final Technology technology,
			final Type type, @Nullable final String contentHash) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(name);
		module.setTechnology(technology);
		module.setType(type);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.MISSING);
		module.setPath("src/cobol/TESTCOBA" + name + ".cbl");
		module.setCreator(Creator.DISCOVERY);
		module.setLinkHash("1111111");
		module.setContentHash(new BinaryValue(CityHash.cityHash128(ByteOrder.BIG_ENDIAN,
				Objects.requireNonNullElse(contentHash, "7777777").getBytes(StandardCharsets.UTF_8))));
		return moduleService.getModule(moduleService.create(module));
	}

	private EntityId loadProjectAndClient(final String name, final EntityId clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(clientId)
				.setNatures(new HashSet<>(Collections.emptySet()))
		).identity();
	}

	private UUID createFunctionalBlocks() {
		final List<UUID> childUIDs = new ArrayList<>();
		final Map<String, Object> flag1 = new HashMap<>();
		flag1.put(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.MODULE);
		final Map<String, Object> flag2 = new HashMap<>();
		flag2.put(FunctionalBlockFlag.RA_ACCESS_TYPE.name(), "[WRITE]");
		flag2.put(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.MODULE);
		flag2.put(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.RA_UPPER_BOUND);
		final Map<String, Object> flag3 = new HashMap<>();
		final List<String> values = new ArrayList<>();
		values.add("[READ]");
		values.add("[WRITE]");
		flag3.put(FunctionalBlockFlag.RA_ACCESS_TYPE.name(), values);
		flag3.put(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.MODULE);
		flag3.put(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.RA_UPPER_BOUND);


		final UUID functionalblockUID3 = createFunctionalBlockPojo(projectId, null, null,
				"Test Unit  1", "",
				flag1);
		final UUID functionalblockUID4 = createFunctionalBlockPojo(projectId, List.of(functionalblockUID3),
				null, "Test Unit  2" , "",
				flag2);
		final UUID functionalblockUID5 = createFunctionalBlockPojo(projectId,
				List.of(functionalblockUID3, functionalblockUID4),
				null, "Test Unit  3" , "",
				flag3);

		final UUID functionalblockUID6 = createFunctionalBlockPojo(projectId,
				List.of(functionalblockUID5, functionalblockUID4),
				null, "Test Unit  3" , "",
				flag3);


		childUIDs.add(functionalblockUID3);
		childUIDs.add(functionalblockUID4);
		childUIDs.add(functionalblockUID5);
		childUIDs.add(functionalblockUID6);

		final UUID peerFunctionalBlock1 = createFunctionalBlockPojo(projectId, null, null, "Peer Unit Parent " ,
				"PeerBlock1", Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_GROUP.name()));
		final UUID peerFunctionalBlock2 = createFunctionalBlockPojo(projectId, null, null, "Peer Unit Parent 1 " ,
				"PeerBlock2", Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_GROUP.name()));
		final UUID peerFunctionalBlock3 = createFunctionalBlockPojo(projectId, null, null, "Peer Unit Parent 2 " ,
				"PeerBlock3", Map.of(FunctionalBlockFlag.TYPE.name(), FunctionalBlockType.FUNCTIONAL_GROUP.name()));
		functionalBlockService.setResolvedModuleParts(peerFunctionalBlock1, List.of(new ResolvedModulePart(EntityId.of(module1.getUid()))));
		functionalBlockService.setResolvedModuleParts(peerFunctionalBlock2, List.of(new ResolvedModulePart(EntityId.of(module1.getUid()))));
		functionalBlockService.setResolvedModuleParts(peerFunctionalBlock3, List.of(new ResolvedModulePart(EntityId.of(module2.getUid()))));

		final ModuleRelationshipPojo relationshipPojo1 = createReference(module1.identity(), module2.identity(), RelationshipType.CALLS);
		final ModuleRelationshipPojo relationshipPojo2 = createReference(module3.identity(), module4.identity(), RelationshipType.INCLUDES);

		final UUID functionalblockUID1 = createFunctionalBlockPojo(projectId, childUIDs,
				List.of(createLink(childUIDs.get(0), childUIDs.get(1), UUID.randomUUID(), relationshipPojo1.getRelationship()),
						createLink(childUIDs.get(2), childUIDs.get(3), UUID.randomUUID(), relationshipPojo2.getRelationship())), "Test Group 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.RA_ACCESS_MODULE, FunctionalBlockType.CALL_CHAIN)));
		createFunctionalBlockPojo(projectId, List.of(functionalblockUID1),
				null, "Test Unit Parent " , "", flag2);
		createFunctionalBlockPojo(projectId, List.of(functionalblockUID3),
				null, "Test Unit Parent 1 " , "", flag2);
		functionalBlockService.setGeneratedFrom(childUIDs.get(0), GeneratedFrom.fromModule(module1.getLinkHash(), module1.getContentHash()
				.orElseThrow(() -> new IllegalStateException("Module1 content hash must be present")).toString(), null));
		functionalBlockService.setGeneratedFrom(childUIDs.get(1), GeneratedFrom.fromModule(module2.getLinkHash(), module2.getContentHash()
				.orElseThrow(() -> new IllegalStateException("Module1 content hash must be present")).toString(), null));
		functionalBlockService.setGeneratedFrom(childUIDs.get(2), GeneratedFrom.fromModule(module3.getLinkHash(), module3.getContentHash()
				.orElseThrow(() -> new IllegalStateException("Module1 content hash must be present")).toString(), null));
		functionalBlockService.setGeneratedFrom(childUIDs.get(3), GeneratedFrom.fromModule(module4.getLinkHash(), module4.getContentHash()
				.orElseThrow(() -> new IllegalStateException("Module1 content hash must be present")).toString(), null));

		return functionalblockUID1;
	}

	private FunctionalBlockLink createLink(final UUID child1, final UUID child2, final UUID parent, final RelationshipType relationshipBetweenChildAAndB) {
		return new FunctionalBlockLink(UUID.randomUUID(), parent, child1, child2, null,
				Map.of(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), List.of(relationshipBetweenChildAAndB)), null);
	}

	private UUID createFunctionalBlockPojo(
			final EntityId project,
			final @Nullable List<UUID> children,
			final @Nullable List<FunctionalBlockLink> links,
			final String name,
			final String description,
			final @Nullable Map<String, Object> flags) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(description);

		if (children != null) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		final UUID uuid = functionalBlockService.create(functionalBlockPojoPrototype);
		if (links != null) {
			functionalBlockService.setLinks(uuid, links);
		}
		return uuid;
	}

	private EntityId createAnnotation(final String name) {
		final ModuleLocation dummyLocation = new ModuleLocation();
		dummyLocation.setOffset(Integer.valueOf(1));
		dummyLocation.setLength(Integer.valueOf(1));
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setType(AnnotationType.FUNCTIONAL);
		annotation.setState(WorkingState.CANDIDATE);
		annotation.setSourceAttachment("Content");
		annotation.setLocation(dummyLocation);
		annotation.setCreatedByUserId("1");
		annotation.setModule(cobolModule.identity());
		return annotationService.create(annotation);
	}
}

