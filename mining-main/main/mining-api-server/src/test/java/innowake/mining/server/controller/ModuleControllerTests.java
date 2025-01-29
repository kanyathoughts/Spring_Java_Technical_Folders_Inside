/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.controller.module.ModuleController.MODULE_ERROR_MARKER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.module.ModuleController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.codeviewer.CodeViewerLink;
import innowake.mining.shared.model.codeviewer.CodeViewerLinkModel;
import innowake.mining.shared.model.codeviewer.CodeViewerRange;
import innowake.mining.shared.model.codeviewer.LinkTargetType;
import innowake.mining.shared.model.codeviewer.LinkType;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Test {@link ModuleController} endpoints and authorization
 */
@AutoConfigureMockMvc
@ActiveProfiles(value= Profiles.NO_AUTH, inheritProfiles = false)
class ModuleControllerTests extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private SourceService sourceService;

	@Autowired
	private JobManager jobManager;

	final static EntityId projectId = EntityId.of(1L);
	final static String TEST_MODULE_DISCOVERY = "TestModuleDiscovery";

	/**
	 * Creates a new module
	 * @param folder folder of the sources
	 * @param path Path of the module
	 * @param moduleName The module name
	 * @param technology The technology of the module
	 * @param type The type of the module 
	 * @return Returns the created module
	 * @throws IOException Thrown if the source-file can not be accessed
	 */
	protected ModulePojo createModule(final String folder, final String path, final String moduleName, final Technology technology, final Type type) throws IOException {
		final String content = Files.readString(Paths.get(folder).resolve(path)).replaceAll("\\r\\n", "\n").replaceAll("\\r", "\n");
		final String completePath = folder + path;

		final EntityId source = sourceService.create(new SourcePojoPrototype()
				.setProject(projectId)
				.setName(moduleName)
				.setPath(completePath)
				.setTechnology(technology)
				.setType(type)
				.setMetaDataRevision(1L)
				.setContentRevision(1L)
				.setContent(new BinaryString(content)
						));

		return moduleService.getModule(moduleService.create(new ModulePojoPrototype()
				.setProject(projectId)
				.setName(moduleName)
				.setPath(completePath)
				.setSource(source)
				.setTechnology(technology)
				.setType(type)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				));
	}


	@Test
	void testCreateModuleSetsCreatorApiByDefault() throws Exception {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TestModuleApi");
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		/* must set id here or else the JSON serialization fails */
		module.setNid((long) -1);

		mvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, 1).contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(module)))
		.andExpect(status().isCreated());

		final Optional<ModulePojo> moduleFromDb = moduleService.findAnyModule(b -> b.ofProject(projectId).withName("TestModuleApi"));
		assertTrue(moduleFromDb.isPresent());
		assertEquals(Creator.API, moduleFromDb.get().getCreator());
	}

	@Test
	void testCreateModulePreservesCreatorIfAlreadySet() throws Exception {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(TEST_MODULE_DISCOVERY);
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);

		/* explicitly set creator */
		module.setCreator(Creator.DISCOVERY);

		mvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, 1).contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(module)))
		.andExpect(status().isCreated());

		final Optional<ModulePojo> moduleFromDb = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TEST_MODULE_DISCOVERY));
		assertTrue(moduleFromDb.isPresent());
		assertEquals(Creator.DISCOVERY, moduleFromDb.get().getCreator());
	}

	@Test
	void testUpdateModulePreservesCreator() throws Exception {
		/* test that performing update without specifying creator preserves the existing value */

		/* run above test case to create Module with creator=DISCOVERY */
		testCreateModulePreservesCreatorIfAlreadySet();
		/* repeating the assertion from above test case for safety in case the test case is changed */
		Optional<ModulePojo> moduleFromDb = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TEST_MODULE_DISCOVERY));
		assertTrue(moduleFromDb.isPresent());
		assertEquals(Technology.COBOL, moduleFromDb.get().getTechnology());
		assertEquals(Creator.DISCOVERY, moduleFromDb.get().getCreator());

		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setNid(moduleFromDb.get().getId()); /* setting id for update */
		module.setName("TestModuleDiscovery");
		module.setProject(projectId);
		module.setTechnology(Technology.NATURAL); /* changing this from COBOL */
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		/* creator is not set - previous value "DISCOVERY" must be preserved */

		mvc.perform(put("/api" + ModuleController.MODULE_BY_ID_URL, 1, moduleFromDb.get().getId()).contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(module)))
		.andExpect(status().isOk());

		moduleFromDb = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TEST_MODULE_DISCOVERY));
		assertTrue(moduleFromDb.isPresent());
		assertEquals(Technology.NATURAL, moduleFromDb.get().getTechnology());
		assertEquals(Creator.DISCOVERY, moduleFromDb.get().getCreator()); /* assert creator was not changed */
	}

	@Test
	void testDataLineageAvailability() throws Exception {

		final ModulePojoPrototype binaryModule = new ModulePojoPrototype();
		binaryModule.setNid(1L);
		binaryModule.setName("TestDataLineageAvailabilityNatural");
		binaryModule.setProject(projectId);
		binaryModule.setTechnology(Technology.BINARY);
		binaryModule.setType(Type.PROGRAM);
		binaryModule.setStorage(Storage.FILE);
		binaryModule.setIdentification(Identification.IDENTIFIED);
		binaryModule.setOrigin(Origin.CUSTOM);
		binaryModule.setCreator(Creator.DISCOVERY);

		final ModulePojo createdBinaryModule = moduleService.getModule(moduleService.create(binaryModule));

		final String binaryResult = mvc.perform(get("/api" + ModuleController.CHECK_DATALINEAGE_AVAILABILITY, projectId.getNid(), createdBinaryModule.getId())).andReturn().getResponse().getContentAsString();
		/* DataLineage for binary is not supported yet, once it is supported this test case needs to be altered */
		assertFalse(Boolean.parseBoolean(binaryResult));

		final ModulePojoPrototype cobolModule = new ModulePojoPrototype();
		cobolModule.setNid(2L);
		cobolModule.setName("TestDataLineageAvailabilityCobol");
		cobolModule.setProject(projectId);
		cobolModule.setTechnology(Technology.COBOL); /* changing this from COBOL */
		cobolModule.setType(Type.PROGRAM);
		cobolModule.setStorage(Storage.FILE);
		cobolModule.setIdentification(Identification.IDENTIFIED);
		cobolModule.setOrigin(Origin.CUSTOM);
		cobolModule.setCreator(Creator.DISCOVERY);

		final ModulePojo createdCobolModule = moduleService.getModule(moduleService.create(cobolModule));

		final String cobolResult = mvc.perform(get("/api" + ModuleController.CHECK_DATALINEAGE_AVAILABILITY, projectId.getNid(), createdCobolModule.getId())).andReturn().getResponse().getContentAsString();
		assertTrue(Boolean.parseBoolean(cobolResult));
	}

	@Test
	void testDataLineagePreviewInCodeViewer() throws Exception {
		final ModulePojo createdCobolModule = createModule("./test-resources/innowake/mining/datalineage/source/", "modulecontrollertest/LINK.cbl", "LINK", Technology.COBOL, Type.PROGRAM);
		
		final MvcResult cobolResult = mvc.perform(get("/api" + ModuleController.CODE_VIEWER_DATA_FLOW_LINKS_URL, projectId.getNid(), createdCobolModule.getId(), 236).contentType(MediaType.APPLICATION_JSON)).andDo(print()).andExpect(status().isOk()).andReturn();
		
		final CodeViewerLink cvl1 = new CodeViewerLink(LinkType.DATA_FLOW_READ_ACCESS, LinkTargetType.LOCAL, "reads", "Call", EntityId.of(createdCobolModule.getId()),
				new ModuleLocation(236, 1), new CodeViewerRange(9, 23, 9, 24), EntityId.of(createdCobolModule.getId()), new ModuleLocation(427, 33),
				new CodeViewerRange(17, 12, 17, 45));
		final CodeViewerLink cvl2 = new CodeViewerLink(LinkType.DATA_FLOW_WRITE_ACCESS, LinkTargetType.LOCAL, "writes", "Call", EntityId.of(createdCobolModule.getId()),
				new ModuleLocation(236, 1), new CodeViewerRange(9, 23, 9, 24), EntityId.of(createdCobolModule.getId()), new ModuleLocation(427, 33),
				new CodeViewerRange(17, 12, 17, 45));
		final CodeViewerLink cvl3 = new CodeViewerLink(LinkType.DATA_FLOW_WRITE_ACCESS, LinkTargetType.LOCAL, "writes", "Move", EntityId.of(createdCobolModule.getId()),
				new ModuleLocation(236, 1), new CodeViewerRange(9, 23, 9, 24), EntityId.of(createdCobolModule.getId()), new ModuleLocation(381, 33),
				new CodeViewerRange(16, 12, 16, 45));
		final List<CodeViewerLink> expectedList = List.of(cvl1, cvl2, cvl3);
		
		final CodeViewerLinkModel model = objectMapper.readValue(cobolResult.getResponse().getContentAsString(), new TypeReference<>() {});
		assertEquals(expectedList.size(), model.getLinks().size());
		assertTrue(expectedList.containsAll(model.getLinks()), () -> "Expected: " + expectedList + " Actual: " + model.getLinks());
	}

	@Test
	void testCodeViewerDataFlowGraphJob() throws Exception {
		final ModulePojo wMIN10724 = createModule("./test-resources/innowake/mining/datalineage/source/",
				"modulecontrollertest/WMIN10724.cbl", "WMIN10724", Technology.COBOL, Type.PROGRAM);

		/* Running Data Flow Graph Query job based on field offset */
		final MvcResult jobResponse = mvc.perform(get("/api" + ModuleController.CODE_VIEWER_DATA_FLOW_GRAPH_JOB_URL,
						projectId.getNid(), wMIN10724.getId(), 539, false, null).contentType(MediaType.APPLICATION_JSON))
				.andDo(print()).andExpect(status().isOk())
				.andReturn();

		final String id = objectMapper.readValue(jobResponse.getResponse().getContentAsString(), new TypeReference<>() {});
		assertNotNull(id);
		final UUID jobId = UUID.fromString(id);

		/* Wait for the Job to complete */
		JobStatus jobStatus = getJobStatus(jobId);
		for (int i = 0; i < 10 && JobStatus.isActive(jobStatus); i++) {
			TimeUnit.SECONDS.sleep(1);
			jobStatus = getJobStatus(jobId);
		}

		/* Fetch the Job result */
		final MvcResult dlResult = mvc.perform(get("/api" + "/v1/jobs/{jobId}/result",
						jobId).contentType(MediaType.APPLICATION_JSON))
				.andDo(print()).andExpect(status().isOk())
				.andReturn();
		final DataFlowGraph dfg = objectMapper.treeToValue(objectMapper.readTree(dlResult.getResponse().getContentAsString())
				.get("object"), DataFlowGraph.class);
		assertNotNull(dfg);
		assertEquals(7, dfg.getNodes().size());

		final Set<StatementNode> statementNodes = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		assertEquals(2, statementNodes.size());
		final Set<FieldNode> fieldNodes = dfg.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
		assertEquals(Integer.valueOf(4), fieldNodes.size());
	}

	private JobStatus getJobStatus(final UUID jobId) {
		final List<JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(jobId));
		Assertions.assertFalse(jobInfos.isEmpty(), "Could not find any job");

		return jobInfos.get(0).getStatus();
	}

	@Test
	void testNoConfidentialErrorMessageInResponse() throws Exception {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TestHTTPErrorMessage");
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.API);
		/* must set id here or else the JSON serialization fails */
		module.setNid(1l);
		final ModulePojo createdModule = moduleService.getModule(moduleService.create(module));
		final Long moduleId = createdModule.getId();

		final MockHttpServletResponse response = mvc.perform(
				get("/api" + ModuleController.DEPENDENCIES_URL, projectId.getNid(), moduleId).param("maxDepth", "0").param("maxGraphNodes", "")
						.param("query", "$in.HasTaxonomy.id===4")).andReturn().getResponse();

		assertNotNull(response);
		assertEquals(HttpStatus.BAD_REQUEST.value(), response.getStatus());
		assertThat(response.getContentAsString(), CoreMatchers.containsString("java.lang.IllegalArgumentException"));
	}

	@Test
	void testCreateModuleSetsRequiresReviewFalseByDefault() throws Exception {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName("TestModuleRequiresReview");
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		/* must set id here or else the JSON serialization fails */
		module.setNid(Long.valueOf(-1));

		mvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, 1).contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(module)))
		.andExpect(status().isCreated());

		final Optional<ModulePojo> moduleFromDb = moduleService.findAnyModule(b -> b.ofProject(projectId).withName("TestModuleRequiresReview"));
		assertTrue(moduleFromDb.isPresent());
		assertEquals(false, moduleFromDb.get().isRequiresReview());
	}

	@Test
	void testGetErrorMarkers() throws Exception {
		final ModulePojoPrototype modulePojo = new ModulePojoPrototype();
		modulePojo.setName("TestModuleErrorMarkers");
		modulePojo.setProject(projectId);
		modulePojo.setTechnology(Technology.COBOL);
		modulePojo.setType(Type.PROGRAM);
		modulePojo.setStorage(Storage.FILE);
		modulePojo.setIdentification(Identification.IDENTIFIED);
		modulePojo.setOrigin(Origin.CUSTOM);
		modulePojo.setCreator(Creator.API);

		final ModulePojo module = moduleService.getModule(moduleService.create(modulePojo));

		final Long moduleId = module.getId();
		final UUID recordId = module.getUid();
		assertNotNull(recordId);

		/* As there are no ErrorMarkers, the size of the ErrorMarkerList should be 0.*/
		final MvcResult intialResult = mvc.perform(get("/api" + MODULE_ERROR_MARKER, projectId.getNid(), moduleId)
				.contentType(MediaType.APPLICATION_JSON)).andDo(print()).andExpect(status().isOk()).andReturn();
		final List<ErrorMarker> errorMarkerList =
				objectMapper.readValue(intialResult.getResponse().getContentAsString(), new TypeReference<>() {});
		assertEquals(0, errorMarkerList.size());

		final ErrorMarkerPojoPrototype err1 = new ErrorMarkerPojoPrototype().setProject(projectId).setModule(module.identity()).setCause("PARSE_ERROR");
		final ErrorMarkerPojoPrototype err2 = new ErrorMarkerPojoPrototype().setProject(projectId).setModule(module.identity()).setCause("METRICS_CALCULATION_ERROR");
		final ErrorMarkerPojoPrototype err3 = new ErrorMarkerPojoPrototype().setProject(projectId).setModule(module.identity()).setCause("MODULE_ABORT");

		moduleService.createErrorMarkers(List.of(err1, err2, err3));

		final MvcResult currentResult = mvc.perform(get("/api" + MODULE_ERROR_MARKER, projectId.getNid(), moduleId)
				.contentType(MediaType.APPLICATION_JSON)).andDo(print()).andExpect(status().isOk()).andReturn();
		final List<ErrorMarker> currentErrorMarkerList =
				objectMapper.readValue(currentResult.getResponse().getContentAsString(), new TypeReference<>() {
                });
		assertEquals(3, currentErrorMarkerList.size());
		assertTrue(currentErrorMarkerList.stream().map(ErrorMarker::getCause).collect(Collectors.toList())
				.containsAll(Arrays.asList("PARSE_ERROR" , "METRICS_CALCULATION_ERROR", "MODULE_ABORT")));
	}
}
