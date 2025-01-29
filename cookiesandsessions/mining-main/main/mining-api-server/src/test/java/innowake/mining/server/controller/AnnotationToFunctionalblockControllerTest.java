/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;

import brave.internal.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
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

/**
 * Test for {@link AnnotationToFunctionalBlockController}
 */
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false)
class AnnotationToFunctionalblockControllerTest extends DatabaseRelatedTest {
	
	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private AnnotationService annotationService;
	
	@Autowired
	private FunctionalBlockService functionalBlockService;
	
	@Autowired
	private ObjectMapper objectMapper;
	
	private static final String GET_FUNCTIONAL_BLOCK_URL = "/api" + AnnotationToFunctionalBlockController.FUNCTIONAL_BLOCKS_FROM_ANNOTATION;
	
	private static final String CREATE_FUNCIONAL_BLOCK_URL = "/api" + AnnotationToFunctionalBlockController.CREATE_FUNCTIONAL_UNIT_FROM_ANNOTATION;
	private EntityId projectId = null;

	@BeforeAll
	void initialize() {
		projectId = loadProjectAndClient("Mock Project 1", EntityId.of(1L));
	}
	
	@Test
	void testGetFunctionalUnitsForAnnotations() throws Exception {
		final var module1Id = createModule(projectId, '1');
		final var annotation1 = createAnnotation(module1Id, "Annotation_1");
		final var annotation2 = createAnnotation(module1Id, "Annotation_2");
		final List<EntityId> annotations = new ArrayList<EntityId>();
		annotations.add(annotation1);
		annotations.add(annotation2);

		final var jsonContent = objectMapper.writeValueAsString(annotations);
		final MvcResult result = mvc
				.perform(post(CREATE_FUNCIONAL_BLOCK_URL, projectId.getNid()).contentType("application/json")
						.content(jsonContent))
				.andDo(print()).andExpect(status().isOk()).andReturn();
		final String annotationToFunctionalGroupJson = result.getResponse().getContentAsString();
		var resultMap = objectMapper.readValue(annotationToFunctionalGroupJson, new TypeReference<Map<EntityId, UUID>>() { });
		assertTrue(resultMap.keySet().stream().map(EntityId::getNid).allMatch(n -> annotation1.getNid() == n || annotation2.getNid() == n),
				"Should return the annotation id");
		assertTrue(resultMap.keySet().stream().map(EntityId::getNid).anyMatch(annotation2.getNid()::equals), "Should return the annotation id");
		assertFalse(annotationToFunctionalGroupJson.contains("null"), "Create functional block should not return null");
	}
	
	@Test
	void testGetFunctionalBlockForAnnotations() throws Exception {
		final var module2Id = createModule(projectId, '2');
		final var annotation1 = createAnnotation(module2Id, "Annotation_3");
		final var annotation2 = createAnnotation(module2Id, "Annotation_4");

		final List<EntityId> annotations = new ArrayList<>();
		annotations.add(annotation1);
		annotations.add(annotation2);

		final UUID groupUid = createFunctionalBlocks(annotations);
		/* Creating a saved search named Individual Saved Search with scope Individual */
		final MvcResult result = mvc
				.perform(post(GET_FUNCTIONAL_BLOCK_URL, projectId.getNid()).contentType("application/json").content(new Gson().toJson(annotations)))
				.andDo(print()).andExpect(status().isOk()).andReturn();
		final String annotationToFunctionalGroupJson = result.getResponse().getContentAsString();
		final Map<Long, List<FunctionalBlockPojo>> resultMap = objectMapper.readValue(annotationToFunctionalGroupJson,
				new TypeReference<Map<Long, List<FunctionalBlockPojo>>>() { });

		final List<FunctionalBlockPojo> functionalblockList1 = resultMap.get(annotation1.getNid());
		assertEquals(1, functionalblockList1.size(), "There should be one functional group for annotation 1");
		assertEquals(groupUid, functionalblockList1.get(0).getUid(), "Uid of Functional block should be as created");

		final List<FunctionalBlockPojo> functionalblockList2 = resultMap.get(annotation2.getNid());
		assertEquals(1, functionalblockList2.size(), "There should be one functional group for annotation 2");
		assertEquals(groupUid, functionalblockList2.get(0).getUid(), "Uid of Functional block should be as created");

		assertTrue(resultMap.values().stream().flatMap(List::stream).map(FunctionalBlockPojo::getName).anyMatch("Test Group 1"::equals),
				"Name of functional group should be correct");
		assertTrue(resultMap.get(annotation1.getNid()).stream().map(FunctionalBlockPojo::getUid).anyMatch(groupUid::equals),
				"Uid of functional block should be as created");
		assertTrue(resultMap.get(annotation2.getNid()).stream().map(FunctionalBlockPojo::getUid).anyMatch(groupUid::equals),
				"Uid of functional block should be as created");
		assertTrue(resultMap.values().stream().flatMap(List::stream).map(FunctionalBlockPojo::getFlags)
					.map(m -> (List<?>) m.get(FunctionalBlockFlag.TYPE.name())).allMatch(l -> l.contains(FunctionalBlockType.FUNCTIONAL_GROUP.name())),
				"Return functional block should be type of Group");
	}
	
	private EntityId createModule(final EntityId projectId, final char suffix) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName("MOD" + suffix);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setOrigin(Origin.CUSTOM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.MISSING);
		module.setPath("src/cobol/TESTCOBA" + suffix + ".cbl");
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private EntityId createAnnotation(final EntityId moduleId, final String name) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setType(AnnotationType.RULE);
		annotation.setName(name);
		annotation.setState(WorkingState.IN_ANALYSIS);
		annotation.setCreatedByUserId("asdf");
		annotation.setModule(moduleId);
		annotation.setLocation(new ModuleLocation(0, 1));
		
		return annotationService.create(annotation);
	}
	
	private EntityId loadProjectAndClient(final String name, final EntityId clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(clientId)
				.setNatures(new HashSet<>(Collections.emptyList()))
			).identity();
	}
	
	private UUID createFunctionalBlocks(final List<EntityId> annotations) {
		final List<UUID> childUIDs = new ArrayList<>();

		for (int i = 0; i < annotations.size(); i++) {
			final UUID functionalblockUID3 = createFunctionalBlockPojoPrototype(projectId, null, "Test Unit " + i, "",
					Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)));
			childUIDs.add(functionalblockUID3);
			functionalBlockService.setGeneratedFrom(functionalblockUID3, GeneratedFrom.fromAnnotation(annotations.get(i)));
		}

		final UUID functionalblockUID1 = createFunctionalBlockPojoPrototype(projectId, childUIDs, "Test Group 1", "",
				Collections.singletonMap("TYPE", Set.of(FunctionalBlockType.FUNCTIONAL_GROUP)));
		return functionalblockUID1;
	}
	
	private UUID createFunctionalBlockPojoPrototype(
			 final EntityId project,
			 final @Nullable List<UUID> children,
			 final String name,
			 final String description,
			 final @Nullable Map<String, Object> flags) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype= new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(description);

		if(children != null) {
			functionalBlockPojoPrototype.setChildren(children);
		}
		if (flags != null) {
			functionalBlockPojoPrototype.setFlags(flags);
		}
		return functionalBlockService.create(functionalBlockPojoPrototype);
	}
}
