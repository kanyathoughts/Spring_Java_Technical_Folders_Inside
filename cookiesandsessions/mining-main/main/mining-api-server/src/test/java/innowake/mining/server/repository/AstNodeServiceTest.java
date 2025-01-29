/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.repository;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.collection.IsIterableWithSize.iterableWithSize;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.model.springdata.JclControlFlowNodeMetadata;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * {@link AstNodeRepository} related tests.
 */
class AstNodeServiceTest extends DatabaseRelatedTest {

	public static final String TYPE_NAME = "SuperImportantType";
	public static final String OTHER_TYPE_NAME = "DummyType";

	private final AstService astService;
	private ModuleService moduleService;
	
	private static final EntityId PROJECT_ID = EntityId.of(1L);
	
	@Autowired
	AstNodeServiceTest(final AstService astService, final ModuleService moduleService) {
		this.astService = astService;
		this.moduleService = moduleService;
	}

	@Test
	void customQueryForRetrievingAstNodesByType() {
		final EntityId module = createModule();
		/* Let's add 3 AST nodes to the Module */
		createAstNode(TYPE_NAME, module);
		createAstNode(TYPE_NAME, module);
		createAstNode(OTHER_TYPE_NAME, module);

		/* Retrieve all the AST nodes and filter out those belonging to our Module */
		final List<AstNodePojo> allNodesOnFirstModule = astService.find(q -> q.ofModule(module));
		/* Call the method, which uses the custom query we are actually trying to test. Here we are only interested
		* in the AST nodes with a particular type, which should result in a subset of nodes associated with the Module */
		final List<AstNodePojo> nodesFromCustomQuery = astService.find(q -> q.ofModule(module).withType(TYPE_NAME));

		assertAll(
				/* Overall the Module has 3 AST nodes associated */
				() -> assertThat(allNodesOnFirstModule, hasSize(3)),
				/* Restricting it to a certain type should yield only a subset */
				() -> assertThat(nodesFromCustomQuery, hasSize(2))
		);
	}

	@Test
	void metadataCanBeSavedAsAdditionalInfo() {
		/* Let's create a test module and associate an AST node with a random type */
		final EntityId module = createModule();
		final String type = UUID.randomUUID().toString();
		createAstNode(type, module);

		/* Retrieve the AST node we previously saved in the database */
		final List<AstNodePojo> nodes = astService.find(q -> q.ofModule(module).withType(type));
		assertThat(nodes, hasSize(1));

		/* Now we need some metadata we want to associate with the AST node, which we created */
		final JclControlFlowNodeMetadata metadata = new JclControlFlowNodeMetadata();
		metadata.setInputFileIds(Collections.singletonList(module.getNid()));
		metadata.setOutputFileIds(Collections.singletonList(module.getNid()));

		/* Persist the metadata */
		astService.update(new AstNodePojoPrototype().setId(nodes.get(0).getId()).setProperties(PojoMapper.convert(metadata)));

		/* Let's again retrieve the AST node from the database to ensure the metadata is available */
		final AstNodePojo astNode = astService.get(nodes.get(0).getId());

		/* and then let's retrieve this metadata */
		final JclControlFlowNodeMetadata persistedMetadata = PojoMapper.convert(astNode.getProperties(), JclControlFlowNodeMetadata.class);

		assertAll(
				/* Finally, let's make sure we can retrieve the input and output files */
				() -> assertThat(persistedMetadata.getInputFileIds(), iterableWithSize(1)),
				() -> assertEquals(module.getNid(), persistedMetadata.getInputFileIds().get(0)),
				() -> assertThat(persistedMetadata.getOutputFileIds(), iterableWithSize(1)),
				() -> assertEquals(module.getNid(), persistedMetadata.getOutputFileIds().get(0))
		);
	}
	
	@Test
	void testfindUnreferencedBlockNodes() {
		final var module = createModule();
		/* Let's add 3 AST nodes to the Module */
		createAstNode(TYPE_NAME, module);
		createAstNode(TYPE_NAME, module);
		createAstNode(OTHER_TYPE_NAME, module);
		final List<AstNodePojo> unreferencedBlockNodes = astService.find(q -> q.ofModule(module)
				.withSuperTypes("CfgCollapsibleNode").withRelationshipCount(AstRelationshipType.FLOW, RelationshipDirection.IN, Comperator.EQUAL, 0));
		assertEquals(3, unreferencedBlockNodes.size());
		final long superImportantTypeNodescount = unreferencedBlockNodes.stream().filter(node -> assertNotNull(node.getType()).equals(TYPE_NAME)).count();
		assertEquals(2, superImportantTypeNodescount);
		final long dummyTypeNodescount = unreferencedBlockNodes.stream().filter(node -> assertNotNull(node.getType()).equals(OTHER_TYPE_NAME)).count();
		assertEquals(1, dummyTypeNodescount);
	}

	private UUID createAstNode(final String type, final EntityId moduleId) {
		return astService.create(new AstNodePojoPrototype()
			.setLabel("DummyLabel")
			.setType(type)
			.setModule(moduleId)
			.setLocation(new AstNodeLocation(null, null))
			.setSuperTypes(Collections.singleton("CfgCollapsibleNode"))
		);
	}

	private EntityId createModule() {
		return moduleService.create(new ModulePojoPrototype()
			.setProject(PROJECT_ID)
			.setName("test Program")
			.setType(Type.PROGRAM)
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setOrigin(Origin.CUSTOM)
			.setStorage(Storage.FILE)
			.setIdentification(Identification.IDENTIFIED)
			.setCreator(Creator.DISCOVERY)
		);
	}

}
