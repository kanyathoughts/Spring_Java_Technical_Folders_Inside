/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.Optional;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for the server side function StoreAst.
 */
@WithMockUser
class StoreAstTest extends AbstractIdentificationTest {
	
	private static final String PATH_PRGA = "src/cobol/programs/PRGA.cbl";
	private static final String PATH_PRGB = "src/cobol/programs/PRGB.cbl";
	private static final String PATH_PRGC = "src/cobol/programs/PRGC.cbl";
	private static final String PATH_PRGD = "src/cobol/programs/PRGD.cbl";
	private static final String PATH_CC1 = "src/cobol/programs/CC1.cpy";
	private static final String PATH_CC2 = "src/cobol/programs/CC2.cpy";
	private static final Long LONG_ONE = 1L;
	private static final Long LONG_TWO = 2L;
	
	@Autowired
	private ExecutorService executorService;
	
	@Autowired
	private AstService astService;
	
	@Test
	void testSimple() {
		/* query Module ID */
		final EntityId moduleId = findModuleIdByPath(PATH_PRGA, LONG_ONE);
		assertNotNull(moduleId);
		assertAst(moduleId, 5, LONG_ONE.intValue(), Collections.singletonList(moduleId));
		
		/* Delete Module as we do in in ModuleDao */
		final Long countDeletedModules = Long.valueOf(moduleService.deleteModule(moduleId, false));
		assertEquals(LONG_ONE, countDeletedModules);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode2 = queryCountAstNode(moduleId);
		assertEquals(0, countAstNode2);
	}
	
	@Test
	void testNestedCopies() {
		final EntityId moduleIdPrg = findModuleIdByPath(PATH_PRGB, LONG_ONE);
		final EntityId moduleIdCc1 = findModuleIdByPath(PATH_CC1, LONG_ONE);
		final EntityId moduleIdCc2 = findModuleIdByPath(PATH_CC2, LONG_ONE);
		/* store AST for Module ID */
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleIdPrg);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode = queryCountAstNode(moduleIdPrg);
		assertEquals(29, countAstNode);
		
		/* count number of HasAst edges */
		final Long countHasAst = queryCountHasAst(Arrays.asList(moduleIdPrg, moduleIdCc1, moduleIdCc2));
		assertEquals(4, countHasAst);
		
		/* query in from HasAst edge between Module and AstNode */
		assertHasAstInRid(moduleIdPrg);
		final Long countHasAstPrg = queryCountHasAst(moduleIdPrg);
		assertEquals(LONG_ONE, countHasAstPrg);
		final Long countHasAstCc1 = queryCountHasAst(moduleIdCc1);
		assertEquals(LONG_ONE, countHasAstCc1);
		final Long countHasAstCc2 = queryCountHasAst(moduleIdCc2);
		assertEquals(LONG_TWO, countHasAstCc2);
		
		assertInclusionCalleeModulePresent(moduleIdPrg, moduleIdCc1, moduleIdCc2);
		assertNoAst(moduleIdCc1);
		assertNoAst(moduleIdCc2);
		
		/* Delete Module as we do in in ModuleDao */
		final Long countDeletedModules = Long.valueOf(moduleService.deleteModule(moduleIdPrg, false));
		assertEquals(LONG_ONE, countDeletedModules);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode2 = queryCountAstNode(moduleIdPrg);
		assertEquals(0, countAstNode2);
	}
	
	@Test
	void testProperties() {
		final EntityId moduleId = findModuleIdByPath(PATH_PRGC, LONG_ONE);
		
		/* store AST for Module ID */
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleId);

		/* count number of AstNode vertices for Module ID */
		final Long countAstNode = queryCountAstNode(moduleId);
		assertEquals(21, countAstNode);
		
		final var nodes = astService.find(q -> q.ofModule(moduleId).withType("CobolDataField", "CobolFieldReference", "CobolPerformStmt"));
		assertEquals(6, nodes.size());
		
		for (final var node : nodes) {
			assertTrue("properties must not be empty", node.getProperties().size() > 0);
		}
	}
	
	@Test
	void testRefersTo() {
		final EntityId moduleId = findModuleIdByPath(PATH_PRGC, LONG_ONE);
		
		/* store AST for Module ID */
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleId);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode = queryCountAstNode(moduleId);
		assertEquals(21, countAstNode);
		
		final var relations = astService.findRelationships(q -> q.withinModule(moduleId).withType(AstRelationshipType.REFERS));
		for (final var relation : relations) {
			assertEquals("CobolDataField", relation.getDstNode().getType());
			assertEquals("CobolFieldReference", relation.getSrcNode().getType());
		}
	}
	
	@Test
	void testRedefines() {
		final EntityId moduleId = findModuleIdByPath(PATH_PRGD, LONG_ONE);
		
		/* store AST for Module ID */
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleId);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode = queryCountAstNode(moduleId);
		assertEquals(12, countAstNode);
		
		final var relations = astService.findRelationships(q -> q.withinModule(moduleId).withType(AstRelationshipType.REDEFINES));
		assertEquals(2, relations.size());
		relations.sort((r1, r2) ->  Integer.compare(r1.getSrcNode().getLocation().getRetracedOffset().orElse(0),
				r2.getSrcNode().getLocation().getRetracedOffset().orElse(0)));
		
		var relation = relations.get(0);
		assertEquals("CobolDataField", relation.getDstNode().getType());
		assertEquals(61, relation.getDstNode().getLocation().getRetracedOffset().orElseThrow());
		assertEquals("CobolDataField", relation.getSrcNode().getType());
		assertEquals(126, relation.getSrcNode().getLocation().getRetracedOffset().orElseThrow());

		relation = relations.get(1);
		assertEquals("CobolDataField", relation.getDstNode().getType());
		assertEquals(61, relation.getDstNode().getLocation().getRetracedOffset().orElseThrow());
		assertEquals("CobolDataField", relation.getSrcNode().getType());
		assertEquals(220, relation.getSrcNode().getLocation().getRetracedOffset().orElseThrow());
	}
	
	@Test
	void testNatural() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createModule(projectId, "NATPRGA", "NATPRGA.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		
		assertAst(moduleId, 21, LONG_ONE.intValue(), Collections.singletonList(moduleId));
		
		/* Delete Module as we do in ModuleDao */
		final Long countDeletedModules = Long.valueOf(moduleService.deleteModule(moduleId, false));
		assertEquals(LONG_ONE, countDeletedModules);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode2 = queryCountAstNode(moduleId);
		assertEquals(0, countAstNode2);
	}

	@Test
	void testNaturalCopycode() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleProgramId = createModule(projectId, "NATPRGB", "NATPRGB.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleCopyId = createModule(projectId, "NATCCA", "NATCCA.nsc", RESOURCE_PATH, Technology.NATURAL, Type.COPYCODE);
		createReference(RelationshipType.INCLUDES, moduleProgramId, moduleCopyId);
		
		assertAst(moduleProgramId, 27, LONG_TWO.intValue(), Arrays.asList(moduleProgramId, moduleCopyId));
		
		final Long countHasAstPrg = queryCountHasAst(moduleProgramId);
		assertEquals(LONG_ONE, countHasAstPrg);
		final Long countHasAstCc = queryCountHasAst(moduleCopyId);
		assertEquals(LONG_ONE, countHasAstCc);
		
		assertInclusionCalleeModulePresent(moduleProgramId, moduleCopyId);
		assertNoAst(moduleCopyId);
		
		/* Delete Module as we do in ModuleDao */
		final Long countDeletedModules = Long.valueOf(moduleService.deleteModule(moduleProgramId, false));
		assertEquals(LONG_ONE, countDeletedModules);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode2 = queryCountAstNode(moduleProgramId);
		assertEquals(0, countAstNode2);
	}

	@Test
	void testAstBindingCreation() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = 
				createModule(projectId, "EE", "ExitParagraph.cbl",
						RESOURCE_PATH,
						Technology.COBOL, Type.PROGRAM);
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleId);
		
		final var ast = astService.find(q -> q.ofModule(moduleId));
		
		int astBindingCount = 0;
		for (final var node : ast) {
			if (node.getSuperTypes().contains("FieldDefinition")) {
				assertTrue(node.getIncomingRelations().size() > 0);
			}
			if (node.getSuperTypes().contains("FieldReference")) {
				assertTrue(node.getOutgoingRelations().size() > 0);
			}
			astBindingCount += node.getOutgoingRelations().stream().filter(r -> AstRelationshipType.BINDING.equals(r.getType())).count();
		}
		assertEquals(4, astBindingCount);
	}

	@Test
	void testNaturalLda() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createModule(projectId, "NATPRGC", "NATPRGC.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleLdaId = createModule(projectId, "NATLDAA", "NATLDAA.nsiwl", RESOURCE_PATH, Technology.NATURAL, Type.LDA);
		createReference(RelationshipType.INCLUDES, moduleId, moduleLdaId);
		
		assertAst(moduleId, 13, LONG_TWO.intValue(), Arrays.asList(moduleId, moduleLdaId));
		
		final Long countHasAstPrg = queryCountHasAst(moduleId);
		assertEquals(LONG_ONE, countHasAstPrg);
		final Long countHasAstLda = queryCountHasAst(moduleLdaId);
		assertEquals(LONG_ONE, countHasAstLda);
		
		assertInclusionCalleeModulePresent(moduleId, moduleLdaId);
		assertNoAst(moduleLdaId);
		
		/* Delete Module as we do in ModuleDao */
		final Long countDeletedModules = Long.valueOf(moduleService.deleteModule(moduleId, false));
		assertEquals(LONG_ONE, countDeletedModules);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode2 = queryCountAstNode(moduleId);
		assertEquals(0, countAstNode2);
	}
	
	@Test
	void testNaturalMultipleInclusions() {
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createModule(projectId, "NATPRGD", "NATPRGD.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		final EntityId moduleIdCcA = createModule(projectId, "NATCCA", "NATCCA.nsc", RESOURCE_PATH, Technology.NATURAL, Type.COPYCODE);
		final EntityId moduleIdCcB = createModule(projectId, "NATCCB", "NATCCB.nsc", RESOURCE_PATH, Technology.NATURAL, Type.COPYCODE);
		final EntityId moduleIdLda = createModule(projectId, "NATLDAA", "NATLDAA.nsiwl", RESOURCE_PATH, Technology.NATURAL, Type.LDA);
		final EntityId moduleIdPda = createModule(projectId, "NATPDAA", "NATPDAA.nsiwa", RESOURCE_PATH, Technology.NATURAL, Type.PDA);

		createReference(RelationshipType.INCLUDES, moduleId, moduleIdCcA);
		createReference(RelationshipType.INCLUDES, moduleId, moduleIdCcB);
		createReference(RelationshipType.INCLUDES, moduleIdCcB, moduleIdCcA);
		createReference(RelationshipType.INCLUDES, moduleId, moduleIdLda);
		createReference(RelationshipType.INCLUDES, moduleId, moduleIdPda);
		
		assertAst(moduleId, 63, 6, Arrays.asList(moduleId, moduleIdCcA, moduleIdCcB, moduleIdLda, moduleIdPda));
		
		final Long countHasAstPrg = queryCountHasAst(moduleId);
		assertEquals(LONG_ONE, countHasAstPrg);
		final Long countHasAstCcA = queryCountHasAst(moduleIdCcA);
		assertEquals(LONG_TWO, countHasAstCcA);
		final Long countHasAstCcB = queryCountHasAst(moduleIdCcB);
		assertEquals(LONG_ONE, countHasAstCcB);
		final Long countHasAstLda = queryCountHasAst(moduleIdLda);
		assertEquals(LONG_ONE, countHasAstLda);
		final Long countHasAstPda = queryCountHasAst(moduleIdPda);
		assertEquals(LONG_ONE, countHasAstPda);
		
		assertInclusionCalleeModulePresent(moduleId, moduleIdCcA, moduleIdCcB, moduleIdLda, moduleIdPda);
		assertNoAst(moduleIdCcA);
		assertNoAst(moduleIdCcB);
		assertNoAst(moduleIdLda);
		assertNoAst(moduleIdPda);
		
		/* Delete Module as we do in ModuleDao */
		final Long countDeletedModules = Long.valueOf(moduleService.deleteModule(moduleId, false));
		assertEquals(LONG_ONE, countDeletedModules);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode2 = queryCountAstNode(moduleId);
		assertEquals(0, countAstNode2);
	}
	
	/**
	 * Make sure {@linkplain AstNode AST nodes} have a proper label.
	 * <p>
	 * For testing purposes the whitespaces on the label values are normalized and the labels are sorted on the node types.
	 */
	@Test
	void astNodeLabelsArePresent() {
		final EntityId moduleIdByPath = findModuleIdByPath(PATH_PRGA, LONG_ONE);
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleIdByPath);
		
		final SortedMap<String, String> labelTypes = new TreeMap<>();
		astService.find(q -> q.ofModule(moduleIdByPath)).forEach(node -> labelTypes.put(node.getType(), node.getLabel()));
		
		final StringBuilder actualLabels = new StringBuilder();
		for (final Iterator<Entry<String, String>> iterator = labelTypes.entrySet().iterator(); iterator.hasNext();) {
			final Entry<String, String> entry = iterator.next();
			final String rawLabel = entry.getValue();
			final String label = StringUtils.isBlank(rawLabel) ? "<NO LABEL>" : StringUtils.normalizeSpace(rawLabel);
			actualLabels.append(entry.getKey()).append(": ").append(label);
			if (iterator.hasNext()) {
				actualLabels.append("\n");
			}
		}
		final String expectedLabels = getContent("WMIN651.labels.expected");
		assertEquals(expectedLabels, actualLabels.toString());
	}

	@Test
	void testPl1ProgramWithCopybook() {
		final var projectId = createProject().identity();
		final EntityId moduleProgramId = createModule(projectId, "Pl1File1", "Pl1File1.pl1", RESOURCE_PATH, Technology.PL1, Type.PROGRAM);
		final EntityId moduleCopyId = createModule(projectId, "SQLCA", "SQLCA.pl1", RESOURCE_PATH, Technology.PL1, Type.COPYBOOK);
		createReference(RelationshipType.INCLUDES, moduleProgramId, moduleCopyId);
		
		assertAst(moduleProgramId, 289, 1, Collections.singletonList(moduleProgramId));
		assertEquals(0, queryCountAstNode(moduleCopyId));
		assertEquals(1, queryCountHasAst(moduleCopyId));
		assertNoAst(moduleCopyId);
	}

	private EntityId findModuleIdByPath(final String path, final Long projectId) {
		return moduleService.findAnyModule(b -> b.ofProject(EntityId.of(projectId)).withPath(path))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for path: " + path))
				.identity();
	}
	
	private Long queryCountAstNode(final EntityId moduleId) {
		return astService.count(q -> q.ofModule(moduleId));
	}
	
	private void assertNoAst(final EntityId moduleId) {
		final List<AstNodePojo> astNodeList = astService.find(q -> q.ofModule(moduleId));
		assertTrue(astNodeList.isEmpty());
	}
	
	private Long queryCountHasAst(final EntityId moduleId) {
		return astService.count(q -> q.withRelationshipToModule(moduleId, AstModuleRelationshipType.ROOT));
	}

	private Long queryCountHasAst(final List<EntityId> moduleIds) {
		return astService.count(q -> q.withRelationshipToModules(EntityId.allUids(moduleIds), Arrays.asList(AstModuleRelationshipType.ROOT)));
	}
	
	private List<AstModuleRelationshipPojo> assertHasAstInRid(final EntityId moduleId) {
		final var result = astService.findModuleRelationships(q -> q.ofModule(moduleId).withType(AstModuleRelationshipType.ROOT));
		assertFalse(result.isEmpty());
		return result;
	}
	
	private void assertInclusionCalleeModulePresent(final EntityId moduleId, final EntityId... expectedIncludes) {
		final var astNodeList = astService.find(q -> q.ofModule(moduleId));
		assertFalse(astNodeList.isEmpty());
		final var includes = astNodeList.stream().map(node -> node.getIncludedModule()).filter(Optional::isPresent).map(Optional::get).collect(Collectors.toSet());
		assertTrue(includes.containsAll(Arrays.asList(expectedIncludes)));
	}
	
	private void assertAst(final EntityId moduleId, final int expectedAstNodes, final int expectedHasAstEdges, final List<EntityId> moduleIds) {
	
		/* store AST for Module ID */
		executorService.executeStoreAst(EntityId.of(LONG_ONE), moduleId);
		
		/* count number of AstNode vertices for Module ID */
		final Long countAstNode = queryCountAstNode(moduleId);
		assertEquals(expectedAstNodes, countAstNode);
		
		/* count number of HasAst edges */
		final Long countHasAst = queryCountHasAst(moduleIds);
		assertEquals(expectedHasAstEdges, countHasAst);
		
		/* query in from HasAst edge between Module and AstNode */
		assertHasAstInRid(moduleId);
	}
	
	/**
	 * Optionally returns an additional SQL script file to be executed against Postgres.
	 *
	 * @return SQL script file to be executed 
	 */
	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-store-ast");
	}
}
