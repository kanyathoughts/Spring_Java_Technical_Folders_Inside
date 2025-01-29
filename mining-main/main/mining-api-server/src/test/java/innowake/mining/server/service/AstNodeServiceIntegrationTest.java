/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.DataFieldFormat;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Contains tests for AstNodeService with AstNode created in database
 */
@WithMockUser
class AstNodeServiceIntegrationTest extends AbstractIdentificationTest {
	
	private static final Integer OFFSET1 = 1802;
	private static final String FIELD_NAME1 = "MY-HEX-TRANS";
	private static final String FIELD_NAME2 = "DFHAID";
	private static final String TEST_ASTNODE_LABEL1 = "01 MY-HEX-TRANS PIC X(8) VALUE 'MMRS71Z1'.";
	private static final String TEST_ASTNODE_LABEL2 = "01 DFHAID.";
	protected static final String RESOURCE_PATH2 = "/test-resources/innowake/mining/server/job/identification/wmin-5715/";
	
	@Autowired
	private AstService astService;
	
	@Autowired
	private AstNodeService astNodeService;
	
	@Autowired
	private ExecutorService executorService;
	
	@Test
	void testGetFormatIfSelectionIsValidWithNormalisedLabel() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH);
		final List<DataFieldFormat> dataFieldFormat = new ArrayList<>();
		
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleId);
		
		/* selects offset and label from AstNode for Module ID */
		for (final var node : astService.find(q -> q.ofModule(moduleId).withSuperTypes("FieldDefinition"))) {
			if (node.getLabel().equals(TEST_ASTNODE_LABEL1)) {
				dataFieldFormat.add(astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, moduleId,
						node.getLocation().getRetracedOffset().orElseThrow() + node.getLabel().indexOf(FIELD_NAME1) + 1, false));
			}
		}
		assertEquals(1, dataFieldFormat.size());
		assertEquals(OFFSET1, dataFieldFormat.get(0).getLocation().getOffset());
		assertEquals(FIELD_NAME1, dataFieldFormat.get(0).getFieldName());
		assertEquals(DefinedLocation.PROGRAM, dataFieldFormat.get(0).getDefinedLocation());
	}
	
	@Test
	void testGetFormatForCopyBooksIncludedInOtherCopyBooks() {
		final EntityId cpyAId = createCobolCopybook(PROJECT_ID_1, "MMRS710A", "MMRS710A.cpy", RESOURCE_PATH2);
		final EntityId cpyDId = createCobolCopybook(PROJECT_ID_1, "MMRS710D", "MMRS710D.cpy", RESOURCE_PATH2);
		final EntityId cpyEId = createCobolCopybook(PROJECT_ID_1, "MMRS710E", "MMRS710E.cpy", RESOURCE_PATH2);
		final EntityId cpyFId = createCobolCopybook(PROJECT_ID_1, "MMRS710F", "MMRS710F.cpy", RESOURCE_PATH2);
		final EntityId cpyGId = createCobolCopybook(PROJECT_ID_1, "MMRS710G", "MMRS710G.cpy", RESOURCE_PATH2);
		final EntityId cpyHId = createCobolCopybook(PROJECT_ID_1, "MMRS710H", "MMRS710H.cpy", RESOURCE_PATH2);
		final EntityId programId = createCobolProgram(PROJECT_ID_1, "MMRS71012", "MMRS71012.cbl", RESOURCE_PATH2);
		
		createReference(RelationshipType.INCLUDES, programId, cpyAId);
		createReference(RelationshipType.INCLUDES, cpyAId, cpyHId);
		createReference(RelationshipType.INCLUDES, cpyDId, cpyFId);
		createReference(RelationshipType.INCLUDES, cpyDId, cpyEId);
		createReference(RelationshipType.INCLUDES, cpyFId, cpyHId);
		createReference(RelationshipType.INCLUDES, cpyFId, cpyGId);
		
		final List<DataFieldFormat> dataFieldFormat = new ArrayList<>();
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, programId);
		
		dataFieldFormat.add(astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, cpyHId, 118, false));
		assertEquals(1, dataFieldFormat.size());
		assertEquals("MMRS-CURRENT-MINUTE", dataFieldFormat.get(0).getFieldName());
		
		/* asserting that method throws an exception when a Copybook is not included in any Program*/
		Assert.assertThrows(MiningEntityNotFoundException.class, () -> astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, cpyFId, 169, false));
	}
	
	@Test
	void testGetFormatIfSelectionThrowsExceptionIfNoAstNodePresent() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_2, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH);
		
		/* count number of AstNode vertices for Module ID */
		final Long countResult = astService.count(q -> q.ofModule(moduleId));
		assertEquals(0, countResult);
		final int offset = 0;
		Assert.assertThrows(MiningEntityNotFoundException.class, () -> astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_2, moduleId, offset, false));
	}
	
	@Test
	void testGetFormatIfSelectionThrowsExceptionIfCursorIsNotPlacedOnFieldName() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH);
		
		/* count number of AstNode vertices for Module ID */
		final Long countResult = astService.count(q -> q.ofModule(moduleId));
		assertEquals(0, countResult);
		
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleId);
		final int offset = 2718;
		Assert.assertThrows(ConstraintViolationException.class, () -> astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, moduleId, offset, false));
	}
	
	@Test
	void testCopyBookFieldName() {
		final EntityId moduleIdA = createCobolCopybook(PROJECT_ID_1, "DFHAID", "DFHAID.cpy", RESOURCE_PATH);
		final EntityId moduleIdB = createCobolProgram(PROJECT_ID_1, "MMRS71B1", "MMRS71B1.cbl", RESOURCE_PATH);
		createReference(RelationshipType.INCLUDES, moduleIdB, moduleIdA);
		final List<DataFieldFormat> dataFieldFormat = new ArrayList<>();
		
		/* store AST for Module ID */
//		executorService.executeStoreAst(PROJECT_ID_1, moduleIdA);
		executorService.executeStoreAst(PROJECT_ID_1, moduleIdB);
		
		/* selects offset and label from AstNode for Module ID */
		for (final var node : astService.find(q -> q.ofModule(moduleIdB).withSuperTypes("FieldDefinition"))) {
			if (node.getLabel().equals(TEST_ASTNODE_LABEL2)) {
				Integer retracedOffset = node.getLocation().getRetracedOffset().orElseThrow();
				int additionalOffset = node.getLabel().indexOf(FIELD_NAME2) + 1;
				dataFieldFormat.add(astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, moduleIdA,
						retracedOffset + additionalOffset, false));
			}
		}
		assertEquals(1, dataFieldFormat.size());
		assertEquals(FIELD_NAME2, dataFieldFormat.get(0).getFieldName());
		assertEquals(DefinedLocation.COPYBOOK, dataFieldFormat.get(0).getDefinedLocation());
	}
	
	@Test
	void testCorrectDefinedLocationForPl1ManualDDECreation() {
		final EntityId beginModule = createModule(PROJECT_ID_1, "BeginLocation", "BeginLocation.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		final EntityId packageModule = createModule(PROJECT_ID_1, "packageLocation", "packageLocation.pl1", RESOURCE_PATH_PL1, Technology.PL1, Type.PROGRAM);
		
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, beginModule);
		executorService.executeStoreAst(PROJECT_ID_1, packageModule);
		
		final DataFieldFormat begin = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, beginModule, 244, false);
		final DataFieldFormat proc = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, beginModule, 81, false);
		final DataFieldFormat pack = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, packageModule, 53, false);
		
		assertEquals(DefinedLocation.BEGIN, begin.getDefinedLocation());
		assertEquals(DefinedLocation.PROCEDURE, proc.getDefinedLocation());
		assertEquals(DefinedLocation.PACKAGE, pack.getDefinedLocation());
		assertEquals("A", begin.getFieldName());
		assertEquals("X", proc.getFieldName());
		assertEquals("IO_PTR_PCB", pack.getFieldName());
		assertEquals("CHAR", begin.getLanguageType());
		assertEquals("CHAR", proc.getLanguageType());
		assertEquals("POINTER", pack.getLanguageType());
	}
	
	@Test
	void testCorrectDefinedLocationForNaturalManualDDECreation() {
		final EntityId programModule = createModule(PROJECT_ID_1, "MAIN1", "MAIN1.nsp", RESOURCE_PATH, Technology.NATURAL, Type.PROGRAM);
		final EntityId subProgModule = createModule(PROJECT_ID_1, "SUB1", "SUB1.nsn", RESOURCE_PATH, Technology.NATURAL, Type.SUBPROGRAM);
		final EntityId subRoutModule = createModule(PROJECT_ID_1, "SUBR1", "SUBR1.nss", RESOURCE_PATH, Technology.NATURAL, Type.SUBROUTINE);
		
		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, programModule);
		executorService.executeStoreAst(PROJECT_ID_1, subProgModule);
		executorService.executeStoreAst(PROJECT_ID_1, subRoutModule);
		
		final DataFieldFormat program = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, programModule, 110, false);
		final DataFieldFormat subProg = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, subProgModule, 25, false);
		final DataFieldFormat subRout = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID_1, subRoutModule, 34, false);
		
		assertEquals(DefinedLocation.PROGRAM, program.getDefinedLocation());
		assertEquals(DefinedLocation.SUBPROGRAM, subProg.getDefinedLocation());
		assertEquals(DefinedLocation.SUBROUTINE, subRout.getDefinedLocation());
		assertEquals("#PROG2", program.getFieldName());
		assertEquals("#A", subProg.getFieldName());
		assertEquals("#A", subRout.getFieldName());
		assertEquals("A", program.getLanguageType());
		assertEquals("A", subProg.getLanguageType());
		assertEquals("A", subRout.getLanguageType());
	}

	@Test
	void testIfParserHasError() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "parseError", "CBL_PARS_ERROR.cbl", RESOURCE_PATH);

		/* store AST for Module ID */
		executorService.executeStoreAst(PROJECT_ID_1, moduleId);

		/* count number of AstNode vertices for Module ID */
		final Long countResult = astService.count(q -> q.ofModule(moduleId));
		/* this program has parser error and it should not save AstNode */
		assertEquals(0, countResult);
	}
}
