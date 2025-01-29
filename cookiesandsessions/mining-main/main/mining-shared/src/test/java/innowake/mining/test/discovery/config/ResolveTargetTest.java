/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test.discovery.config;

import static innowake.mining.shared.model.discovery.ResolveTarget.ASSEMBLER;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL_COPYBOOK;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL_PROGRAM;
import static innowake.mining.shared.model.discovery.ResolveTarget.JCL;
import static innowake.mining.shared.model.discovery.ResolveTarget.JCL_JOB;
import static innowake.mining.shared.model.discovery.ResolveTarget.JCL_PROC;
import static innowake.mining.shared.model.discovery.ResolveTarget.LANGUAGE;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_GDA;
import static innowake.mining.shared.model.discovery.ResolveTarget.NONE;
import static innowake.mining.shared.model.discovery.ResolveTarget.PL1;
import static innowake.mining.shared.model.discovery.ResolveTarget.PL1_COPYBOOK;
import static innowake.mining.shared.model.discovery.ResolveTarget.PL1_PROGRAM;
import static innowake.mining.shared.model.discovery.ResolveTarget.RESOURCE_TPFDF_DATASET;
import static innowake.mining.shared.model.discovery.ResolveTarget.RESOURCE;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.Test;

import innowake.mining.shared.discovery.config.core.XMLResolveTarget;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

public class ResolveTargetTest {
	
	@Test
	public void testGetPath() {
		assertEquals(asList(COBOL), COBOL.getRootChain());
		assertEquals(asList(COBOL, COBOL_PROGRAM), COBOL_PROGRAM.getRootChain());
		assertEquals(emptyList(), LANGUAGE.getRootChain());
		assertEquals(asList(NONE), NONE.getRootChain());
		assertEquals(asList(NATURAL, NATURAL_GDA), NATURAL_GDA.getRootChain());
		assertEquals(asList(JCL), JCL.getRootChain());
		assertEquals(asList(PL1), PL1.getRootChain());
		assertEquals(asList(PL1, PL1_PROGRAM), PL1_PROGRAM.getRootChain());
		assertEquals(asList(PL1, PL1_COPYBOOK), PL1_COPYBOOK.getRootChain());
		assertEquals(asList(RESOURCE, RESOURCE_TPFDF_DATASET), RESOURCE_TPFDF_DATASET.getRootChain());
	}
	
	@Test
	public void testQualifiedName() {
		assertEquals("ASSEMBLER", new XMLResolveTarget(ASSEMBLER).getQualifiedId());
		assertEquals("COBOL",  new XMLResolveTarget(COBOL).getQualifiedId());
		assertEquals("COBOL/PROGRAM",  new XMLResolveTarget(COBOL_PROGRAM).getQualifiedId());
		assertEquals("JCL/PROC",  new XMLResolveTarget(JCL_PROC).getQualifiedId());
		assertEquals("NATURAL/GDA",  new XMLResolveTarget(NATURAL_GDA).getQualifiedId());
		assertEquals("PL1/PROGRAM",  new XMLResolveTarget(PL1_PROGRAM).getQualifiedId());
		assertEquals("PL1/COPYBOOK",  new XMLResolveTarget(PL1_COPYBOOK).getQualifiedId());
	}
	
	@Test
	public void testForName() {
		assertEquals(ASSEMBLER, XMLResolveTarget.forQualifiedId("ASSEMBLER").get());
		assertEquals(COBOL, XMLResolveTarget.forQualifiedId("COBOL").get());
		assertEquals(COBOL_PROGRAM, XMLResolveTarget.forQualifiedId("COBOL/PROGRAM").get());
		assertEquals(JCL_PROC, XMLResolveTarget.forQualifiedId("JCL/PROC").get());
		assertEquals(NATURAL_GDA, XMLResolveTarget.forQualifiedId("NATURAL/GDA").get());
		assertEquals(PL1, XMLResolveTarget.forQualifiedId("PL1").get());
		assertEquals(PL1_PROGRAM, XMLResolveTarget.forQualifiedId("PL1/PROGRAM").get());
		assertEquals(PL1_COPYBOOK, XMLResolveTarget.forQualifiedId("PL1/COPYBOOK").get());
	}
	
	@Test
	public void testGetLanguage() {
		assertEquals(ASSEMBLER, ASSEMBLER.getLanguage());
		assertEquals(NATURAL, NATURAL.getLanguage());
		assertEquals(COBOL, COBOL_COPYBOOK.getLanguage());
		assertEquals(JCL, JCL_JOB.getLanguage());
		assertEquals(NONE, NONE.getLanguage());
		assertEquals(NONE, LANGUAGE.getLanguage());
		assertEquals(PL1, PL1_PROGRAM.getLanguage());
		assertEquals(PL1, PL1_COPYBOOK.getLanguage());
	}
	
	@Test
	public void testNaturalReportingMode() {
		assertEquals(ResolveTarget.NATURAL_PROGRAM_REPORTING, ResolveTarget.NATURAL_PROGRAM.getChildren().iterator().next());
		assertEquals(ResolveTarget.NATURAL_SUBPROGRAM_REPORTING, ResolveTarget.NATURAL_SUBPROGRAM.getChildren().iterator().next());
		assertEquals(ResolveTarget.NATURAL_SUBROUTINE_REPORTING, ResolveTarget.NATURAL_SUBROUTINE.getChildren().iterator().next());
		assertEquals(ResolveTarget.NATURAL_FUNCTION_REPORTING, ResolveTarget.NATURAL_FUNCTION.getChildren().iterator().next());
		assertEquals(ResolveTarget.NATURAL_COPYCODE_REPORTING, ResolveTarget.NATURAL_COPYCODE.getChildren().iterator().next());
		assertEquals(ResolveTarget.NATURAL_HELP_REPORTING, ResolveTarget.NATURAL_HELP.getChildren().iterator().next());
		assertEquals(ResolveTarget.NATURAL_MAP_REPORTING, ResolveTarget.NATURAL_MAP.getChildren().iterator().next());
	}
	
	@Test
	public void testModuleReferenceType() {
		for (final ResolveTarget resolveTarget : ResolveTarget.values()) {
			final Technology technology = ResolveTargetHelper.toTechnology(resolveTarget);
			final Type type = ResolveTargetHelper.toType(resolveTarget);

			assertDoesNotThrow(() -> RelationshipType.from(technology, type),
					String.format("ModuleReferenceType does not exists for the technology: %s, type: %s", technology, type));
		}
	}
	
	@Test
	public void testStorage() {
		for (final ResolveTarget resolveTarget : ResolveTarget.values()) {
			final Technology technology = ResolveTargetHelper.toTechnology(resolveTarget);
			final Type type = ResolveTargetHelper.toType(resolveTarget);

			assertDoesNotThrow(() -> Storage.from(technology, type),
					String.format("Storage does not exists for the technology: %s, type: %s", technology, type));
		}
	}

}
