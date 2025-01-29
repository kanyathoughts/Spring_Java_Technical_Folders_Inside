/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static innowake.mining.shared.model.RelationshipType.CALLS;
import static innowake.mining.shared.model.RelationshipType.REFERENCES;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.BATCH;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;
import static innowake.mining.shared.model.Technology.BASIC;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Technology.JCL;
import static innowake.mining.shared.model.Technology.NATURAL;
import static innowake.mining.shared.model.Technology.PL1;
import static innowake.mining.shared.model.Technology.VMS;
import static innowake.mining.shared.model.Type.DCL;
import static innowake.mining.shared.model.Type.JOB;
import static innowake.mining.shared.model.Type.MAINPROGRAM;
import static innowake.mining.shared.model.Type.PROC;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import innowake.mining.server.integration.DatabaseRelatedTest;

import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.data.core.taxonomy.impl.BatchProgramTaxonomyIdentifier;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests regarding Taxonomy identification for JCL.
 */
class TaxonomyIdentificationJclTest extends DatabaseRelatedTest {
	
	private final static String IRRELEVANT_MODULE_NAME = "Irrelevant";

	@Autowired
	private TechnicalTaxonomyIdentifier technicalTaxonomyIdentifier;

	@Test
	void batchProgram() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), COBOL, PROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule callingJob = new DefaultDependecyModule(EntityId.of(1L), JCL, JOB, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(callingJob, CALLS);
		
		assertExpectedBatchProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}
	
	@Test
	void batchProgramMustBeAnActualProgramRegardlessOfTechnology() {
		final DefaultDependecyModule copycodeModule = new DefaultDependecyModule(NATURAL, Type.COPYCODE, IRRELEVANT_MODULE_NAME);
		final DependencyModule copycodeCallingJob = new DefaultDependecyModule(JCL, JOB, IRRELEVANT_MODULE_NAME);
		/* This constellation is bogus but technically possible */
		copycodeModule.addIncoming(copycodeCallingJob, CALLS);
		
		final List<Tuple2<Name, TypeName>> firstIdentifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(copycodeModule), Optional.empty()
				, Optional.empty());
		assertEquals(0, firstIdentifiedTaxonomies.size());
		
		final DefaultDependecyModule programModule = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule callingJob = new DefaultDependecyModule(JCL, JOB, IRRELEVANT_MODULE_NAME);
		programModule.addIncoming(callingJob, CALLS);
		
		assertExpectedBatchProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(programModule), Optional.empty(), Optional.empty()));
	}
	
	@Test
	void batchProgramWithPl1MainProgram() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule referencingJclJob = new DefaultDependecyModule(EntityId.of(1L), JCL, JOB, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(referencingJclJob, REFERENCES);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new BatchProgramTaxonomyIdentifier();
		final List<Tuple2<Name, TypeName>> firstIdentifiedTaxonomies = identifier.identify(module);
		assertEquals(0, firstIdentifiedTaxonomies.size());
		
		final DependencyModule callingJclProc = new DefaultDependecyModule(JCL, PROC, IRRELEVANT_MODULE_NAME);
		module.addIncoming(callingJclProc, CALLS);
		assertExpectedBatchProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}
	
	@Test
	void batchProgramWithBasicProgram() {
		/* Incoming call VMS DCL should be identified */
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), BASIC, PROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule callingProgram = new DefaultDependecyModule(EntityId.of(1L), VMS, DCL, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(callingProgram, CALLS);
		
		final List<Tuple2<Name, TypeName>> firstIdentifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, firstIdentifiedTaxonomies.size());
	}
	
	@Test
	void batchProgramIncomingCallMustBeDCLVMSForBasic() {
		/* must be VMS and DCL otherwise unidentified */
		final DefaultDependecyModule module = new DefaultDependecyModule(BASIC, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule callingProgram1 = new DefaultDependecyModule(VMS, PROGRAM, IRRELEVANT_MODULE_NAME);
		module.addIncoming(callingProgram1, CALLS);
		
		/* must be VMS and DCL otherwise unidentified */
		final DependencyModule callingProgram2 = new DefaultDependecyModule(NATURAL, DCL, IRRELEVANT_MODULE_NAME);
		module.addIncoming(callingProgram2, CALLS);
		
		/* must not be an outgoing call */
		final DependencyModule callingProgram3 = new DefaultDependecyModule(VMS, DCL, IRRELEVANT_MODULE_NAME);
		module.addOutgoing(callingProgram3, CALLS);
		
		/* must not be a reference */
		final DependencyModule callingProgram4 = new DefaultDependecyModule(VMS, DCL, IRRELEVANT_MODULE_NAME);
		module.addOutgoing(callingProgram4, REFERENCES);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new BatchProgramTaxonomyIdentifier();
		final List<Tuple2<Name, TypeName>> firstIdentifiedTaxonomies = identifier.identify(module);
		assertEquals(0, firstIdentifiedTaxonomies.size());
	}
	
	@Test
	void batchProgramIncomingCallMustBeJcl() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule callingProgram = new DefaultDependecyModule(EntityId.of(1L), NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(callingProgram, CALLS);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new BatchProgramTaxonomyIdentifier();
		final List<Tuple2<Name, TypeName>> firstIdentifiedTaxonomies = identifier.identify(module);
		assertEquals(0, firstIdentifiedTaxonomies.size());
		
		/* add incoming JCL reference and still nothing should be identified */
		final DependencyModule referencingJclProc = new DefaultDependecyModule(JCL, PROC, IRRELEVANT_MODULE_NAME);
		module.addIncoming(referencingJclProc, REFERENCES);
		final List<Tuple2<Name, TypeName>> secondIdentifiedTaxonomies = identifier.identify(module);
		assertEquals(0, secondIdentifiedTaxonomies.size());

		/* add incoming JCL call, which now should be identified */
		final DependencyModule referencingJclJob = new DefaultDependecyModule(JCL, JOB, IRRELEVANT_MODULE_NAME);
		module.addIncoming(referencingJclJob, CALLS);
		assertExpectedBatchProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Override
	protected void resetTestData() throws IOException {
		/* Do nothing */
	}
	
	private void assertExpectedBatchProgramTaxonomies(final List<Tuple2<Name, TypeName>> taxonomies) {
		assertExpectedTaxonomies(taxonomies, BATCH, PROGRAM_TYPE);
	}
	
	private void assertExpectedTaxonomies(final List<Tuple2<Name, TypeName>> taxonomies, final Name batch, final TypeName programType) {
		assertEquals(1, taxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> taxonomy = taxonomies.get(0);
		assertNotNull(taxonomy);
		assertEquals(batch, taxonomy.e1);
		assertEquals(programType, taxonomy.e2);
	}

}
