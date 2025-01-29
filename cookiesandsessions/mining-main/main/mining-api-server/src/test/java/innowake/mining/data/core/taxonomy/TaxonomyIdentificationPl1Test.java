/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static innowake.mining.shared.model.RelationshipType.CALLS;
import static innowake.mining.shared.model.Technology.ECL;
import static innowake.mining.shared.model.Technology.IMS;
import static innowake.mining.shared.model.Technology.NONE;
import static innowake.mining.shared.model.Technology.PL1;
import static innowake.mining.shared.model.Type.APPLICATION;
import static innowake.mining.shared.model.Type.ECL_JOB;
import static innowake.mining.shared.model.Type.MAINPROGRAM;
import static innowake.mining.shared.model.Type.MFS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import innowake.mining.server.integration.DatabaseRelatedTest;

import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests regarding Taxonomy identification for PL1.
 */
class TaxonomyIdentificationPl1Test extends DatabaseRelatedTest {

	private final static String IRRELEVANT_MODULE_NAME = "Irrelevant";

	@Autowired
	private TechnicalTaxonomyIdentifier technicalTaxonomyIdentifier;
	
	@Test
	void batchPl1TaxonomyWithIncomingDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule incomingCall = new DefaultDependecyModule(EntityId.of(1L), ECL, ECL_JOB, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(incomingCall, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.BATCH, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiPl1TaxonomyWithOnlyIncomingDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule incomingCall = new DefaultDependecyModule(EntityId.of(1L), IMS, APPLICATION, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(incomingCall, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.LIBRARY, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiPl1TaxonomyWithOnlyOutgoingDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule outgoingCall = new DefaultDependecyModule(EntityId.of(1L), NONE, MFS, IRRELEVANT_MODULE_NAME, null);
		module.addOutgoing(outgoingCall, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.LIBRARY, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiTaxonomyWithIncomingOutgoingDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule incomingCall = new DefaultDependecyModule(EntityId.of(1L), IMS, APPLICATION, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule outgoingCall = new DefaultDependecyModule(EntityId.of(1L), NONE, MFS, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(incomingCall, CALLS);
		module.addOutgoing(outgoingCall, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void taxonomyWithIncomingOutgoingDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule incomingCallB = new DefaultDependecyModule(EntityId.of(1L), ECL, ECL_JOB, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule incomingCallU = new DefaultDependecyModule(EntityId.of(1L), IMS, APPLICATION, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule outgoingCall = new DefaultDependecyModule(EntityId.of(1L), NONE, MFS, IRRELEVANT_MODULE_NAME, null);
		module.addIncoming(incomingCallB, CALLS);
		module.addIncoming(incomingCallU, CALLS);
		module.addOutgoing(outgoingCall, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(2, identifiedTaxonomies.size());
		assertTrue(identifiedTaxonomies.stream()
				.map(x -> x.e1)
				.allMatch(name -> Arrays.asList(Name.UI, Name.BATCH).contains(name)));
	}

	@Override
	protected void resetTestData() throws IOException {
		/* Do nothing */
	}
}
