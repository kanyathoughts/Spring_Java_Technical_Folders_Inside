/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static innowake.mining.shared.model.ReferenceAttributes.DB_ACCESS_TYPES;
import static innowake.mining.shared.model.RelationshipType.ACCESSES;
import static innowake.mining.shared.model.RelationshipType.CALLS;
import static innowake.mining.shared.model.RelationshipType.INCLUDES;
import static innowake.mining.shared.model.RelationshipType.REFERENCES;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.DELETE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.LIBRARY;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.MQ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.STORE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UPDATE;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.DB_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;
import static innowake.mining.shared.model.Technology.BASIC;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Technology.NATURAL;
import static innowake.mining.shared.model.Technology.PL1;
import static innowake.mining.shared.model.Technology.RESOURCE;
import static innowake.mining.shared.model.Technology.UNKNOWN;
import static innowake.mining.shared.model.Type.COPYBOOK;
import static innowake.mining.shared.model.Type.MAINPROGRAM;
import static innowake.mining.shared.model.Type.PROGRAM;
import static innowake.mining.shared.model.Type.SUBPROGRAM;
import static innowake.mining.shared.model.Type.TABLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.mining.server.integration.DatabaseRelatedTest;

import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.data.core.taxonomy.impl.DatabaseAccessTaxonomy;
import innowake.mining.data.core.taxonomy.impl.DependencyBasedUiProgramTaxonomyIdentifier;
import innowake.mining.data.core.taxonomy.impl.MqProgramTaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests regarding TaxonomyPojo identification.
 */
class TaxonomyIdentificationTest extends DatabaseRelatedTest {
	
	@Autowired
	private TechnicalTaxonomyIdentifier technicalTaxonomyIdentifier;
	
	private final static String IRRELEVANT_MODULE_NAME = "Irrelevant";

	@Test
	void mqProgramWithMqSet() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQSET");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}
	
	@Test
	void noMqProgramWhenNotCalled() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQSET");
		module.addOutgoing(mqProgram, REFERENCES);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new MqProgramTaxonomyIdentifier();
		final List<Tuple2<Name, TypeName>> identified = identifier.identify(module);
		assertEquals(0, identified.size());
	}

	@Test
	void noMqProgramWhenIncomingCall() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQSET");
		module.addIncoming(mqProgram, CALLS);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new MqProgramTaxonomyIdentifier();
		final List<Tuple2<Name, TypeName>> identified = identifier.identify(module);
		assertEquals(0, identified.size());
	}
	
	@Test
	void noMqProgramWhenNotAProgram() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, COPYBOOK, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQSET");
		module.addOutgoing(mqProgram, CALLS);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new MqProgramTaxonomyIdentifier();
		final List<Tuple2<Name, TypeName>> identified = identifier.identify(module);
		assertEquals(0, identified.size());
	}
	
	@Test
	void mqProgramWhenMainProgram() {
		final DefaultDependecyModule module = new DefaultDependecyModule(PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQSET");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQINQ() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQINQ");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQGET() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQGET");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQPUT1() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQPUT1");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQPUT() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQPUT");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQCLOSE() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQCLOSE");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQOPEN() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQOPEN");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQDISC() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQDISC");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}

	@Test
	void mqProgramMQCONN() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQCONN");
		module.addOutgoing(mqProgram, CALLS);
		
		assertExpectedMqProgramTaxonomies(technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty()));
	}
	
	@Test
	void noMqProgramWithMqPrefix() {
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule mqProgram = new DefaultDependecyModule(UNKNOWN, PROGRAM, "MQprefixmodulename");
		module.addOutgoing(mqProgram, CALLS);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new MqProgramTaxonomyIdentifier();
		assertEquals(0, identifier.identify(module).size());
	}
	
	@Test
	void dbAccess() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.READ.name());
		module.addOutgoing(table, ACCESSES, properties);
		
		final List<Tuple2<Name, TypeName>> taxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty());
		assertEquals(2, taxonomies.size(), "Identified list must have 2 entries");

		for (final Iterator<Tuple2<Name, TypeName>> iterator = taxonomies.iterator(); iterator.hasNext();) {
			final Tuple2<Name, TypeName> taxonomy = iterator.next();
			assertNotNull(taxonomy);
			if (DB_ACCESS.equals(taxonomy.e2) && READ.equals(taxonomy.e1) 
					|| PROGRAM_TYPE.equals(taxonomy.e2) && LIBRARY.equals(taxonomy.e1)) {
				iterator.remove();
			}
		}
		assertEquals(0, taxonomies.size(), "List must be empty at end of test");
	}
	
	@Test
	void dbAccessInsert() {
		final DefaultDependecyModule module = new DefaultDependecyModule(PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.STORE.name());
		module.addOutgoing(table, ACCESSES, properties);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		final List<Tuple2<Name, TypeName>> taxonomies = identifier.identify(module);
		assertEquals(1, taxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> taxonomy = taxonomies.get(0);
		assertNotNull(taxonomy);
		assertEquals(STORE, taxonomy.e1);
		assertEquals(DB_ACCESS, taxonomy.e2);
	}
	
	@Test
	void dbAccessMultipleTaxonomiesFromOneRelationship() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), String.format("%s,%s", DatabaseAccessType.READ.name(), DatabaseAccessType.UPDATE.name()));
		module.addOutgoing(table, ACCESSES, properties);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		final List<Tuple2<Name, TypeName>> taxonomies = identifier.identify(module);
		assertEquals(2, taxonomies.size(), "Identified list must have 2 entry");
		for (final Iterator<Tuple2<Name, TypeName>> iterator = taxonomies.iterator(); iterator.hasNext();) {
			final Tuple2<Name, TypeName> taxonomy = iterator.next();
			assertNotNull(taxonomy);
			assertEquals(DB_ACCESS, taxonomy.e2);
			if (READ.equals(taxonomy.e1) || UPDATE.equals(taxonomy.e1)) {
				iterator.remove();
			}
		}
		assertEquals(0, taxonomies.size(), "List must be empty at end of test");
	}
	
	@Test
	void dbAccessMultipleTaxonomiesFromMultipleRelationship() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table1 = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final DependencyModule table2 = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME + "2");

		final Map<String, Object> properties1 = new HashMap<>();
		properties1.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), String.format("%s,%s", DatabaseAccessType.READ.name(), DatabaseAccessType.UPDATE.name()));
		module.addOutgoing(table1, ACCESSES, properties1);

		final Map<String, Object> properties2 = new HashMap<>();
		properties2.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.DELETE.name());
		module.addOutgoing(table2, ACCESSES, properties2);
		
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		final List<Tuple2<Name, TypeName>> taxonomies = identifier.identify(module);
		assertEquals(3, taxonomies.size(), "Identified list must have 3 entry");
		for (final Iterator<Tuple2<Name, TypeName>> iterator = taxonomies.iterator(); iterator.hasNext();) {
			final Tuple2<Name, TypeName> taxonomy = iterator.next();
			assertNotNull(taxonomy);
			assertEquals(DB_ACCESS, taxonomy.e2);
			if (UPDATE.equals(taxonomy.e1) 
					|| DELETE.equals(taxonomy.e1)
					|| READ.equals(taxonomy.e1)) {
				iterator.remove();
			}
		}
		assertEquals(0, taxonomies.size(), "List must be empty at end of test");
	}
	
	@Test
	void noDbAccessWithoutReadsWritesRelationship() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.READ.name());
		module.addOutgoing(table, REFERENCES, properties);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		assertEquals(0, identifier.identify(module).size(), "Identified list must be empty");
	}
	
	@Test
	void noDbAccessWithoutProgram() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, COPYBOOK, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.READ.name());
		module.addOutgoing(table, ACCESSES, properties);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		assertEquals(0, identifier.identify(module).size(), "Identified list must be empty");
	}

	@Test
	void noDbAccessWithEmptyProperty() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		final Map<String, Object> properties = new HashMap<>();
		properties.put(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), "");
		module.addOutgoing(table, ACCESSES, properties);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		assertEquals(0, identifier.identify(module).size(), "Identified list must be empty");
	}

	@Test
	void noSqlAccessWithoutAccessTypesProperties() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule table = new DefaultDependecyModule(RESOURCE, TABLE, IRRELEVANT_MODULE_NAME);
		module.addOutgoing(table, ACCESSES);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DatabaseAccessTaxonomy();
		assertEquals(0, identifier.identify(module).size(), "Identified list must be empty");
	}

	@Test
	void libraryProgram() {
		final DefaultDependecyModule module = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule callingProgram = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME + "2");
		final DependencyModule copybook = new DefaultDependecyModule(COBOL, COPYBOOK, IRRELEVANT_MODULE_NAME);
		module.addIncoming(callingProgram, CALLS);
		module.addOutgoing(copybook, INCLUDES);
		
		final List<Tuple2<Name, TypeName>> taxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty());
		assertEquals(1, taxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> identifiedTaxonomy = taxonomies.get(0);
		assertEquals(LIBRARY, identifiedTaxonomy.e1);
		assertEquals(PROGRAM_TYPE, identifiedTaxonomy.e2);
	}
	
	@Test
	void libraryProgramWhenMainProgram() {
		final DependencyModule callingProgram = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DefaultDependecyModule module = new DefaultDependecyModule(PL1, MAINPROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule copybook = new DefaultDependecyModule(COBOL, COPYBOOK, IRRELEVANT_MODULE_NAME + "2");
		module.addIncoming(callingProgram, CALLS);
		module.addOutgoing(copybook, INCLUDES);
		
		final List<Tuple2<Name, TypeName>> taxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty());
		assertEquals(1, taxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> identifiedTaxonomy = taxonomies.get(0);
		assertEquals(LIBRARY, identifiedTaxonomy.e1);
		assertEquals(PROGRAM_TYPE, identifiedTaxonomy.e2);
	}
	
	@Test
	void libraryProgramWhenSubProgram() {
		final DependencyModule callingProgram = new DefaultDependecyModule(NATURAL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DefaultDependecyModule module = new DefaultDependecyModule(NATURAL, SUBPROGRAM, IRRELEVANT_MODULE_NAME + "SUB");
		final DependencyModule copybook = new DefaultDependecyModule(NATURAL, Type.COPYCODE, IRRELEVANT_MODULE_NAME + "2");
		module.addIncoming(callingProgram, CALLS);
		module.addOutgoing(copybook, INCLUDES);
		
		final List<Tuple2<Name, TypeName>> taxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(), Optional.empty());
		assertEquals(1, taxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> identifiedTaxonomy = taxonomies.get(0);
		assertEquals(LIBRARY, identifiedTaxonomy.e1);
		assertEquals(PROGRAM_TYPE, identifiedTaxonomy.e2);
	}
	
	@Test
	void uiTaxonomyWithByDependencyModuleInvalidCases() {
		final DefaultDependecyModule basicProgram = new DefaultDependecyModule(BASIC, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule ifdlModule = new DefaultDependecyModule(Technology.VMS, Type.IFDL_FORM, IRRELEVANT_MODULE_NAME);
		final DefaultDependecyModule cobolProgram = new DefaultDependecyModule(COBOL, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule formEnable = new DefaultDependecyModule(Technology.NONE, Type.UNKNOWN, "FORMS$ENABLE");
		basicProgram.addIncoming(ifdlModule, CALLS);
		cobolProgram.addIncoming(formEnable, CALLS);
		cobolProgram.addIncoming(ifdlModule, CALLS);
		
		final TaxonomyIdentifier<DependencyModule> identifier = new DependencyBasedUiProgramTaxonomyIdentifier();
		List<Tuple2<Name, TypeName>> identifiedTaxonomies = identifier.identify(basicProgram);
		assertEquals(0, identifiedTaxonomies.size());
		identifiedTaxonomies = identifier.identify(cobolProgram);
		assertEquals(0, identifiedTaxonomies.size());
	}

	@Override
	protected void resetTestData() throws IOException {
		/* Do nothing */
	}

	private void assertExpectedMqProgramTaxonomies(final List<Tuple2<Name, TypeName>> taxonomies) {
		assertExpectedTaxonomies(taxonomies, MQ, PROGRAM_TYPE);
	}

	private void assertExpectedTaxonomies(final List<Tuple2<Name, TypeName>> taxonomies, final Name mq, final TypeName programType) {
		assertEquals(1, taxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> taxonomy = taxonomies.get(0);
		assertNotNull(taxonomy);
		assertEquals(mq, taxonomy.e1);
		assertEquals(programType, taxonomy.e2);
	}

}
