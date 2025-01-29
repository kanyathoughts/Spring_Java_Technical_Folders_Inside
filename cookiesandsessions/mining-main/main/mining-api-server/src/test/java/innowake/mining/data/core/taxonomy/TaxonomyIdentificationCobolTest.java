/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static innowake.mining.shared.model.RelationshipType.CALLS;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.WRITE;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.FILE_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import innowake.mining.data.core.storeast.impl.AbstractCobolTest;
import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.data.core.taxonomy.impl.*;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Tests regarding TaxonomyPojo identification for Cobol.
 */
class TaxonomyIdentificationCobolTest extends AbstractCobolTest {
	
	private final static String UI_FOLDER_NAME = "ui";
	private final static String FILE_ACCESS_FOLDER_NAME = "file-access";
	private final static String IRRELEVANT_MODULE_NAME = "Irrelevant";

	private static TechnicalTaxonomyIdentifier technicalTaxonomyIdentifier;

	@BeforeAll
	public static void setUp() {
		final TaxonomyIdentifier<AstNodePojo> fileAccessIdentifier = new FileAccessTaxonomyIdentifier();
		final TaxonomyIdentifier<AstNodePojo> uiProgIdentifier = new UiProgramTaxonomyIdentifier();
		final TaxonomyIdentifier<DependencyModule> batchProgIdentifier = new BatchProgramTaxonomyIdentifier();
		final TaxonomyIdentifier<DependencyModule> dbAccessIdentifier = new DatabaseAccessTaxonomy();
		final TaxonomyIdentifier<DependencyModule> dependencyBasedUiProgIdentifier = new DependencyBasedUiProgramTaxonomyIdentifier();
		final TaxonomyIdentifier<DependencyModule> mqProgIdentifier = new MqProgramTaxonomyIdentifier();

		technicalTaxonomyIdentifier = new TechnicalTaxonomyIdentifier();
		ReflectionTestUtils.setField(technicalTaxonomyIdentifier, "astBasedIdentifiers", List.of(fileAccessIdentifier,
				uiProgIdentifier));
		ReflectionTestUtils.setField(technicalTaxonomyIdentifier, "dependencyBasedIdentifiers",
				List.of(batchProgIdentifier, dbAccessIdentifier, dependencyBasedUiProgIdentifier, mqProgIdentifier));
	}

	@Test
	void uiProgramTypeNotFoundInEmptyProgram() {
		final String fileName = "WMIN537UI0.cbl";
		final AstNodePojo rootNode = createAst(UI_FOLDER_NAME, fileName).get(null);
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(
				Optional.empty(), Optional.of(rootNode), Optional.empty());
		assertEquals(0, identifiedTaxonomies.size());
	}

	@Test
	void uiProgramTypeOnAllRelevantExecCicsStatements() {
		testUiProgramType("WMIN537UI1.cbl");
	}

	@Test
	void uiProgramTypeOnExecCicsSendPage() {
		testUiProgramType("WMIN537UI2.cbl");
	}
	
	@Test
	void uiProgramTypeOnExecCicsSendMap() {
		testUiProgramType("WMIN537UI3.cbl");
	}

	@Test
	void uiProgramTypeOnExecCicsSendControl() {
		testUiProgramType("WMIN537UI4.cbl");
	}

	@Test
	void uiProgramTypeOnExecCicsSendText() {
		testUiProgramType("WMIN537UI5.cbl");
	}
	
	@Test
	void uiProgramTypeOnExecCicsReceiveMap() {
		testUiProgramType("WMIN537UI6.cbl");
	}

	@Test
	void uiProgramTypeOnExecCicsReceive() {
		testUiProgramType("WMIN537UI7.cbl");
	}

	@Test
	void uiProgramTypeOnExecCicsSend() {
		testUiProgramType("WMIN537UI8.cbl");
	}
	
	@Test
	void fileAccessOnCobolRead() {
		testFileAccess("WMIN539FILE1.cbl", READ);
	}
	
	@Test
	void fileAccessOnCobolWrite() {
		testFileAccess("WMIN539FILE2.cbl", WRITE);
	}

	@Test
	void fileAccessOnCobolRewrite() {
		testFileAccess("WMIN539FILE3.cbl", WRITE);
	}

	@Test
	void fileAccessOnCobolDelete() {
		testFileAccess("WMIN539FILE4.cbl", WRITE);
	}
	
	@Test
	void fileAccessOnMultipleCobolReads() {
		testFileAccess("WMIN539FILE5.cbl", READ);
	}
	
	@Test
	void fileAccessOnMultipleCobolWrites() {
		testFileAccess("WMIN539FILE6.cbl", WRITE);
	}
	
	@Test
	void fileAccessOnCobolReadAndWrite() {
		testFileAccess("WMIN539FILE7.cbl", READ, WRITE);
	}

	@Test
	void fileAccessOnMultipleCobolReadsAndWrites() {
		testFileAccess("WMIN539FILE8.cbl", READ, WRITE);
	}
	
	@Test
	void fileAccessOnExecCicsFileRead() {
		testFileAccess("WMIN539FILE9.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileReadNext() {
		testFileAccess("WMIN539FILE10.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileReadPrev() {
		testFileAccess("WMIN539FILE11.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileStartBr() {
		testFileAccess("WMIN539FILE12.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileEndBr() {
		testFileAccess("WMIN539FILE13.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileResetBr() {
		testFileAccess("WMIN539FILE14.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileUnlock() {
		testFileAccess("WMIN539FILE15.cbl", READ);
	}

	@Test
	void fileAccessOnExecCicsFileWrite() {
		testFileAccess("WMIN539FILE16.cbl", WRITE);
	}

	@Test
	void fileAccessOnExecCicsFileReWrite() {
		testFileAccess("WMIN539FILE17.cbl", WRITE);
	}

	@Test
	void fileAccessOnExecCicsFileDelete() {
		testFileAccess("WMIN539FILE18.cbl", WRITE);
	}

	@Test
	void fileAccessOnExecCicsFileReadAndWrite() {
		testFileAccess("WMIN539FILE19.cbl", WRITE, READ);
	}

	@Test
	void fileAccessOnCobolAndExecCicsFile() {
		testFileAccess("WMIN539FILE20.cbl", WRITE, READ);
	}

	@Test
	void uiTaxonomyWithCobolProgramCallsIcscrgetByDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), COBOL, PROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule programBeingCalled = new DefaultDependecyModule(EntityId.of(1L), Technology.NONE, Type.UNKNOWN, "ICSCRGET", null);
		module.addOutgoing(programBeingCalled, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiTaxonomyWithCobolProgramCallsIfdlByDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(EntityId.of(1L), COBOL, PROGRAM, IRRELEVANT_MODULE_NAME, null);
		final DependencyModule programBeingCalled = new DefaultDependecyModule(EntityId.of(1L), Technology.NONE, Type.UNKNOWN, "FORMS$ENABLE", null);
		module.addOutgoing(programBeingCalled, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}
	
	private void testUiProgramType(final String fileName) {
		final AstNodePojo rootNode = createAst(UI_FOLDER_NAME, fileName).get(null);
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.empty(), Optional.of(rootNode),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> identifiedTaxonomy = identifiedTaxonomies.get(0);
		assertEquals(UI, identifiedTaxonomy.e1);
		assertEquals(PROGRAM_TYPE, identifiedTaxonomy.e2);
	}
	
	private void testFileAccess(final String fileName, final TechnicalTaxonomies.Name... taxonomyNames) {
		final AstNodePojo rootNode = createAst(FILE_ACCESS_FOLDER_NAME, fileName).get(null);
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.empty(), Optional.of(rootNode),
				Optional.empty());
		assertEquals(taxonomyNames.length, identifiedTaxonomies.size());
		
		for (final Iterator<Tuple2<Name, TypeName>> iterator = identifiedTaxonomies.iterator(); iterator.hasNext();) {
			final Tuple2<Name, TypeName> identifiedTaxonomy = iterator.next();
			assertEquals(FILE_ACCESS, identifiedTaxonomy.e2);
			for (final TechnicalTaxonomies.Name name : taxonomyNames) {
				if (name.equals(identifiedTaxonomy.e1)) {
					iterator.remove();
				}
			}
		}
		assertEquals(0, identifiedTaxonomies.size(), "Identified taxonomies contains unexpected name. Identified list should be empty.");
	}

}
