/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.WRITE;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.FILE_ACCESS;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import innowake.mining.data.core.storeast.impl.AbstractNaturalTest;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.data.core.taxonomy.impl.*;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Tests regarding TaxonomyPojo identification for Natural.
 */
class TaxonomyIdentificationNaturalTest extends AbstractNaturalTest {
	
	private final static String FILE_ACCESS_FOLDER_NAME = "file-access";
	private final static String UI_FOLDER_NAME = "ui";

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
		ReflectionTestUtils.setField(technicalTaxonomyIdentifier, "astBasedIdentifiers",
				List.of(fileAccessIdentifier, uiProgIdentifier));
		ReflectionTestUtils.setField(technicalTaxonomyIdentifier, "dependencyBasedIdentifiers",
				List.of(batchProgIdentifier, dbAccessIdentifier, dependencyBasedUiProgIdentifier, mqProgIdentifier));
	}

	@Test
	void fileAccessRead() {
		testFileAccess("WMIN1205A.nsp", READ);
	}
	
	@Test
	void fileAccessReadAndWrite() {
		testFileAccess("WMIN1205B.nsp", READ, WRITE);
	}
	
	@Test
	void uiProgramTypeForWrite() {
		testUiProgramType("WMIN1206A.nsp");
	}
	
	@Test
	void uiProgramTypeForPrint() {
		testUiProgramType("WMIN1206B.nsp");
	}
	
	@Test
	void uiProgramTypeForDisplay() {
		testUiProgramType("WMIN1206C.nsp");
	}
	
	@Test
	void uiProgramTypeForInput() {
		testUiProgramType("WMIN1206D.nsp");
	}
	
	@Test
	void uiProgramType() {
		testUiProgramType("WMIN1206E.nsp");
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
	
	private void testUiProgramType(final String fileName) {
		final AstNodePojo rootNode = createAst(UI_FOLDER_NAME, fileName).get(null);
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.empty(), Optional.of(rootNode),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size(), "Identified list must have 1 entry");
		final Tuple2<Name, TypeName> identifiedTaxonomy = identifiedTaxonomies.get(0);
		assertEquals(UI, identifiedTaxonomy.e1);
		assertEquals(PROGRAM_TYPE, identifiedTaxonomy.e2);
	}
	
}
