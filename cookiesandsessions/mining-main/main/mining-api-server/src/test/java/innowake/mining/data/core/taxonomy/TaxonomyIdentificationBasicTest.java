/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static innowake.mining.shared.model.RelationshipType.CALLS;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.WRITE;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.FILE_ACCESS;
import static innowake.mining.shared.model.Technology.BASIC;
import static innowake.mining.shared.model.Type.FUNCTION;
import static innowake.mining.shared.model.Type.PROGRAM;
import static innowake.mining.shared.model.Type.SUBROUTINE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.util.AssertionErrors.assertTrue;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import innowake.mining.server.integration.DatabaseRelatedTest;

import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicParser;
import innowake.ndt.parsing.parser.basic.DefaultDataProvider;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests regarding Taxonomy identification for BASIC.
 */
class TaxonomyIdentificationBasicTest extends DatabaseRelatedTest {
	
	protected static final Charset CHARSET = Charset.forName("CP1252");
	private final static String IRRELEVANT_MODULE_NAME = "Irrelevant";
	private final Path MODEL_TESTS_BASE = Paths.get("test-resources", "innowake", "mining", "data", "core");

	@Autowired
	private TechnicalTaxonomyIdentifier technicalTaxonomyIdentifier;

	@Test
	void fileAccessOnBasicFileRead() {
		testBasicFileAccess("WMIN846FILE01.bas", READ);
	}
	
	@Test
	void fileAccessOnBasicFileWriteDelete() {
		testBasicFileAccess("WMIN846FILE02.bas", WRITE);
	}
	
	@Test
	void fileAccessOnBasicFileWriteKill() {
		testBasicFileAccess("WMIN846FILE04.bas", WRITE);
	}
	
	@Test
	void fileAccessOnBasicFileWriteScratch() {
		testBasicFileAccess("WMIN846FILE05.bas", WRITE);
	}
	
	@Test
	void fileAccessOnBasicFileWriteUpdate() {
		testBasicFileAccess("WMIN846FILE06.bas", WRITE);
	}
	
	@Test
	void fileAccessOnBasicFileWritePut() {
		testBasicFileAccess("WMIN846FILE07.bas", WRITE);
	}
	
	@Test
	void fileAccessOnBasicFileReadWrite() {
		testBasicFileAccess("WMIN846FILE03.bas", WRITE, READ);
	}
	
	@Test
	void uiTaxonomyWithIfdlCallsBasicFunctionByDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(BASIC, FUNCTION, IRRELEVANT_MODULE_NAME);
		final DependencyModule callingProgram = new DefaultDependecyModule(Technology.VMS, Type.IFDL_FORM, IRRELEVANT_MODULE_NAME);
		module.addIncoming(callingProgram, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiTaxonomyWithBasicProgramCallsIcscrgetByDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(BASIC, PROGRAM, IRRELEVANT_MODULE_NAME);
		final DependencyModule programBeingCalled = new DefaultDependecyModule(Technology.NONE, Type.UNKNOWN, "ICSCRGET");
		module.addOutgoing(programBeingCalled, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiTaxonomyWithBasicFunctionCallsIcscrgetByDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(BASIC, FUNCTION, IRRELEVANT_MODULE_NAME);
		final DependencyModule programBeingCalled = new DefaultDependecyModule(Technology.NONE, Type.UNKNOWN, "ICSCRGET");
		module.addOutgoing(programBeingCalled, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}
	
	@Test
	void uiTaxonomyWithBasicSubroutineCallsIcscrgetByDependencyModule() {
		final DefaultDependecyModule module = new DefaultDependecyModule(BASIC, SUBROUTINE, IRRELEVANT_MODULE_NAME);
		final DependencyModule programBeingCalled = new DefaultDependecyModule(Technology.NONE, Type.UNKNOWN, "ICSCRGET");
		module.addOutgoing(programBeingCalled, CALLS);
		
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.of(module), Optional.empty(),
				Optional.empty());
		assertEquals(1, identifiedTaxonomies.size());
		assertEquals(Name.UI, identifiedTaxonomies.get(0).e1);
	}

	@Override
	protected void resetTestData() throws IOException {
		/* Do nothing */
	}

	private void testBasicFileAccess(final String fileName, final TechnicalTaxonomies.Name... taxonomyNames) {
		final Path sourceFile = Paths.get("taxonomy", "file-access", fileName);
		final Path testFilePath = MODEL_TESTS_BASE.resolve(sourceFile);
		final BasicParser<Path> parser = new BasicParser<>(new BaseParserConfiguration.Builder<Path>()
									.setAssemblingDataProvider(new DefaultDataProvider(CHARSET)).build());
		final Optional<BasicModel> model = parser.parse(testFilePath);
		assertTrue("Basic file is not parseable", model.isPresent());
		final AstModel astModel = model.get();
		final List<Tuple2<Name, TypeName>> identifiedTaxonomies = technicalTaxonomyIdentifier.identify(Optional.empty(), Optional.empty(),
				Optional.of(astModel));
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
