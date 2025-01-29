/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.cobol;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.discovery.categorize.Statistic;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test splitting of Cobol modules with and without program ids.
 */
class CobolModuleSplitterTest extends DatabaseResettingTest {

	@Autowired
	private SourceCachingService sourceService;

	private final EntityId PROJECT_ID = EntityId.of(1L);
	
	private final Statistic statistic = new Statistic();

	private final String COBOL_SAMPLE =
			"      **************************************************************\n" +
			"      ***                       E900VSA1 ENTRY                   ***\n" +
			"      **************************************************************\n" +
			"       IDENTIFICATION DIVISION.\n" +
			"       PROGRAM-ID. E900VSA1.\n" +
			"           COPY FUXVSACC.\n" +
			"       END PROGRAM E900VSA1.\n" +
			"      **************************************************************\n" +
			"      ***                       E900VSA2 ENTRY                   ***\n" +
			"      **************************************************************\n" +
			"       IDENTIFICATION DIVISION.\n" +
			"       PROGRAM-ID. E900VSA2.\n" +
			"           COPY FUXVSACC.\n" +
			"       END PROGRAM E900VSA2.\n";

	private final String COBOL_SAMPLE_WITHOUT_IDS =
			"      **************************************************************\n" +
			"      ***                       E900VSA1 ENTRY                   ***\n" +
			"      **************************************************************\n" +
			"       IDENTIFICATION DIVISION.\n" +
			"           COPY FUXVSACC.\n" +
			"       END PROGRAM E900VSA1.\n" +
			"      **************************************************************\n" +
			"      ***                       E900VSA2 ENTRY                   ***\n" +
			"      **************************************************************\n" +
			"       IDENTIFICATION DIVISION.\n" +
			"       PROGRAM-ID.\n";

	@Test
	void splitModule() {
		assertNotNull(sourceService);

		final SourcePojo cobol = createProgram("COBOL1", "test/COBOL1.cbl", COBOL_SAMPLE);
		CobolModuleSplitter.execute(cobol, sourceService, statistic);

		final SourcePojo splitCobol = sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL1.cbl"));
		assertTrue(splitCobol.getContent().toString().length() < COBOL_SAMPLE.length());

		sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL1/E900VSA2.cbl"));

		assertEquals(2, sourceService.find(q -> q.ofProject(PROJECT_ID)).size());
	}

	@Test
	void splitTwoModulesWithSameIds() {
		assertNotNull(sourceService);

		final SourcePojo cobol1 = createProgram("COBOL1", "test/COBOL1.cbl", COBOL_SAMPLE);
		final SourcePojo cobol2 = createProgram("COBOL2", "test/COBOL2.cbl", COBOL_SAMPLE);
		CobolModuleSplitter.execute(cobol1, sourceService, statistic);
		CobolModuleSplitter.execute(cobol2, sourceService, statistic);

		final SourcePojo splitCobol1 = sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL1.cbl"));
		assertTrue(splitCobol1.getContent().toString().length() < COBOL_SAMPLE.length());

		sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL1/E900VSA2.cbl"));

		final SourcePojo splitCobol2 = sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL2.cbl"));
		assertNotNull(splitCobol2);
		assertTrue(splitCobol2.getContent().toString().length() < COBOL_SAMPLE.length());

		sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL2/E900VSA2.cbl"));

		assertEquals(4, sourceService.find(q -> q.ofProject(PROJECT_ID)).size());
	}

	@Test
	void splitModuleWithoutIds() {
		assertNotNull(sourceService);

		final SourcePojo cobol = createProgram("COBOL2", "test/COBOL2.cbl", COBOL_SAMPLE_WITHOUT_IDS);
		CobolModuleSplitter.execute(cobol, sourceService, statistic);

		final SourcePojo splitCobol = sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL2.cbl"));
		assertNotNull(splitCobol);
		assertTrue(splitCobol.getContent().toString().length() < COBOL_SAMPLE_WITHOUT_IDS.length());

		assertNotNull(sourceService.get(q -> q.ofProject(PROJECT_ID).withPath("test/COBOL2/COBOL2_1.cbl")));

		assertEquals(2, sourceService.find(q -> q.ofProject(PROJECT_ID)).size());
	}

	private SourcePojo createProgram(final String name, String path, final String content) {
		sourceService.create(new SourcePojoPrototype()
				.setProject(PROJECT_ID)
				.setName(name)
				.setPath(path)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setMetaDataRevision(0l)
				.setContentRevision(0l)
				.setContent(new BinaryString(content)));
				//new SourcePojo(PROJECT_ID, name, path, , , PROJECT_ID, PROJECT_ID, content, CityHash.cityHash128Hex(content)));
		return sourceService.findOne(q -> q.ofProject(PROJECT_ID).withPath(path))
				.orElseThrow(() -> new AssertionError("Created source not found."));
	}
}
