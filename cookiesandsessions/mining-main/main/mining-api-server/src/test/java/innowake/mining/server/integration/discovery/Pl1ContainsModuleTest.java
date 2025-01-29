/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.io.discovery.config.DiscoveryConfigurationImportService;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

@WithMockUser
class Pl1ContainsModuleTest extends BaseDiscoveryTest {

	@Autowired
	private DiscoveryConfigurationImportService configurationImportService;

	private static final String SEARCH_ORDER_CONFIG_FILE = "discovery-search-order.xml";

	final static String DISCOVERY_SEARCH_ORDER_XML = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n"
			+ "<search-orders>\r\n"
			+ "			<search-order>\r\n"
			+ "						<source pattern=\"**/*.pl1\"/>\r\n"
			+ "						<target pattern=\"src/pl1/include/copies/**/*.pcpy\"/>\r\n"
			+ "						<target pattern=\"src/pl1/copies/**/*.pcpy\"/>\r\n"
			+ "			</search-order>\r\n"
			+ "</search-orders>";

	@Test
	void test() {
		final EntityId projectId = performDiscovery();

		final List<ModulePojo> progs = moduleService.findModules(b -> b.ofProject(projectId).withName("TEST.pl1"));
		assertEquals(1, progs.size());
		final ModulePojo prog = progs.get(0);

		final List<ModulePojo> subProgs = moduleService.findModules(b -> b.ofProject(projectId).withName("HelloWorld"));
		assertEquals(1, subProgs.size());
		final ModulePojo subProg = subProgs.get(0);

		assertEquals("PL1 subprogram is contained in program", 1L, moduleService.countRelationships(q -> q
				.ofSource(prog.identity()).withType(RelationshipType.CONTAINS).ofDestination(subProg.identity())));
	}

	@Test
	void testSearchOrderReferences() throws Exception {
		final EntityId projectId = performDiscovery();
		assertEquals(0, getReferencesSourceObjectCount());

		final List<ModulePojo> programModules = moduleService.findModules(q -> q.ofProject(projectId).withName("TEST.pl1"));
		final List<ModulePojo> copyModules = moduleService.findModules(q -> q.ofProject(projectId).withName("TESTCPY.pcpy"));

		final SourcePojo source = sourceService.findOne(q -> q.ofProject(projectId).withPath(assertNotNull(programModules.get(0).getPath().get()))).get();
		final SourcePojo target = sourceService.findOne(q -> q.ofProject(projectId).withPath(assertNotNull(copyModules.get(0).getPath().get()))).get();

		sourceService.putReference(source.identity(), target.identity());
		assertEquals(1, getReferencesSourceObjectCount());

		final Map<String, String> configs = new HashMap<>(64);
		configs.put(SEARCH_ORDER_CONFIG_FILE, DISCOVERY_SEARCH_ORDER_XML);
		configurationImportService.importConfigurations(projectId, configs);
		assertEquals(0, getReferencesSourceObjectCount());

		sourceService.putReference(source.identity(), target.identity());
		assertEquals(1, getReferencesSourceObjectCount());
		/* Verify that same configurations do not delete the existing references */
		configurationImportService.importConfigurations(projectId, configs);
		assertEquals(1, getReferencesSourceObjectCount());
	}

	private long getReferencesSourceObjectCount() {
		return sourceService.countReferences();
	}

	private EntityId performDiscovery() {
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		final BinaryString content = new BinaryString(
				"  HelloWorld: proc options (main);\n" +
				"    put skip list(\"Hello PL/I World!\");\n" +
				"    %INCLUDE TESTCPY;\n" +
				"  end HelloWorld;\n");

		uploadSourceObject(new SourcePojoPrototype()
				.setProject(projectId)
				.setName("TEST.pl1")
				.setPath("src/pl1/TEST.pl1")
				.setTechnology(Technology.PL1)
				.setType(Type.PROGRAM)
				.setContent(content));

		final BinaryString copyContent = new BinaryString(
				"  DCL TEST2proc EXTERNAL ENTRY;\r\n" +
				"  call TEST2proc;");

		uploadSourceObject(new SourcePojoPrototype()
				.setProject(projectId)
				.setName("TESTCPY.pcpy")
				.setPath("src/pl1/TESTCPY.pcpy")
				.setTechnology(Technology.PL1)
				.setType(Type.COPYBOOK)
				.setContent(copyContent));

		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));

		return projectId;
	}

	@Override
	protected String getTestFolder() {
		/* This test does not use the contents of the test folder, but it needs the 'submitJob' method! */
		return "PL1_ContainsModule";
	}
}
