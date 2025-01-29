/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;

@WithMockUser
class CobolBmsMapContainsModuleTest extends BaseDiscoveryTest {

	private String testFolder = "";

	/**
	 * Tests whether there is a ContainsModule edge from a BMS_MAPSET to its BMS_MAP.
	 */
	@DisplayName("Tests ContainsModule edge from a BMS_MAPSET to its BMS_MAP")
	@Test
	void testIsContainedInMapSet() {
		this.testFolder = "cobol_bms_map";

		final EntityId projectId = performDiscovery().identity();
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
		assertEquals(2, modules.size());
		final ModulePojo mapset = modules.get(0).getType() == Type.BMS_MAPSET ? modules.get(0) : modules.get(1);
		final ModulePojo map = modules.get(0).getType() == Type.BMS_MAP ? modules.get(0) : modules.get(1);
		assertEquals(Type.BMS_MAPSET, mapset.getType());
		assertEquals(Type.BMS_MAP, map.getType());
		assertTrue("Missing ContainsModule edge!", moduleService.findAnyRelationship(q -> q.ofSource(mapset.identity()).ofDestination(map.identity())).isPresent());
	}

	/**
	 * Tests whether the virtual modules for maps are created correctly without duplicates.
	 */
	@DisplayName("Tests virtual modules for maps of mapsets")
	@Test
	void testBmsMap() {
		this.testFolder = "WMIN2118";

		projectId = assertNotNull(performDiscovery()).identity();

		final List<LazyModelArtifact> artifacts = modelArtifactService.find(b -> b.ofProject(projectId));
		assertEquals(17, artifacts.size());

		/* There are seven maps defined in four map sets: 2 x UMACTVA, UMACTM2, UMACTM3, UMACTM4, M8268A, M8268B */
		final List<ModelArtifact> maps = artifacts.stream()
													.filter(artifact -> artifact.getType() == ResolveTarget.CICS_BMS_MAP)
													.collect(Collectors.toList());
		assertEquals(7, maps.size());
		final List<ModelArtifact> progs = artifacts.stream()
													.filter(artifact -> artifact.getType() == ResolveTarget.COBOL_PROGRAM)
													.collect(Collectors.toList());
		assertEquals(3, progs.size());
		final List<ModelArtifact> mapsets = artifacts.stream()
													.filter(artifact -> artifact.getType() == ResolveTarget.CICS_BMS_MAPSET)
													.collect(Collectors.toList());
		assertEquals(4, mapsets.size());

		/* There are two maps with id 'UMACTVA' defined, in UTACTM2.map and UTACTVA.map and both maps get called by UTDACTVA.cbl and UTDACTM2.cbl */
		assertEquals(2, maps.stream()
							.filter(artifact -> "UMACTVA".equals(artifact.getName()))
							.count());

		/* The map is defined in MS8268A.map and get's called twice by MEE8268A.cbl */
		assertEquals(1, maps.stream()
							.filter(artifact -> "M8268A".equals(artifact.getName()))
							.count());

		assertContainsModule(mapsets, "UTACTM2", maps, "UMACTM2");
		assertContainsModule(mapsets, "UTACTM2", maps, "UMACTM3");
		assertContainsModule(mapsets, "UTACTM2", maps, "UMACTM4");
		assertContainsModule(mapsets, "MS8268A", maps, "M8268A");
		assertContainsModule(mapsets, "MS8268B", maps, "M8268B");

		/* Two maps with name "UMACTVA" exists in mapsets "UTACTVA" and "UTACTM2" */
		final List<ModelArtifact> mapsets2 = mapsets.stream()
												.filter(artifact -> "UTACTVA".equals(artifact.getName()) || "UTACTM2".equals(artifact.getName()))
												.collect(Collectors.toList());
		assertEquals(2, mapsets2.size());

		final List<ModelArtifact> maps2 = maps.stream().filter(artifact -> "UMACTVA".equals(artifact.getName())).collect(Collectors.toList());
		assertEquals(2, maps2.size());

		int[] findings = {0, 0};
		for (int i = 0; i < mapsets2.size(); i++) {
			for (ModelArtifact map : maps2) {
				final int ii = i;
				if (moduleService.findAnyRelationship(q -> q.ofSource(assertNotNull(mapsets2.get(ii).getModuleId()))
						.ofDestination(assertNotNull(map.getModuleId()))).isPresent()) {
					findings[i]++;
				}
			}
		}

		for (int i = 0; i < mapsets2.size(); i++) {
			assertEquals(String.format("Missing ContainsModule edge for mapset %s!", mapsets2.get(i).getName()), 1, findings[i]);
		}
	}

	private void assertContainsModule(final List<ModelArtifact> mapsets, final String mapsetName, final List<ModelArtifact> maps, final String mapName) {
		final List<ModelArtifact> mapsets2 = mapsets.stream().filter(artifact -> mapsetName.equals(artifact.getName())).collect(Collectors.toList());
		assertEquals(1, mapsets2.size());

		final List<ModelArtifact> maps2 = maps.stream().filter(artifact -> mapName.equals(artifact.getName())).collect(Collectors.toList());
		assertEquals(1, maps2.size());

		assertTrue(String.format("Missing ContainsModule edge between %s and %s!", mapsets2.get(0).getName(), maps2.get(0).getName()),
				moduleService.findAnyRelationship(q -> q.ofSource(Objects.requireNonNull(mapsets2.get(0).getModuleId()))
						.ofDestination(Objects.requireNonNull(maps2.get(0).getModuleId()))).isPresent());
	}

	private ProjectPojo performDiscovery() {
		sourceService.resetCaches();
		final EntityId entityId = createProject().identity();
		uploadResources(entityId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(entityId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(entityId, true));
		return assertNotNull(projectService.get(entityId));
	}

	@Override
	public String getTestFolder() {
		return testFolder;
	}
}
