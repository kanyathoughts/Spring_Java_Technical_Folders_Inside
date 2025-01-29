/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery.dna.export;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.data.model.discovery.dna.ModelCluster;
import innowake.mining.data.model.discovery.dna.ModelClustering;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.server.integration.discovery.dna.DiscoverDnaJobTest;
import innowake.mining.server.service.DnaModelService;

/**
 * WMIN-8180: Test to validate whether the DNA clusters sorted based on modules count in it.
 */
class DiscoverDnaClustersTest extends DiscoverDnaJobTest {

	@Autowired
	private DnaModelService dnaModelService;
	
	@Override
	protected String getTestFolder() {
		return Paths.get("DNA/sequencer/NATURAL").toString();
	}
	
	/**
	 * Test to validate whether the DNA clusters sorted based on modules count in it.
	 */
	@Test
	void testForClustersSortedBySizeOfModules() {
		final Optional<ModelDna> latestDna = dnaModelService.getLatestDna(assertNotNull(projectId));
		assertTrue(latestDna.isPresent());
		final List<ModelClustering> clusterings = latestDna.get().getClusterings();
		assertEquals(2, clusterings.size());
		final List<ModelCluster> clusters1 = clusterings.get(0).getClusters();
		assertEquals(2, clusters1.size());
		assertTrue(clusters1.get(0).getModuleCount() > clusters1.get(1).getModuleCount());
		final List<ModelCluster> clusters2 = clusterings.get(1).getClusters();
		assertEquals(2, clusters2.size());
		assertTrue(clusters2.get(0).getModuleCount() > clusters2.get(1).getModuleCount());
	} 

}
