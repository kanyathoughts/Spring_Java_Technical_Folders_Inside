/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery.dna.export;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import innowake.mining.extensions.export.discovery.dna.NeighboringModulesGenerator;
import innowake.mining.server.integration.discovery.dna.DiscoverDnaJobTest;
import innowake.mining.shared.discovery.Tuple2;

/**
 * Test class to validate if the export and calculation of average similarity is performed correctly when a Module has only two neighboring 
 * similarity modules
 */
class DnaNeighboringExportAverageSimilarityTest extends DiscoverDnaJobTest {

	@Autowired
	private NeighboringModulesGenerator neighboringModulesGenerator;
	
	/**
	 * Test to validate correct export of DnaNeighboring similarities when containing only two neighboring modules.
	 */
	@Test
	void testForDiscoverDnaNeighboringModules() {
		updateProjectConfigurationsAndRunDna("0.2", Optional.empty());
		final List<Tuple2<String, byte[]>> result = neighboringModulesGenerator.build(assertNotNull(projectId));
		assertEquals(2, result.size());
		final String actualCobolMethodRuleResult = new String(result.get(0).e2, StandardCharsets.UTF_8);
		
		final String expectedCobolMethodRuleResult = "Cluster Index, Module,First Neighbor,First Neighbor Similarity,Second Neighbor,Second Neighbor Similarity,Third Neighbor,Third Neighbor Similarity,Avg of Top 3 Similarity Weights\n" +
				"-1,src/cobol/WMIN5309B/programs/DPB010.cbl,src/cobol/WMIN5309B/programs/DPB030.cbl,0.7,src/cobol/WMIN5309B/programs/DPB031.cbl,0.33333333333333337,0.5166666666666666\n"
				+ "1,src/cobol/WMIN5309B/programs/DPB030.cbl,src/cobol/WMIN5309B/programs/DPB010.cbl,0.7,src/cobol/WMIN5309B/programs/DPB031.cbl,0.47619047619047616,0.588095238095238\n"
				+ "1,src/cobol/WMIN5309B/programs/DPB031.cbl,src/cobol/WMIN5309B/programs/DPB030.cbl,0.47619047619047616,src/cobol/WMIN5309B/programs/DPB010.cbl,0.33333333333333337,0.40476190476190477";
		
		assertEquals(expectedCobolMethodRuleResult, actualCobolMethodRuleResult);
	}
	
	@Override
	protected String getTestFolder() {
		return Paths.get("DNA/WMIN5309B").toString();
	}
}
