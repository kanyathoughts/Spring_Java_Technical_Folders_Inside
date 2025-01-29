/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery.dna.export;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.extensions.export.discovery.dna.NeighboringModulesGenerator;
import innowake.mining.server.integration.discovery.dna.DiscoverDnaJobTest;
import innowake.mining.shared.discovery.Tuple2;

/**
 * Test to validate the export of neighboring modules export job
 */
class DnaNeighboringModulesExporterTest extends DiscoverDnaJobTest {

	@Autowired
	private NeighboringModulesGenerator neighboringModulesGenerator;

	private static final String TEST_FOLDER = Paths.get("DNA/WMIN5309").toString();

	/**
	 * Test to check the neighboring modules exported are generated correctly.
	 */
	@Test
	void testForDiscoverDnaNeighboringModules() {
		updateProjectConfigurationsAndRunDna("0.21", Optional.empty());
		final List<Tuple2<String, byte[]>> result = neighboringModulesGenerator.build(assertNotNull(projectId));
		assertEquals(2, result.size());
		final String actualCobolSkeletonRuleResult = maskClusterIndex(new String(result.get(0).e2, StandardCharsets.UTF_8));
		final String actualCobolMethodRuleResult = maskClusterIndex(new String(result.get(1).e2, StandardCharsets.UTF_8));
		final String expectedCobolSkeletonRuleResult =
				  "X,src/cobol/WMIN5309/programs/DPB010.cbl,src/cobol/WMIN5309/programs/DPB030.cbl,0.7,src/cobol/WMIN5309/programs/DPB031.cbl,0.33333333333333337,src/cobol/WMIN5309/programs/DPB011.cbl,0.32307692307692304,0.45213675213675214\n"
				+ "X,src/cobol/WMIN5309/programs/DPB011.cbl,src/cobol/WMIN5309/programs/DPB031.cbl,0.8,src/cobol/WMIN5309/programs/DPB030.cbl,0.4461538461538461,src/cobol/WMIN5309/programs/DPB010.cbl,0.32307692307692304,0.5230769230769231\n"
				+ "X,src/cobol/WMIN5309/programs/DPB030.cbl,src/cobol/WMIN5309/programs/DPB010.cbl,0.7,src/cobol/WMIN5309/programs/DPB031.cbl,0.47619047619047616,src/cobol/WMIN5309/programs/DPB011.cbl,0.4461538461538461,0.5407814407814407\n"
				+ "X,src/cobol/WMIN5309/programs/DPB031.cbl,src/cobol/WMIN5309/programs/DPB011.cbl,0.8,src/cobol/WMIN5309/programs/DPB030.cbl,0.47619047619047616,src/cobol/WMIN5309/programs/DPB010.cbl,0.33333333333333337,0.5365079365079365";
		
		final String expectedCobolMethodRuleResult = 
				  "X,src/cobol/WMIN5309/programs/DPB010.cbl,src/cobol/WMIN5309/programs/DPB030.cbl,0.5204081632653061,src/cobol/WMIN5309/programs/DPB011.cbl,0.28415300546448086,src/cobol/WMIN5309/programs/DPB031.cbl,0.223175965665236,0.342579044798341\n"
				+ "X,src/cobol/WMIN5309/programs/DPB011.cbl,src/cobol/WMIN5309/programs/DPB031.cbl,0.5622317596566524,src/cobol/WMIN5309/programs/DPB030.cbl,0.4316939890710383,src/cobol/WMIN5309/programs/DPB010.cbl,0.28415300546448086,0.42602625139739053\n"
				+ "X,src/cobol/WMIN5309/programs/DPB030.cbl,src/cobol/WMIN5309/programs/DPB010.cbl,0.5204081632653061,src/cobol/WMIN5309/programs/DPB011.cbl,0.4316939890710383,src/cobol/WMIN5309/programs/DPB031.cbl,0.3948497854077253,0.44898397924802325\n"
				+ "X,src/cobol/WMIN5309/programs/DPB031.cbl,src/cobol/WMIN5309/programs/DPB011.cbl,0.5622317596566524,src/cobol/WMIN5309/programs/DPB030.cbl,0.3948497854077253,src/cobol/WMIN5309/programs/DPB010.cbl,0.223175965665236,0.3934191702432046";
				
		assertEqualsIgnoringOrder(expectedCobolSkeletonRuleResult, actualCobolSkeletonRuleResult);
		assertEqualsIgnoringOrder(expectedCobolMethodRuleResult, actualCobolMethodRuleResult);
	}
	
	private static void assertEqualsIgnoringOrder(final String expected, final String actual) {
		final List<String> expectedList = Arrays.asList(expected.split("(\\r\\n|\\r|\\n)")).stream().filter(StringUtils::isNotBlank).sorted()
				.collect(Collectors.toList());
		final List<String> actualList = Arrays.asList(actual.split("(\\r\\n|\\r|\\n)")).stream().filter(StringUtils::isNotBlank).sorted()
				.collect(Collectors.toList());

		assertEquals(expectedList.size(), actualList.size(), "List sizes must match");
		for (int i = 0; i < expectedList.size(); i++) {
			assertEquals(expectedList.get(i), actualList.get(i), "List entry must match");
		}
	}

	/* The cluserIndex for a module is dynamic and we can't predict the value, in order to test it here we should replace the clusterIndex with some dummy value.*/
	private String maskClusterIndex(final String actual) {
		return Arrays.asList(actual.split("\n")).stream().skip(1).map(neiboringSimilarityString -> {
			final String[] arrayOfNeighboringSimilarityContents = neiboringSimilarityString.split(",");
			if (arrayOfNeighboringSimilarityContents.length > 0) {
				arrayOfNeighboringSimilarityContents[0] = "X";
				return Arrays.asList(arrayOfNeighboringSimilarityContents).stream().collect(Collectors.joining(","));
			}
			return neiboringSimilarityString;
		}).collect(Collectors.joining("\n"));
	}
	
	@Override
	protected String getTestFolder() {
		return TEST_FOLDER;
	}
}
