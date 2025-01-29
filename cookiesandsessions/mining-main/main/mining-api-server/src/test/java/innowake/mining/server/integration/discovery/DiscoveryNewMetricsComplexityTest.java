/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

/**
 * This test encapsulates all complexity tests. 
 * 
 * This will check the logic to calculate complexity.
 * The test case will read the input files from /mining-api-server/test-resources/innowake/mining/server/discovery/source/complexity/ folder and compare
 * against /mining-api-server/test-resources/innowake/mining/server/discovery/expected/complexity/
 */
public class DiscoveryNewMetricsComplexityTest extends DiscoveryBulkTest {

	@Override
	protected String folder() {
		return "complexity";
	}
}
