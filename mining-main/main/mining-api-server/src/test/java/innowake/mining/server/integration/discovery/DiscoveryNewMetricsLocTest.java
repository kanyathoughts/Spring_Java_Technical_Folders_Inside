/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;


/**
 * This executes new Metrics 2.0 LoC tests.
 * 
 * The test case will read the input files from /mining-api-server/test-resources/innowake/mining/server/discovery/source/newMetricsLoc/ folder and compare
 * against /mining-api-server/test-resources/innowake/mining/server/discovery/expected/newMetricsLoc/
 */
public class DiscoveryNewMetricsLocTest extends DiscoveryBulkTest {

	@Override
	protected String folder() {
		return "newMetricsLoc";
	}

}
