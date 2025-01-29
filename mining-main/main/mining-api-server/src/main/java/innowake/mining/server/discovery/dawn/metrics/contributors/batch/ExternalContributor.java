/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

/**
 * Interface handling all the external technology references in the batch contributor
 */
public interface ExternalContributor {
	
	/**
	 * Collects the metrics from the passed source
	 *
	 * @param sourceContent the source content
	 */
	void collectMetrics(String... sourceContent);

}
