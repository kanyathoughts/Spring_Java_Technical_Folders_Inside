/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.service;

/**
 * Represents an ongoing Discovery and allows to manually advance into the next "cycle".
 * Allows interacting with an ongoing Discovery process to allow integration with the existing legacy Discovery.
 */
public interface OngoingDiscovery {
	/**
	 * Returns {@code true} if there are remaining tasks to be executed, so {@link #executeNextCycle()} should be called.
 	 * @return {@code true} if there are more tasks to be executed
	 */
	boolean hasNextCycle();

	/**
	 * Executes all tasks that represent the next "cycle" of Discovery.
	 * The Discovery process is not finished until {@link #hasNextCycle()} returns {@code false}. Until then, this method must be called
	 * repeatedly.
	 */
	void executeNextCycle();
}
