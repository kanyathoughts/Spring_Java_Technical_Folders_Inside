/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery.config;

/**
 * Used to define all configuration related files.
 */
public enum ConfigResources {

	DISCOVERY_CONFIG("Discovery_Config.xml"),
	DNA_SEQUENCER_CONFIG("DNA_Sequencer_Config.xml"),
	DNA_LOUVAIN_RUNNER_CONFIG("DNA_LouvainRunner_Config.xml"),
	DNA_SIMILARITY_PROCESSOR_CONFIG("DNA_SimilarityProcessor_Config.xml"),
	UTILITIES("utilities.xml");

	private final String resourceName;

	private ConfigResources(final String resourceName) {
		this.resourceName = resourceName;
	}

	/**
	 * Name of the resource file
	 *
	 * @return resource name.
	 */
	public String getResourceName() {
		return resourceName;
	}
}
