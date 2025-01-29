/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.model.discovery.dna;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.UnaryOperator;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.io.discovery.config.Configurable;
import innowake.mining.data.io.discovery.config.Configuration;
import innowake.mining.data.model.discovery.ModelAlgorithmOption;
import innowake.mining.shared.discovery.config.ConfigResources;

/**
 * Gets the DNA configuration details
 */
public class DnaConfig extends Configurable {

	private int maxLevels = 5;
	private int maxIterations = 10;
	private double defaultTolerance = 0.0001;
	private int minDnaLength = 20;
	private double similarityThreshold = 0.85;
	
	/**
	 * Sets the similarity threshold.
	 *
	 * @param similarityThreshold the similarity threshold
	 */
	@Configuration(name = "similarity threshold", defaultValue = "0.85",
			comment = "Define the minimum similarity for the dna to be processed by the community detection. Range 0.0-1.0", title = "Similarity Threshold")
	public void setSimilarityThreshold(final double similarityThreshold) {
		this.similarityThreshold = similarityThreshold;
	}
	

	/**
	 * Sets the maximum levels.
	 *
	 * @param maxLevels the maximum levels
	 */
	@Configuration(name = "maxLevels", defaultValue = "5",
			comment = "Define the number of graph versions. Each version optimize modularity by the number of iterations and verify the improvement.",
			title = "Maximum Levels")
	public void setMaxLevels(final int maxLevels) {
		this.maxLevels = maxLevels;
	}

	
	/**
	 * Sets the maximum iterations.
	 *
	 * @param maxIterations the maximum iterations
	 */
	@Configuration(name = "maxIterations", defaultValue = "10",
			comment = "Define the number to modularity optimization iterations. See: https://en.wikipedia.org/wiki/Louvain_modularity",
			title = "Maximum Iterations")
	public void setMaxIterations(final int maxIterations) {
		this.maxIterations = maxIterations;
	}

	
	/**
	 * Sets the default tolerance.
	 *
	 * @param defaultTolerance the default tolerance
	 */
	@Configuration(name = "defaultTolerance", defaultValue = "0.001",
			comment = "The minimum of Q-improvement required to take the current graph as improvement of the level. Q is the graph modularity. "
					+ "See https://en.wikipedia.org/wiki/Modularity_(networks)", title = "Default Tolerance")
	public void setDefaultTolerance(final double defaultTolerance) {
		this.defaultTolerance = defaultTolerance;
	}
	
	/**
	 * Sets the minimum DNA length.
	 *
	 * @param minDnaLength the minimum DNA length.
	 */
	@Configuration(name = "minDNALength", defaultValue = "20",
			comment = "The minimum string length of the extracted dna to be valid for further processing.", title = "Minimum DNA Length")
	public void setMinDnaLength(final int minDnaLength) {
		this.minDnaLength = minDnaLength;
	}
	
	/**
	 * Gets the similarity threshold.
	 * 
	 * @return the similarity threshold
	 */
	public double getSimilarityThreshold() {
		return similarityThreshold;
	}

	/**
	 * Gets the maximum levels.
	 * 
	 * @return the maximum levels
	 */
	public int getMaxLevels() {
		return maxLevels;
	}

	/**
	 * Gets the maximum iterations.
	 * 
	 * @return the maximum iterations
	 */
	public int getMaxIterations() {
		return maxIterations;
	}

	/**
	 * Gets the default tolerance.
	 * 
	 * @return the default tolerance
	 */
	public double getDefaultTolerance() {
		return defaultTolerance;
	}

	/**
	 * Gets the minimum DNA length.
	 * 
	 * @return the minimum DNA length
	 */
	public int getMinDnaLength() {
		return minDnaLength;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (obj == null) {
			return false;
		}
		if (this == obj) {
			return true;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final DnaConfig other = (DnaConfig) obj;
		
		return ! (Double.doubleToLongBits(defaultTolerance) != Double.doubleToLongBits(other.defaultTolerance) || 
				maxIterations != other.maxIterations ||
				maxLevels != other.maxLevels ||
				minDnaLength != other.minDnaLength ||
				Double.doubleToLongBits(similarityThreshold) != Double.doubleToLongBits(other.similarityThreshold));
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
	
	public static DnaConfig fromMap(final Map<String, Object> props) {
		final var config = new DnaConfig();
		config.loadMap(props);
		return config;
	}

	/**
	 * Loads the DNA configuration properties from external files and sets the DNA configuration properties.
	 * Example: DNA_Sequencer_Config.xml, DNA_SimilarityProcessor_Config.xml, DNA_LouvainRunner_Config.xml
	 * @param configProvider Function providing configuration data by configuration name.
	 * @return the {@link DnaConfig}
	 */
	public static DnaConfig loadAndSetDnaConfigs(final UnaryOperator<String> configProvider) {
		final var dnaConfig = new DnaConfig();
		final List<Configurable> configurationProvider = new ArrayList<>();
		configurationProvider.add(new Configurable(() -> configProvider.apply(ConfigResources.DNA_LOUVAIN_RUNNER_CONFIG.getResourceName())));
		configurationProvider.add(new Configurable(() -> configProvider.apply(ConfigResources.DNA_SEQUENCER_CONFIG.getResourceName())));
		configurationProvider.add(new Configurable(() -> configProvider.apply(ConfigResources.DNA_SIMILARITY_PROCESSOR_CONFIG.getResourceName())));
		configurationProvider
			.stream()
			.map(Configurable::loadConfiguration)
			.filter(Objects::nonNull)
			.forEach(dnaConfig::setProperties);
		return dnaConfig;
	}
	
	/**
	 * Load the DNA configuration properties from {@link DnaConfig}.
	 * If there is any new property added in existing configuration file or a new configuration file added for DNA calculation, this method needs to be 
	 * updated. 
	 * 
	 * @param dnaConfig the {@link DnaConfig}
	 * @return list of {@link ModelAlgorithmOption}
	 */
	public static List<ModelAlgorithmOption> getDnaConfigurationOptions(final DnaConfig dnaConfig) {
		final var maxLevels = new ModelAlgorithmOption("maxLevels", "Maximum Levels", String.valueOf(dnaConfig.getMaxLevels()));
		final var maxIterations = new ModelAlgorithmOption("maxIterations", "Maximum Iterations", String.valueOf(dnaConfig.getMaxIterations()));
		final var defaultTolerance = new ModelAlgorithmOption("defaultTolerance", "Default Tolerance", String.format("%f", dnaConfig.getDefaultTolerance()));
		final var minDNALength = new ModelAlgorithmOption("minDNALength", "Minimum DNA Length", String.valueOf(dnaConfig.getMinDnaLength()));
		final var similarityThreshold = new ModelAlgorithmOption("similarity threshold", "Similarity Threshold", String.valueOf(dnaConfig.getSimilarityThreshold()));
		return Arrays.asList(maxLevels, maxIterations, defaultTolerance, minDNALength, similarityThreshold);
	}
}

