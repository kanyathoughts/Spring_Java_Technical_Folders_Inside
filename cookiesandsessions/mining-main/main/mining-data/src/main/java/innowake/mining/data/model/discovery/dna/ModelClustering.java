/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.model.discovery.dna;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.annotations.SerializedName;

import innowake.mining.data.model.discovery.ModelAlgorithmOption;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Clustering details. A DNA Snapshot would contain multiple clusterings.
 */
@MiningDataType(name = "ModelClustering")
public class ModelClustering {

	@SerializedName("algorithm")
	private Map<String, String> algorithm = new HashMap<>();
	@SerializedName("options")
	private List<ModelAlgorithmOption> options = new ArrayList<>();
	@SerializedName("clusters")
	private List<ModelCluster> clusters = new ArrayList<>();

	/**
	 * Algorithm name in key value pair.
	 *
	 * @return Algorithm Name.
	 */
	public Map<String, String> getAlgorithm() {
		return algorithm;
	}

	/**
	 * Sets the algorithm name.
	 *
	 * @param algorithm key value pair.
	 */
	public void setAlgorithm(final Map<String, String> algorithm) {
		this.algorithm = algorithm;
	}

	/**
	 * Retrieves algorithm options used.
	 *
	 * @return list of algorithm options.
	 */
	public List<ModelAlgorithmOption> getOptions() {
		return options;
	}

	/**
	 * Sets the algorithm options.
	 *
	 * @param options key value pair.
	 */
	public void setOptions(final List<ModelAlgorithmOption> options) {
		this.options = options;
	}

	/**
	 * Retrieves all the cluster details of this clustering.
	 *
	 * @return list of clusters.
	 */
	public List<ModelCluster> getClusters() {
		return clusters;
	}

	/**
	 * Sets the cluster details of this clustering.
	 *
	 * @param clusters List of clusters.
	 */
	public void setClusters(final List<ModelCluster> clusters) {
		this.clusters = clusters;
	}

}
