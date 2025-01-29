/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.model.discovery.dna;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.annotations.SerializedName;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model of the DNA Snapshot.
 */
@MiningDataType(name = "ModelDna")
public class ModelDna {

	@SerializedName("moduleCount")
	private int moduleCount;
	@SerializedName("clusterings")
	private List<ModelClustering> clusterings = new ArrayList<>();

	/**
	 * Parameterized constructor for capturing Model details.
	 * 
	 * @param moduleCount Total number of modules in the source code.
	 */
	public ModelDna(final int moduleCount) {
		this.moduleCount = moduleCount;
	}
	
	/**
	 * No Argument constructor.
	 */
	public ModelDna() {}

	/**
	 * Retrieves the number of modules.
	 *
	 * @return Number of modules.
	 */
	public int getModuleCount() {
		return moduleCount;
	}

	/**
	 * Sets the module count.
	 *
	 * @param moduleCount Number of modules.
	 */
	public void setModuleCount(final int moduleCount) {
		this.moduleCount = moduleCount;
	}

	/**
	 * Retrieves list of clusterings.
	 *
	 * @return List of clusterings.
	 */
	public List<ModelClustering> getClusterings() {
		return clusterings;
	}

	/**
	 * Sets the clusterings details.
	 *
	 * @param clusterings List of clusterings.
	 */
	public void setClusterings(final List<ModelClustering> clusterings) {
		this.clusterings = clusterings;
	}

}
