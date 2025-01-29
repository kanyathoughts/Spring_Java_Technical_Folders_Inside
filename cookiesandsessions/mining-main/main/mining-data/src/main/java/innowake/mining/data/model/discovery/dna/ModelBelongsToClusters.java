/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.model.discovery.dna;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonSetter;
import com.google.gson.annotations.SerializedName;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model of clusters in which module belongs to.
 */
public class ModelBelongsToClusters { 
    
	@SerializedName("moduleId")
	@Nullable
    private EntityId moduleId;
    
	@SerializedName("clusters") 
    private List<ModelDnaCluster> dnaCluster = new ArrayList<>();
	
	/**
	 * Creates an instance of {@link ModelBelongsToClusters}.
	 * 
	 * @param moduleId the id of {@code module} entity
	 * @param dnaCluster the list of {@link ModelDnaCluster}
	 */
	public ModelBelongsToClusters(final EntityId moduleId, final List<ModelDnaCluster> dnaCluster) {
		this.moduleId = moduleId;
		this.dnaCluster = dnaCluster;
	}

	/**
	 * Creates an instance of {@link ModelBelongsToClusters}.
	 */
	public ModelBelongsToClusters() {}

	/**
	 * Gets the id of {@code module} entity.
	 *
	 * @return the id of {@code module} entity
	 */
	@Nullable
	public EntityId getModuleId() {
		return moduleId;
	}

	/**
	 * Sets the id of {@code module} entity.
	 *
	 * @param moduleId the id of {@code module} entity
	 */
	public void setModuleId(final EntityId moduleId) {
		this.moduleId = moduleId;
	}

	/**
	 * Gets the list of {@link ModelDnaCluster}.
	 *
	 * @return the list of {@link ModelDnaCluster}
	 */
	public List<ModelDnaCluster> getDnaCluster() {
		return dnaCluster;
	}

	/**
	 * Sets the list of {@link ModelDnaCluster}.
	 *
	 * @param dnaCluster the list of {@link ModelDnaCluster}
	 */
	@JsonSetter("clusters")
	public void setDnaCluster(final List<ModelDnaCluster> dnaCluster) {
		this.dnaCluster = dnaCluster;
	}

	@MiningDataType(name = "ModelDnaCluster")
	public class ModelDnaCluster {
        
		@SerializedName("clusterIndex")
        private Integer clusterIndex;
       
		@SerializedName("algorithm")
        private ModelDnaAlgorithm dnaAlgorithm;

		/**
		 * Creates an instance of {@link ModelDnaCluster}.
		 * 
		 * @param clusterIndex the index of the cluster
		 * @param dnaAlgorithm the {@link ModelDnaAlgorithm}
		 */
        public ModelDnaCluster(final Integer clusterIndex, final ModelDnaAlgorithm dnaAlgorithm) {
            this.clusterIndex = clusterIndex;
            this.dnaAlgorithm = dnaAlgorithm;
        }        

        /**
    	 * Gets the cluster index.
    	 *
    	 * @return the cluster index
    	 */
		public Integer getClusterIndex() {
			return clusterIndex;
		}

		/**
		 * Sets the cluster index.
		 *
		 * @param clusterIndex the cluster index
		 */
		@JsonSetter("clusterIndex")
		public void setClusterIndex(final Integer clusterIndex) {
			this.clusterIndex = clusterIndex;
		}

		/**
		 * Gets the {@link ModelDnaAlgorithm}.
		 *
		 * @return the {@link ModelDnaAlgorithm}
		 */
		public ModelDnaAlgorithm getDnaAlgorithm() {
			return dnaAlgorithm;
		}

		/**
		 * Sets {@link ModelDnaAlgorithm}.
		 *
		 * @param dnaAlgorithm {@link ModelDnaAlgorithm}
		 */
		@JsonSetter("algorithm")
		public void setDnaAlgorithm(final ModelDnaAlgorithm dnaAlgorithm) {
			this.dnaAlgorithm = dnaAlgorithm;
		}

    }
}