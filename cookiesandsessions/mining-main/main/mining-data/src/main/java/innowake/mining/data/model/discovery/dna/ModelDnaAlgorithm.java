/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.model.discovery.dna;

import com.fasterxml.jackson.annotation.JsonSetter;
import com.google.gson.annotations.SerializedName;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;

/**
 * Model of DNA Algorithm.
 */
@MiningDataType(name = "ModelDnaAlgorithm")
public class ModelDnaAlgorithm {
    
	@SerializedName("Sequencer")  
    private String sequencerId;
	
    @SerializedName("Similarity") 
    private String similarityId;
    
    @SerializedName("Clustering") 
    private String clusterAlgorithmId;
    
    /**
	 * Creates an instance of {@link ModelDnaAlgorithm}.
	 * 
	 * @param sequencerId the {@link DnaSequencer}
	 * @param similarityId the {@link DnaSimilarityAlgorithm}
	 * @param clusterAlgorithmId the {@link DnaClusterAlgorithm}
	 */
	public ModelDnaAlgorithm(final DnaSequencer sequencerId, final DnaSimilarityAlgorithm similarityId, final DnaClusterAlgorithm clusterAlgorithmId) {
		this.sequencerId = sequencerId.getId();
		this.similarityId = similarityId.getId();
		this.clusterAlgorithmId = clusterAlgorithmId.getId();
	}
    
    /**
	 * Creates an instance of {@link ModelDnaAlgorithm}.
	 * 
	 * @param sequencerId the {@link DnaSequencer}
	 * @param similarityId the {@link DnaSimilarityAlgorithm}
	 * @param clusterAlgorithmId the {@link DnaClusterAlgorithm}
	 */
	public ModelDnaAlgorithm(final String sequencerId, final String similarityId, final String clusterAlgorithmId) {
		this.sequencerId = sequencerId;
		this.similarityId = similarityId;
		this.clusterAlgorithmId = clusterAlgorithmId;
	}

	/**
	 * Gets the {@link DnaSequencer}.
	 *
	 * @return the {@link DnaSequencer}
	 */
	public String getSequencerId() {
		return sequencerId;
	}

	/**
	 * Sets the {@link DnaSequencer}.
	 *
	 * @param sequencerId the {@link DnaSequencer}
	 */
	@JsonSetter("Sequencer")
	public void setSequencerId(final String sequencerId) {
		this.sequencerId = sequencerId;
	}

	/**
	 * Gets the {@link DnaSimilarityAlgorithm}.
	 *
	 * @return the {@link DnaSimilarityAlgorithm}
	 */
	public String getSimilarityId() {
		return similarityId;
	}

	/**
	 * Sets the {@link DnaSimilarityAlgorithm}.
	 *
	 * @param similarityId the {@link DnaSimilarityAlgorithm}
	 */
	@JsonSetter("Similarity")
	public void setSimilarityId(final String similarityId) {
		this.similarityId = similarityId;
	}

	/**
	 * Gets the {@link DnaClusterAlgorithm}.
	 *
	 * @return the {@link DnaClusterAlgorithm}
	 */
	public String getClusterAlgorithmId() {
		return clusterAlgorithmId;
	}

	/**
	 * Sets the {@link DnaClusterAlgorithm}.
	 *
	 * @param clusterAlgorithmId the {@link DnaClusterAlgorithm}
	 */
	@JsonSetter("Clustering")
	public void setClusterAlgorithmId(final String clusterAlgorithmId) {
		this.clusterAlgorithmId = clusterAlgorithmId;
	}
	        
}