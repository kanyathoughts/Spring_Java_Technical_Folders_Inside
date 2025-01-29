/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package db.migration.model.legacy.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.model.Entity;

/**
 * Represent one similarity entry between two dna strings with a double similarity between 0-1.
 * To avoid too much data, it is recommended to configure a threshold of minimum similarity.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public class DnaSimilarity extends Entity {

	@Nullable
	private Long projectId;
	@Nullable
	private SequencerId sequencerId;
	@Nullable
	private DnaSimilarityAlgorithm similarityId;
	private double similarity;
	@Nullable
	private String fromDnaStringRecordId;
	@Nullable
	private String toDnaStringRecordId;

	/**
	 * Creates an empty {@link DnaSimilarity} instance.
	 */
	public DnaSimilarity() {}

	/**
	 * Creates a new {@link DnaSimilarity}.
	 *
	 * @param projectId the id of the Project
	 * @param sequencerId the {@link SequencerId}
	 * @param similarityId the {@link DnaSimilarityAlgorithm}
	 * @param similarity similarity score for a pair of {@link DnaString}s
	 * @param fromDnaStringRecordId the record id of {@link DnaString}
	 * @param toDnaStringRecordId the record id of {@link DnaString}
	 */
	public DnaSimilarity(final Long projectId, final SequencerId sequencerId, final DnaSimilarityAlgorithm similarityId,
			final double similarity, final String fromDnaStringRecordId, final String toDnaStringRecordId){
		this.projectId = projectId;
		this.sequencerId = sequencerId;
		this.similarityId = similarityId;
		this.similarity = similarity;
		this.fromDnaStringRecordId = fromDnaStringRecordId;
		this.toDnaStringRecordId = toDnaStringRecordId;
	}
	
	/**
	 * Creates a new {@link DnaSimilarity}.
	 *
	 * @param fromDnaStringRecordId the record id of {@link DnaString}
	 * @param toDnaStringRecordId the record id of {@link DnaString}
	 *  @param similarity similarity score for a pair of {@link DnaString}s
	 */
	public DnaSimilarity(final String fromDnaStringRecordId, final String toDnaStringRecordId, final double similarity) {
		this.fromDnaStringRecordId = fromDnaStringRecordId;
		this.toDnaStringRecordId = toDnaStringRecordId;
		this.similarity = similarity;
	}

	/**
	 * Gets the id of Project.
	 *
	 * @return the id of Project
	 */
	public Long getProjectId() {
		return assertNotNull(projectId, "Project id must not be null");
	}

	/**
	 * Sets the id of Project.
	 *
	 * @param projectId the id of Project
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = projectId;
	}

	/**
	 * Gets the {@link SequencerId}.
	 *
	 * @return the {@link SequencerId}
	 */
	public SequencerId getSequencerId() {
		return assertNotNull(sequencerId, "Sequencer id must not be null");
	}

	/**
	 * Sets the {@link SequencerId}.
	 *
	 * @param sequencerId the {@link SequencerId}
	 */
	public void setSequencerId(final SequencerId sequencerId) {
		this.sequencerId = sequencerId;
	}

	/**
	 * Gets the {@link DnaSimilarityAlgorithm}.
	 *
	 * @return the {@link DnaSimilarityAlgorithm}
	 */
	public DnaSimilarityAlgorithm getSimilarityId() {
		return assertNotNull(similarityId, "Similarity id must not be null");
	}

	/**
	 * Sets the {@link DnaSimilarityAlgorithm}.
	 *
	 * @param similarityId the {@link DnaSimilarityAlgorithm}
	 */
	public void setSimilarityId(final DnaSimilarityAlgorithm similarityId) {
		this.similarityId = similarityId;
	}

	/**
	 * Gets the similarity.
	 *
	 * @return the similarity
	 */
	public double getSimilarity() {
		return similarity;
	}

	/**
	 * Sets the similarity.
	 *
	 * @param similarity the similarity
	 */
	public void setSimilarity(final double similarity) {
		this.similarity = similarity;
	}

	/**
	 * Gets the record id of the {@link DnaString}.
	 *
	 * @return the record id of the {@link DnaString}
	 */
	public String getFromDnaStringRecordId() {
		return assertNotNull(fromDnaStringRecordId, "From Dna String record id must not be null");
	}

	/**
	 * Sets the record id of the {@link DnaString}.
	 *
	 * @param fromDnaStringRecordId record id of the {@link DnaString}
	 */
	public void setFromDnaStringRecordId(final String fromDnaStringRecordId) {
		this.fromDnaStringRecordId = fromDnaStringRecordId;
	}

	/**
	 * Gets the record id of the {@link DnaString}.
	 *
	 * @return the record id of the {@link DnaString}
	 */
	public String getToDnaStringRecordId() {
		return assertNotNull(toDnaStringRecordId, "To Dna String record id must not be null");
	}

	/**
	 * Sets the record id of the {@link DnaString}.
	 *
	 * @param toDnaStringRecordId record id of the {@link DnaString}
	 */
	public void setToDnaStringRecordId(final String toDnaStringRecordId) {
		this.toDnaStringRecordId = toDnaStringRecordId;
	}
}
