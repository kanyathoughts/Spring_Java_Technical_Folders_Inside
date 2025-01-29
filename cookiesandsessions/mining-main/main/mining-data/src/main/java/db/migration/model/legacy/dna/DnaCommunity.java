/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Entity;

/**
 * Represent one community which is a result of the community detection.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public class DnaCommunity extends Entity {

	@Nullable
	private Long projectId;
	@Nullable
	private String snapshotRecordId;
	@Nullable
	private SequencerId sequencerId;
	@Nullable
	private SimilarityId similarityId;
	@Nullable
	private ClusterAlgorithmId clusterAlgorithmId;
	@Nullable
	private String title;
	@Nullable
	private Integer clusterIndex;
	private int count;

	private List<String> moduleUnitRids = Collections.emptyList();

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
	 * @param sequencerId {@link SequencerId}
	 */
	public void setSequencerId(final SequencerId sequencerId) {
		this.sequencerId = sequencerId;
	}

	/**
	 * Gets the {@link SimilarityId}.
	 *
	 * @return the {@link SimilarityId}
	 */
	public SimilarityId getSimilarityId() {
		return assertNotNull(similarityId, "Similarity id must not be null");
	}

	/**
	 * Sets the {@link SimilarityId}.
	 *
	 * @param similarityId {@link SimilarityId}
	 */
	public void setSimilarityId(final SimilarityId similarityId) {
		this.similarityId = similarityId;
	}

	/**
	 * Gets the record id of the {@link DnaSnapshot}.
	 *
	 * @return the record id of the {@link DnaSnapshot}
	 */
	public String getSnapshotRecordId() {
		return assertNotNull(snapshotRecordId, "record id of the DnaSnapshot must not be null");
	}

	/**
	 * Sets record id of the {@link DnaSnapshot}.
	 *
	 * @param snapshotRecordId record id of the {@link DnaSnapshot}
	 */
	public void setSnapshotRecordId(final String snapshotRecordId) {
		this.snapshotRecordId = snapshotRecordId;
	}

	/**
	 * Gets the {@link ClusterAlgorithmId}.
	 *
	 * @return the {@link ClusterAlgorithmId}
	 */
	public ClusterAlgorithmId getClusterAlgorithmId() {
		return assertNotNull(clusterAlgorithmId, "Cluster algorithm id must not be null");
	}

	/**
	 * Sets {@link ClusterAlgorithmId}.
	 *
	 * @param clusterAlgorithmId {@link ClusterAlgorithmId}
	 */
	public void setClusterAlgorithmId(final ClusterAlgorithmId clusterAlgorithmId) {
		this.clusterAlgorithmId = clusterAlgorithmId;
	}

	/**
	 * Gets the title.
	 *
	 * @return the title
	 */
	public String getTitle() {
		return title != null ? title : StringUtils.EMPTY;
	}

	/**
	 * Sets the title.
	 *
	 * @param title the title
	 */
	public void setTitle(final String title) {
		this.title = title;
	}

	/**
	 * Gets the cluster index.
	 *
	 * @return the cluster index
	 */
	public Integer getClusterIndex() {
		return assertNotNull(clusterIndex, "Cluster index must not be null");
	}

	/**
	 * Sets the cluster index.
	 *
	 * @param clusterIndex the cluster index
	 */
	public void setClusterIndex(final Integer clusterIndex) {
		this.clusterIndex = clusterIndex;
	}

	/**
	 * Gets the list of record id of {@code ModuleUnit}s.
	 *
	 * @return the the list of record id of {@code ModuleUnit}s
	 */
	public List<String> getModuleUnitRids() {
		return moduleUnitRids;
	}

	/**
	 * Sets the list of record id of {@code ModuleUnit}s.
	 *
	 * @param moduleUnitRids the list of record id of {@code ModuleUnit}s
	 */
	public void setModuleUnitRids(final List<String> moduleUnitRids) {
		this.moduleUnitRids = moduleUnitRids;
	}

	/**
	 * Gets the count of {@code ModuleUnit}s.
	 *
	 * @return the count of {@code ModuleUnit}s
	 */
	public int getCount() {
		return count;
	}

	/**
	 * Sets the count of {@code ModuleUnit}s.
	 *
	 * @param count the count of {@code ModuleUnit}s
	 */
	public void setCount(final int count) {
		this.count = count;
	}
}
