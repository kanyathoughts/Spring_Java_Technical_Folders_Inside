/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.util.Date;

import org.apache.commons.lang3.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.shared.model.IdentifiableAndNameableEntity;

/**
 * Dna snapshot that holds the configuration information for a specific Dna execution. We will create a Dna Snapshot only if there its a first run or re run 
 * with any one of the configuration changes or any updated modules available, other wise we will update the existing DNA snapshot.
 * A project can have multiple dna snapshots.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public class DnaSnapshot extends IdentifiableAndNameableEntity {

	@Nullable
	private Long projectId;
	@Nullable
	private Date updatedOn;
	private int totalModuleCount;
	private DnaConfig dnaConfig;

	/**
	 * Creates the DNA snapshot instance and sets the default values.
	 */
	public DnaSnapshot() {
		this.name = StringUtils.EMPTY;
		this.totalModuleCount = 0;
		this.dnaConfig = new DnaConfig();
	}

	/**
	 * Gets the id of Project.
	 *
	 * @return the id of Project
	 */
	public Long getProjectId() {
		return assertNotNull(projectId, "Project id cannot be null");
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
	 * Gets the updated on date.
	 *
	 * @return the updated on date
	 */
	public Date getUpdatedOn() {
		return assertNotNull(updatedOn, "Updated on date cannot be null");
	}

	/**
	 * Sets the updated on date.
	 *
	 * @param updatedOn the updated on date
	 */
	public void setUpdatedOn(final Date updatedOn) {
		this.updatedOn = updatedOn;
	}

	/**
	 * Gets the total module count, the total number of modules taken for DNA processing.
	 *
	 * @return the total module count
	 */
	public int getTotalModuleCount() {
		return totalModuleCount;
	}

	/**
	 * Sets the total module count, the total number of modules taken for DNA processing.
	 *
	 * @param totalModuleCount the total module count
	 */
	public void setTotalModuleCount(final int totalModuleCount) {
		this.totalModuleCount = totalModuleCount;
	}

	/**
	 * Gets the {@link DnaConfig}.
	 *
	 * @return the {@link DnaConfig}
	 */
	public DnaConfig getDnaConfig() {
		return dnaConfig;
	}
	
	/**
	 * Sets the {@link DnaConfig}.
	 *
	 * @param dnaConfig the {@link DnaConfig}
	 */
	public void setDnaConfig(final DnaConfig dnaConfig) {
		this.dnaConfig = dnaConfig;
	}
}
