/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package db.migration.model.legacy.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Entity;

/**
 * Represents one dna entry of a module unit.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public class DnaString extends Entity {

	@Nullable
	private Long projectId;
	@Nullable
	private SequencerId sequencerId;
	@Nullable
	private Date generatedOn;
	private int length;
	@Nullable
	private String moduleUnitRid;
	@Nullable
	private String contentHash;
	private List<DnaStringElement> dnaStringElements = Collections.emptyList();

	/**
	 * Gets the record id of {@code ModuleUnit}.
	 *
	 * @return the record id of {@code ModuleUnit}
	 */
	public String getModuleUnitRid() {
		return assertNotNull(moduleUnitRid, "Module unit record id cannot be null");
	}

	/**
	 * Sets the record id of {@code ModuleUnit}.
	 *
	 * @param moduleUnitRid the record id of {@code ModuleUnit}
	 */
	public void setModuleUnitRid(final String moduleUnitRid) {
		this.moduleUnitRid = moduleUnitRid;
	}

	/**
	 * Gets the content hash of {@code Module}.
	 *
	 * @return the content hash of {@code Module}
	 */
	public String getContentHash() {
		return assertNotNull(contentHash, "Content hash cannot be null");
	}

	/**
	 * Sets the content hash of {@code Module}.
	 *
	 * @param contentHash the content hash of {@code Module}
	 */
	public void setContentHash(final String contentHash) {
		this.contentHash = contentHash;
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
	 * Gets the {@link SequencerId}.
	 *
	 * @return the {@link SequencerId}
	 */
	public SequencerId getSequencerId() {
		return assertNotNull(sequencerId, "Sequencer id cannot be null");
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
	 * Gets the generated on date.
	 *
	 * @return the generated on date
	 */
	public Date getGeneratedOn() {
		return assertNotNull(generatedOn, "Generated on date cannot be null");
	}

	/**
	 * Sets the generated on date.
	 *
	 * @param generatedOn generated on date
	 */
	public void setGeneratedOn(final Date generatedOn) {
		this.generatedOn = generatedOn;
	}

	/**
	 * Gets the length of the DNA string.
	 *
	 * @return the length of the DNA string
	 */
	public int getLength() {
		return length;
	}

	/**
	 * Sets the length of the DNA string.
	 *
	 * @param length length of the DNA string
	 */
	public void setLength(final int length) {
		this.length = length;
	}
	

	/**
	 * Gets list of {@link DnaStringElement}s.
	 *
	 * @return list of {@link DnaStringElement}s
	 */
	public List<DnaStringElement> getDnaStringElements() {
		return dnaStringElements;
	}

	
	/**
	 * Sets list of {@link DnaStringElement}s.
	 *
	 * @param dnaStringElements list of {@link DnaStringElement}s
	 */
	public void setDnaStringElements(final List<DnaStringElement> dnaStringElements) {
		this.dnaStringElements = dnaStringElements;
	}
}
