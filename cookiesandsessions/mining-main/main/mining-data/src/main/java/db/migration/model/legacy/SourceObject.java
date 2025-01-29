/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy;


import static innowake.lib.core.lang.Assert.assertNotNull;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.IdentifiableAndNameableEntity;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * A mining source object.
 * @deprecated Required for Java based OrientDB migration scripts. Will be removed once migration from to Postgres is done.
 */
@Deprecated(forRemoval = true)
public class SourceObject extends IdentifiableAndNameableEntity {

	protected Long projectId;
	protected String path;
	protected Technology technology;
	protected Type type;
	
	protected Long metaDataRevision;
	protected Long contentRevision;
	
	@Nullable
	protected String content;

	private String contentHash;
	public transient static final long CONTENT_SIZE_THRESHOLD_FOR_LIMIT_CHECK = 4000000; /* Threshold to check actual content limit */
	public transient static final long CONTENT_SIZE_LIMIT = 16677216; /*Safe limit although it supports up to 16777216 bytes */

	/**
	 * Creates a new SourceObject without content.
	 *  
	 * @param projectId id of the project this source object belongs to
	 * @param name name of the source object
	 * @param path path of the source object
	 * @param technology technology 
	 * @param type type of the source object
	 * @param metaDataRevision current meta data revision of the source object
	 * @param contentRevision current content revision of the source object
	 * @param contentHash the hash of the content
	 */
	public SourceObject(final Long projectId, final String name, final String path, final Technology technology, final Type type, final Long metaDataRevision,
			final Long contentRevision, @Nullable final String contentHash) {
		this(projectId, name, path, technology, type, metaDataRevision, contentRevision, null, contentHash);
	}
	
	

	/**
	 * Creates a new SourceObject with specific content.
	 *  
	 * @param projectId id of the project this source object belongs to
	 * @param name name of the source object
	 * @param path path of the source object
	 * @param technology technology 
	 * @param type type of the source object
	 * @param metaDataRevision current meta data revision of the source object
	 * @param contentRevision current content revision of the source object
	 * @param content content (source code) of the source object
	 * @param contentHash the hash calculated for the content
	 */
	public SourceObject(final Long projectId, final String name, final String path, final Technology technology, final Type type, final Long metaDataRevision, final Long contentRevision,
			@Nullable final String content, @Nullable final String contentHash) {
		this.projectId = projectId;
		this.path = path;
		this.technology = technology;
		this.type = type;
		this.metaDataRevision = metaDataRevision;
		this.contentRevision = contentRevision;
		this.content = content;
		if (content != null) {
			this.contentHash = assertNotNull(contentHash, "Content hash should not be null");
		} else {
			this.contentHash = CityHash.EMPTY_CONTENT_HASH;
		}
		setName(name);
	}
	
	/**
	 * Validates the given content if it can be uploaded to database.
	 * If the validation fails, it throws {@link IllegalArgumentException}
	 *
	 * @param content the content string to be validated
	 * @throws ValidationException thrown when the validation fails
	 */
	public static void validateContent(final String content) throws ValidationException {
		final int contentLengthAfterEncoding = content.getBytes(StandardCharsets.UTF_8).length;
		if (contentLengthAfterEncoding > CONTENT_SIZE_LIMIT) {
			throw new ValidationException(
					"File content after encoding exceeds the allowable limit. File content size after encoding: " + contentLengthAfterEncoding + " bytes");
		}
	}

	/**
	 * Returns the content (i.e. source code).
	 *
	 * @return the content
	 */
	public String getContent() {
		final String contentNonNull = content;
		return contentNonNull == null ? "" : contentNonNull;
	}
	
	/**
	 * Returns whether the content of this source object has been set.
	 * This is important for the content caching in LazySourceObject.
	 * 
	 * @return is content set
	 */
	public boolean isContentSet() {
		return content != null;
	}

	/**
	 * Set the content of the source object.
	 * 
	 * @param content the new content 
	 */
	public void setContent(final String content) {
		this.content = content;
	}
	
	/**
	 * Returns the project ID.
	 * 
	 * @return the project ID
	 */
	public Long getProjectId() {
		return projectId;
	}
	
	/**
	 * Sets the project ID.
	 *
	 * @param projectId the project ID
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = projectId;
	}
	
	/**
	 * Returns the path.
	 * 
	 * @return the path
	 */
	public String getPath() {
		return path;
	}
	
	/**
	 * Sets the path of this SourceObject.
	 *
	 * @param path Path String.
	 */
	public void setPath(final String path) {
		this.path = path;
	}
	
	/**
	 * Returns the {@link Technology}.
	 * 
	 * @return the {@link Technology}
	 */
	public Technology getTechnology() {
		return technology;
	}
	
	/**
	 * Sets the {@link Technology}
	 *
	 * @param technology {@link Technology}
	 */
	public void setTechnology(final Technology technology) {
		this.technology = technology;
	}
	
	/**
	 * Returns the {@link Type}.
	 * 
	 * @return the {@link Type}
	 */
	public Type getType() {
		return type;
	}
	
	/**
	 * Sets the {@link Type}
	 *
	 * @param type {@link Type}
	 */
	public void setType(final Type type) {
		this.type = type;
	}
	
	/**
	 * Returns the meta data revision.
	 *
	 * @return the meta data revision
	 */
	public Long getMetaDataRevision() {
		return metaDataRevision;
	}
	
	/**
	 * Sets the meta data revision.
	 *
	 * @param metaDataRevision the meta data revision
	 */
	public void setMetaDataRevision(final Long metaDataRevision) {
		this.metaDataRevision = metaDataRevision;
	}
	
	/**
	 * Returns the contentRevision.
	 *
	 * @return the contentRevision
	 */
	public Long getContentRevision() {
		return contentRevision;
	}
	
	/**
	 * Sets the content revision.
	 *
	 * @param contentRevision the content revision
	 */
	public void setContentRevision(final Long contentRevision) {
		this.contentRevision = contentRevision;
	}
	
	/**
	 * Returns the hash value of the content.
	 *
	 * @return the hash value of the content
	 */
	public String getContentHash() {
		return assertNotNull(contentHash);
	}
	
	/**
	 * Sets the hash value of the content.
	 *
	 * @param contentHash the hash value of the content
	 */
	public void setContentHash(final String contentHash) {
		this.contentHash = contentHash;
	}

	/* Note: equals() and hashCode() do not include the content property, for performance reasons */

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(contentRevision, metaDataRevision, path, projectId, technology, type);
		return result;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj) {
			return true;
		}
		if ( ! (obj instanceof SourceObject)) {
			return false;
		}
		if ( ! super.equals(obj)) {
			return false;
		}
		SourceObject other = (SourceObject) obj;
		return Objects.equals(contentRevision, other.contentRevision)
				&& Objects.equals(metaDataRevision, other.metaDataRevision)
				&& Objects.equals(path, other.path)
				&& Objects.equals(projectId, other.projectId)
				&& technology == other.technology
				&& type == other.type;
	}
	
	/**
	 * Validation exception for {@link SourceObject}.
	 */
	public static class ValidationException extends Exception {
		/**
		 * Constructor.
		 * 
		 * @param message Cause of validation failure
		 */
		public ValidationException(final String message) {
			super(message);
		}
	}
}
