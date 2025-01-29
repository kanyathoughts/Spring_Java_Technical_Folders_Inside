/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Backup of a single module including its metadata and properties that identify the module. This is intended to be serialized as JSON and stored
 * for backup purposes as part of {@link MetaDataBackup}.
 */
public class ModuleBackup implements Serializable {

	@Nullable
	private String name;
	@Nullable
	private String linkHash;
	@Nullable
	private String path;
	@Nullable
	private Technology technology;
	@Nullable
	private Type type;
	
	@Nullable
	private byte[] contentHash;
	
	@Nullable
	protected Map<String, Object> customProperties;

	@Nullable
	private String description;
	
	@Nullable
	private List<AnnotationPojo> annotations;
	@Nullable
	private List<DataDictionaryPojo> dataDictionaryEntries;
	@Nullable
	private List<TaxonomyPojo> taxonomies;
	
	private boolean requiresReview;

	/**
	 * Returns the name of the Module.
	 *
	 * @return the name of the Module
	 */
	public String getName() {
		return assertNotNull(name, "Module name must not be null");
	}
	
	/**
	 * Sets the name of the Module.
	 *
	 * @param name the name of the Module
	 */
	public void setName(final String name) {
		this.name = name;
	}
	
	/**
	 * Returns the path of the Module.
	 *
	 * @return the path of the Module
	 */
	@Nullable
	public String getPath() {
		return path;
	}
	
	/**
	 * Returns the path of the Module.
	 *
	 * @param path the path of the Module
	 */
	public void setPath(@Nullable final String path) {
		this.path = path;
	}
	
	/**
	 * Returns the Technology of the Module.
	 *
	 * @return the Technology of the Module
	 */
	public Technology getTechnology() {
		return assertNotNull(technology, "Module technology must not be null");
	}
	
	/**
	 * Sets the Technology of the Module.
	 *
	 * @param technology the Technology of the Module
	 */
	public void setTechnology(final Technology technology) {
		this.technology = technology;
	}
	
	/**
	 * Returns the Type of the Module.
	 *
	 * @return the Type of the Module
	 */
	public Type getType() {
		return assertNotNull(type, "Module type must not be null");
	}
	
	/**
	 * Sets the Type of the Module.
	 *
	 * @param type the Type of the Module
	 */
	public void setType(final Type type) {
		this.type = type;
	}
	
	/**
	 * Returns the Module description.
	 *
	 * @return the Module description
	 */
	@Nullable
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the Module description.
	 *
	 * @param description the Module description
	 */
	public void setDescription(@Nullable final String description) {
		this.description = description;
	}

	/**
	 * Returns the {@link Map} containing  {@link List} of CustomProperties.
	 *
	 * @return the {@link Map} containing  {@link List} of CustomProperties.
	 */
	@Nullable
	public Map<String, Object> getCustomProperties() {
		return customProperties;
	}

	/**
	 * Sets the {@link Map} containing  {@link List} of CustomProperties
	 *
	 * @param customProperties the {@link Map} containing  {@link List} of CustomProperties.
	 */
	public void setCustomProperties(final Map<String, Object> customProperties) {
		this.customProperties = customProperties;
	}
	
	/**
	 * Returns the Annotations of the Module.
	 *
	 * @return the Annotations of the Module
	 */
	@Nullable
	public List<AnnotationPojo> getAnnotations() {
		return annotations;
	}
	
	/**
	 * Sets the Annotations of the Module.
	 *
	 * @param annotations the Annotations of the Module
	 */
	public void setAnnotations(final List<AnnotationPojo> annotations) {
		this.annotations = annotations;
	}
	
	/**
	 * Returns the Data Dictionary Entries of the Module.
	 *
	 * @return the Data Dictionary Entries of the Module
	 */
	@Nullable
	public List<DataDictionaryPojo> getDataDictionaryEntries() {
		return dataDictionaryEntries;
	}
	
	/**
	 * Sets the Data Dictionary Entries of the Module.
	 *
	 * @param dataDictionaryEntries the Data Dictionary Entries of the Module
	 */
	public void setDataDictionaryEntries(final List<DataDictionaryPojo> dataDictionaryEntries) {
		this.dataDictionaryEntries = dataDictionaryEntries;
	}
	
	/**
	 * Returns the Taxonomies of the Module.
	 *
	 * @return the Taxonomies of the Module
	 */
	@Nullable
	public List<TaxonomyPojo> getTaxonomies() {
		return taxonomies;
	}
	
	/**
	 * Sets the Taxonomies of the Module.
	 *
	 * @param taxonomies the Taxonomies of the Module
	 */
	public void setTaxonomies(final List<TaxonomyPojo> taxonomies) {
		this.taxonomies = taxonomies;
	}

	/**
	 * Returns the content hash of the Module or {@code null} if this Module has no source code attached to it.
	 *
	 * @return the content hash or {@code null}
	 */
	@Nullable
	public byte[] getContentHash() {
		return contentHash;
	}

	/**
	 * Sets the content hash of the Module.
	 *
	 * @param contentHash the content hash
	 */
	public void setContentHash(@Nullable final byte[] contentHash) {
		this.contentHash = contentHash;
	}

	/**
	 * Returns whether the module has changes that require review by a user.
	 *
	 * @return true, if the module has changes that require review by a user
	 */
	public boolean isRequiresReview() {
		return requiresReview;
	}

	/**
	 * Sets true if the the module has changes that require review by a user.
	 *
	 * @param requiresReview true or false
	 */
	public void setRequiresReview(final boolean requiresReview) {
		this.requiresReview = requiresReview;
	}

	/**
	 * return the linkHash of the module 
	 *
	 * @return the linkHash of the module
	 */
	public @Nullable String getLinkHash() {
		return linkHash;
	}

	/**
	 * Sets the linkHash of the Module.
	 *
	 * @param linkHash the linkHash
	 */
	public void setLinkHash(@Nullable final String linkHash) {
		this.linkHash = linkHash;
	}

	public static class HasBusinessRules {
		private @Nullable DataDictionaryPojo dde;
		private @Nullable AnnotationPojo annotation;
		
		/**
		 * Constructor
		 * 
		 * @param dde The DataDictionaryEntry to link
		 * @param annotation The Annotation to link
		 */
		public HasBusinessRules(final DataDictionaryPojo dde, final AnnotationPojo annotation) {
			this.dde = dde;
			this.annotation = annotation;
		}
		
		/**
		 * Constructor for JSON
		 */
		public HasBusinessRules() {
			super();
		}

		/**
		 * Returns the DataDictionaryEntry of the pair 
		 *
		 * @return Returns the DataDictionaryEntry of the pair
		 */
		public @Nullable DataDictionaryPojo getDde() {
			return dde;
		}

		/**
		 * Returns the Annotation of the pair
		 *
		 * @return Returns the Annotation of the pair
		 */
		public @Nullable AnnotationPojo getAnnotation() {
			return annotation;
		}
	}

	public static class DataDictionaryAnnotationLink {
		@Nullable private final UUID dataDictionaryUid;
		@Nullable private final UUID annotationUid;

		public DataDictionaryAnnotationLink(@JsonProperty("dataDictionaryUid") final UUID dataDictionaryUid, @JsonProperty("annotationUid") final UUID annotationUid) {
			this.dataDictionaryUid = dataDictionaryUid;
			this.annotationUid = annotationUid;
		}

		public @Nullable UUID getDataDictionaryUid() {
			return dataDictionaryUid;
		}

		public @Nullable UUID getAnnotationUid() {
			return annotationUid;
		}
	}
}
