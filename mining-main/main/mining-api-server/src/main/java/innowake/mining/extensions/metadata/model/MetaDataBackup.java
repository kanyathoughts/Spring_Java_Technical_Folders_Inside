/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.server.functionalblocks.backup.model.FunctionalBlockBackup;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Root of a meta data backup. This is intended to be serialized as JSON and stored for backup/restore purposes.
 */
public class MetaDataBackup {

	@Nullable
	private String apiServerVersion;
	@Nullable
	private Date backupDate;
	@Nullable
	private List<TaxonomyPojo> taxonomies;
	@Nullable
	private List<AnnotationCategory> annotationCategories;
	@Nullable
	private List<ModuleBackup> modules;
	@Nullable
	private TaxonomyCategoryPojo technicalTaxonomyCategory;
	@Nullable
	private TaxonomyCategoryPojo defaultTaxonomyCategory;
	@Nullable
	private Set<TaxonomyTypePojo> defaultCategoryTaxonomyTypes;
	@Nullable
	private Set<TaxonomyTypePojo> technicalCategoryTaxonomyTypes;
	@Nullable
	private Map<String, List<CustomPropertyMetadata>> entityCustomProperties;
	@Nullable
	private List<FunctionalBlockBackup> functionalBlocks;
	
	/**
	 * Returns the version of the api server that created this backup.
	 *
	 * @return version string of the api server that created this backup
	 */
	public String getApiServerVersion() {
		return assertNotNull(apiServerVersion, "Api version must not be null.");
	}
	
	/**
	 * Sets the version of the api server that created this backup.
	 *
	 * @param apiServerVersion version string of the api server that created this backup
	 */
	public void setApiServerVersion(final String apiServerVersion) {
		this.apiServerVersion = apiServerVersion;
	}
	
	/**
	 * Returns the date when this backup file was created.
	 *
	 * @return the date when this backup file was created
	 */
	public Date getBackupDate() {
		return assertNotNull(backupDate, "Back up date must not be null.");
	}
	
	/**
	 * Sets the date when this backup file was created.
	 *
	 * @param backupDate the date when this backup file was created
	 */
	public void setBackupDate(final Date backupDate) {
		this.backupDate = backupDate;
	}
	
	/**
	 * Returns the existing {@linkplain TaxonomyPojo taxonomies}.
	 *
	 * @return the existing {@linkplain TaxonomyPojo taxonomies}
	 */
	@Nullable
	public List<TaxonomyPojo> getTaxonomies() {
		return taxonomies;
	}

	/**
	 * Sets the existing {@linkplain TaxonomyPojo taxonomies}.
	 *
	 * @param taxonomies the existing {@linkplain TaxonomyPojo taxonomies}
	 */
	public void setTaxonomies(final List<TaxonomyPojo> taxonomies) {
		this.taxonomies = taxonomies;
	}

	/**
	 * Returns the {@linkplain AnnotationCategory annotation categories}.
	 *
	 * @return the {@linkplain AnnotationCategory annotation categories}
	 */
	@Nullable
	public List<AnnotationCategory> getAnnotationCategories() {
		return annotationCategories;
	}

	/**
	 * Sets the {@linkplain AnnotationCategory annotation categories}.
	 *
	 * @param annotationCategories the {@linkplain AnnotationCategory annotation categories}
	 */
	public void setAnnotationCategories(final List<AnnotationCategory> annotationCategories) {
		this.annotationCategories = annotationCategories;
	}
	
	/**
	 * Returns the modules (with metadata) that are included in the backup.
	 *
	 * @return the modules that are included in the backup
	 */
	@Nullable
	public List<ModuleBackup> getModules() {
		return modules;
	}
	
	/**
	 * Sets the modules (with metadata) that are included in the backup.
	 *
	 * @param modules the modules that are included in the backup
	 */
	public void setModules(final List<ModuleBackup> modules) {
		this.modules = modules;
	}
	
	/**
	 * Gets the technical taxonomy category of the project.
	 *
	 * @return technicalTaxonomyCategory the {@linkplain TaxonomyCategoryPojo taxonomy category}
	 */
	public TaxonomyCategoryPojo getTechnicalTaxonomyCategory() {
		return assertNotNull(technicalTaxonomyCategory, "Technical taxonomy category can not be null");
	}

	/**
	 * Sets the technical taxonomy category of the project.
	 *
	 * @param technicalTaxonomyCategory the {@linkplain TaxonomyCategoryPojo taxonomy category} to set
	 */
	public void setTechnicalTaxonomyCategory(final TaxonomyCategoryPojo technicalTaxonomyCategory) {
		this.technicalTaxonomyCategory = technicalTaxonomyCategory;
	}

	/**
	 * Gets the default taxonomy category of the project.
	 *
	 * @return defaultTaxonomyCategory the {@linkplain TaxonomyCategoryPojo taxonomy category}
	 */
	public TaxonomyCategoryPojo getDefaultTaxonomyCategory() {
		return assertNotNull(defaultTaxonomyCategory, "Default taxonomy category can not be null");
	}

	/**
	 * Sets the default taxonomy category of the project
	 *
	 * @param defaultTaxonomyCategory the {@linkplain TaxonomyCategoryPojo taxonomy category} to set
	 */
	public void setDefaultTaxonomyCategory(final TaxonomyCategoryPojo defaultTaxonomyCategory) {
		this.defaultTaxonomyCategory = defaultTaxonomyCategory;
	}

	/**
	 * Gets {@linkplain TaxonomyTypePojo taxonomy types} with default taxonomy category.
	 *
	 * @return the {@linkplain TaxonomyTypePojo taxonomy types} with default taxonomy category
	 */
	@Nullable
	public Set<TaxonomyTypePojo> getDefaultCategoryTaxonomyTypes() {
		return defaultCategoryTaxonomyTypes;
	}

	/**
	 * Sets {@linkplain TaxonomyTypePojo taxonomy types} with default taxonomy category.
	 *
	 * @param defaultCategoryTaxonomyTypes the {@linkplain TaxonomyTypePojo taxonomy types} with default taxonomy category
	 */
	public void setDefaultCategoryTaxonomyTypes(final Set<TaxonomyTypePojo> defaultCategoryTaxonomyTypes) {
		this.defaultCategoryTaxonomyTypes = defaultCategoryTaxonomyTypes;
	}

	/**
	 * Gets {@linkplain TaxonomyTypePojo taxonomy types} with technical taxonomy category.
	 *
	 * @return the {@linkplain TaxonomyTypePojo taxonomy types} with technical taxonomy category
	 */
	@Nullable
	public Set<TaxonomyTypePojo> getTechnicalCategoryTaxonomyTypes() {
		return technicalCategoryTaxonomyTypes;
	}

	/**
	 * Sets the {@linkplain TaxonomyTypePojo taxonomy types} with technical taxonomy category.
	 *
	 * @param technicalCategoryTaxonomyTypes the {@linkplain TaxonomyTypePojo taxonomy types} with technical taxonomy category
	 */
	public void setTechnicalCategoryTaxonomyTypes(final Set<TaxonomyTypePojo> technicalCategoryTaxonomyTypes) {
		this.technicalCategoryTaxonomyTypes = technicalCategoryTaxonomyTypes;
	}

	/**
	 * Gets a Map of {@linkplain CustomPropertyMetadata}
	 *
	 * @return a Map of {@linkplain CustomPropertyMetadata}
	 */
	@Nullable
	public Map<String, List<CustomPropertyMetadata>> getEntityCustomProperties() {
		return entityCustomProperties;
	}

	/**
	 * Sets a Map of {@linkplain CustomPropertyMetadata}
	 *
	 * @param entityCustomProperties Map of {@linkplain CustomPropertyMetadata}
	 */
	public void setEntityCustomProperties(final Map<String, List<CustomPropertyMetadata>> entityCustomProperties) {
		this.entityCustomProperties = entityCustomProperties;
	}

	/**
	 * Gets a list of functional blocks contained in the backup.
	 * @return list of functional blocks contained in the backup
	 */
	@Nullable
	public List<FunctionalBlockBackup> getFunctionalBlocks() {
		return functionalBlocks;
	}

	/**
	 * Adds functional blocks to the backup.
	 * @param functionalBlocks list of functional blocks to back up
	 */
	public void setFunctionalBlocks(@Nullable final List<FunctionalBlockBackup> functionalBlocks) {
		this.functionalBlocks = functionalBlocks;
	}
}
