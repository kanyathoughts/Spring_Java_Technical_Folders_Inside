/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;

/**
 * Model class for Annotation Reporting request.
 */
public class AnnotationReportSearchParameter {

	@Nullable
	private String description;
	@Nullable
	private String updatedBy;
	@Nullable
	private String moduleName;
	@Nullable
	private String moduleTaxonomy;
	@Nullable
	private AnnotationType annotationType;
	@Nullable
	private String annotationCategory;

	/**
	 * Gets the description for search pattern criteria.
	 * 
	 * @return the description for search pattern criteria
	 */
	@Nullable
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the description for the search pattern criteria.
	 * 
	 * @param description the description for search pattern criteria
	 */
	public void setDescription(@Nullable final String description) {
		this.description = description;
	}

	/**
	 * Gets the userId that last modified for the search pattern criteria.
	 *   
	 * @return the userId that last modified the search pattern criteria
	 */
	@Nullable
	public String getUpdatedBy() {
		return updatedBy;
	}

	/**
	 * Sets the userId that last modified for the search pattern criteria.
	 * 
	 * @param updatedBy the userId that last modified the search pattern criteria
	 */
	public void setUpdatedBy(@Nullable final String updatedBy) {
		this.updatedBy = updatedBy;
	}

	/**
	 * Gets the module name that the search pattern criteria is linked to.
	 * 
	 * @return the module name that the search pattern criteria is linked to
	 */
	@Nullable
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the module name that the search pattern criteria is linked to.
	 * 
	 * @param moduleName the module name that the search pattern criteria is linked to
	 */
	public void setModuleName(@Nullable final String moduleName) {
		this.moduleName = moduleName;
	}

	/**
	 * Gets the module taxonomy of the search pattern criteria.
	 * 
	 * @return the module taxonomy of the search pattern criteria
	 */
	@Nullable
	public String getModuleTaxonomy() {
		return moduleTaxonomy;
	}

	/**
	 * Sets the module taxonomy of the search pattern criteria.
	 * 
	 * @param moduleTaxonomy the module taxonomy of the search pattern criteria
	 */
	public void setModuleTaxonomy(@Nullable final String moduleTaxonomy) {
		this.moduleTaxonomy = moduleTaxonomy;
	}

	/**
	 * Gets the {@link AnnotationType} of the search pattern criteria.
	 * 
	 * @return the {@link AnnotationType} of the search pattern criteria
	 */
	@Nullable
	public AnnotationType getAnnotationType() {
		return annotationType;
	}

	/**
	 * Sets the {@link AnnotationType} of the search pattern criteria.
	 * 
	 * @param annotationType the {@link AnnotationType} of the search pattern criteria
	 */
	public void setAnnotationType(final String annotationType) {
		if (StringUtils.isNotEmpty(annotationType)) {
			this.annotationType = AnnotationType.valueOf(annotationType);
		}
	}

	/**
	 * Gets the annotation category of the search pattern criteria.
	 * 
	 * @return the annotation category of the search pattern criteria
	 */
	@Nullable
	public String getAnnotationCategory() {
		return annotationCategory;
	}

	/**
	 * Sets the annotation category of the search pattern criteria.
	 * 
	 * @param annotationCategory the annotation category of the search pattern criteria
	 */
	public void setAnnotationCategory(@Nullable final String annotationCategory) {
		this.annotationCategory = annotationCategory;
	}
}
