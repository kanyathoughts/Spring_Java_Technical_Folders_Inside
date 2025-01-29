/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.api.lang.Nullable;

/**
 * Model class for Annotation Reporting
 */
public class AnnotationReport extends IdentifiableAndNameableEntity implements UserIdentifyable {

	@Nullable
	private Long moduleId;

	@Nullable
	private String moduleName;

	@Nullable
	private AnnotationType annotationType;

	@Nullable
	private Long categoryId;

	@Nullable
	private String categoryName;

	@Nullable
	private WorkingState annotationState;

	@Nullable
	private String sourceCode;

	@Nullable
	private String taxonomy;

	@Nullable
	private String updatedByUserId;

	@Nullable
	private String createdByUserId;

	@Nullable
	private String createdByUserName;

	@Nullable
	private String updatedByUserName;

	/**
	 * Gets the module id.
	 *
	 * @return module id of the module associated with the module
	 */
	public Long getModuleId() {
		return assertNotNull(moduleId, "Module id must not be null.");
	}


	/**
	 * sets the module id.
	 *
	 * @param moduleId the module id
	 */
	public void setModuleId(final Long moduleId) {
		this.moduleId = moduleId;
	}

	/**
	 * Gets the module name.
	 *
	 * @return  the module name associated with the annotation
	 */
	public String getModuleName() {
		return assertNotNull(moduleName, "Module name must not be null.");
	}

	/**
	 * Sets the module name.
	 *
	 * @param moduleName the module name
	 */
	public void setModuleName(final String moduleName) {
		this.moduleName = moduleName;
	}

	/**
	 * Gets the taxonomy name.
	 *
	 * @return the taxonomy name
	 */
	@Nullable
	public String getTaxonomy() {
		return taxonomy;
	}

	/**
	 * Sets the taxonomy name.
	 *
	 * @param taxonomy the taxonomy name
	 */
	public void setTaxonomy(final String taxonomy) {
		this.taxonomy = taxonomy;
	}

	/**
	 * Gets the category name.
	 *
	 * @return the category name
	 */
	@Nullable
	public String getCategoryName() {
		return categoryName;
	}

	/**
	 * Sets the category name.
	 *
	 * @param categoryName the category name
	 */
	public void setCategoryName(final String categoryName) {
		this.categoryName = categoryName;
	}

	/**
	 * Gets {@link AnnotationType} type.
	 *
	 * @return {@link AnnotationType} the annotation type
	 */
	public AnnotationType getAnnotationType() {
		return assertNotNull(annotationType, "Annotation type must not be null.");
	}

	/**
	 * Sets {@link AnnotationType} type.
	 *
	 * @param annotationType the {@link AnnotationType}
	 */
	public void setAnnotationType(final AnnotationType annotationType) {
		this.annotationType = annotationType;
	}

	/**
	 * Gets the CategoryId of annotation.
	 *
	 * @return CategoryId the annotation category id
	 */
	@Nullable
	public Long getCategoryId() {
		return categoryId;
	}

	/**
	 * Sets CategoryId of the annotation.
	 *
	 * @param categoryId the CategoryId of the annotation 
	 */
	public void setCategoryId(final Long categoryId) {
		this.categoryId = categoryId;
	}

	/**
	 * Gets {@link WorkingState}.
	 *
	 * @return {@link WorkingState} the annotation state 
	 */
	public WorkingState getAnnotationState() {
		return assertNotNull(annotationState, "Annotation state must not be null.");
	}

	/**
	 * Sets {@link WorkingState}.
	 *
	 * @param annotationState the {@link WorkingState}
	 */
	public void setAnnotationState(final WorkingState annotationState) {
		this.annotationState = annotationState;
	}

	/**
	 * Gets the source code of the annotation.
	 *
	 * @return the source code, or {@code null} if there is not source available
	 */
	@Nullable
	public String getSourceCode() {
		return sourceCode;
	}

	/**
	 * sets the source code.
	 *
	 * @param sourceCode the source code
	 */
	public void setSourceCode(final String sourceCode) {
		this.sourceCode = sourceCode;
	}

	@Override
	public String getCreatedByUserId() {
		return assertNotNull(createdByUserId, "Created by user reference must not be null.");
	}

	@Override
	public void setCreatedByUserId(final String userId) {
		createdByUserId = userId;
	}

	@Override
	public String getCreatedByUserName() {
		return createdByUserName != null ? createdByUserName : "";
	}

	@Override
	public void setCreatedByUserName(final String userName) {
		createdByUserName = userName;
	}

	@Override
	public String getUpdatedByUserId() {
		return updatedByUserId != null ? updatedByUserId : "";
	}

	@Override
	public void setUpdatedByUserId(final String userId) {
		updatedByUserId = userId;
	}

	@Override
	public String getUpdatedByUserName() {
		return updatedByUserName != null ? updatedByUserName : "";
	}

	@Override
	public void setUpdatedByUserName(final String userName) {
		updatedByUserName = userName;
	}

}
