/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Objects;

import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import org.apache.commons.lang.builder.ToStringBuilder;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SortByAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

/**
 * Model class for an annotation metadata entity.
 */
public class AnnotationMetadata extends Entity {

	@Nullable
	private Long projectId;
	
	@MiningDataPoint(displayName = "Identification Reason", description = "Reason for Identification as Business Rule Candidate")
	@Usage(value = Usages.GRAPHQL_QUERY_ANNOTATIONS, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.SQL_FRAGMENT_EQ, value = "metaData.reason.name CONTAINS ?"),
			@UsageAttribute(key = SearchFilterAttributes.SQL_FRAGMENT_NONE, value = "metaData.reason.size() = 0"),
			@UsageAttribute(key = SortByAttributes.SQL_FRAGMENT_ORDER_BY, value = "metaData.reason.name")
	})
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
				value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "METADATA" /* AnnotationFieldName.METADATA */),
			@UsageAttribute(key = SearchFilterAttributes.RSQL_FRAGMENT, value = "metaData.reason.name=containsAny=($'{$query})")
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_SHOW_NONE_OPTION, value = "true")
	})
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.LABEL_MAPPING,
					value = "ANNOTATION_METADATA_REASON_LABELS" /* LabelMappings.LabelType.ANNOTATION_METADATA_REASON_LABELS */)
	})
	@Usage(Usages.SORT_BY)
	@Nullable
	private AnnotationMetaDataReasonEnum reason;
	
	@Nullable
	private Long annotationId;
	
	/**
	 * Creates a new instance.
	 */
	public AnnotationMetadata() {}
	
	/**
	 * Constructor
	 * 
	 * @param recordId @rid of AnnotationMetadata
	 * @param reason the reason why annotation is considered a BR candidate
	 * @param projectId ID of Project
	 * @param annotationId ID of Annotation
	 */
	public AnnotationMetadata(final String recordId, final AnnotationMetaDataReasonEnum reason, final Long projectId, final Long annotationId) {
		this.recordId = recordId;
		this.reason = reason;
		this.projectId = projectId;
		this.annotationId = annotationId;
	}
	
	/**
	 * Constructor
	 * 
	 * @param reason the reason why annotation is considered a BR candidate
	 */
	public AnnotationMetadata(final AnnotationMetaDataReasonEnum reason) {
		this.reason = reason;
	}
	
	/**
	 * Gets the id of the linked Project.
	 *
	 * @return the id of the linked Project
	 */
	public Long getProjectId() {
		return assertNotNull(projectId, "Project ID must not be null.");
	}

	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the id of the linked Project
	 */
	public void setProjectId(final long projectId) {
		this.projectId = Long.valueOf(projectId);
	}
	
	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the id of the linked Project
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = projectId;
	}
	
	/**
	 * Gets the reason of Candidate Annotation
	 *
	 * @return reason of Candidate Annotation
	 */
	public AnnotationMetaDataReasonEnum getReason() {
		return assertNotNull(reason, "The Reason must not be null");
	}

	/**
	 * Sets the reason of Candidate Annotation
	 *
	 * @param reason Reason to set
	 */
	public void setReason(final AnnotationMetaDataReasonEnum reason) {
		this.reason = reason;
	}

	/**
	 * Sets the id of the linked Annotation.
	 *
	 * @return Annotation id
	 */
	public Long getAnnotationId() {
		return assertNotNull(annotationId, "The Annotation ID must not be null");
	}

	/**
	 * Sets the id of the linked Annotation.
	 *
	 * @param annotationId Annotation id
	 */
	public void setAnnotationId(final Long annotationId) {
		this.annotationId = annotationId;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final AnnotationMetadata other = (AnnotationMetadata) obj;
		return Objects.equals(reason, other.reason)
				&& Objects.equals(annotationId, other.annotationId)
				&& Objects.equals(projectId, other.projectId);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(reason, annotationId, projectId);
		return result;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.appendSuper(super.toString());
		builder.append("projectId", projectId);
		builder.append("reason", reason);
		builder.append("annotationId", annotationId);
		return builder.toString();
	}

}
