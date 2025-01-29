/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;

/**
 * Model class for an annotation category entity.
 */
public class AnnotationCategory extends IdentifiableAndNameableEntity {
	
	public enum RuleAnnotationCategory {
		
		BUSINESS_RULE("Business Rule"),
		TECHNICAL_RULE("Technical Rule"),
		VALIDATION_RULE("Validation Rule"),
		FIELD_COMPUTATION_RULE("Field Computation Rule"),
		ERROR_PROCESSING_RULE("Error Processing Rule"),
		UNKNOWN;
		
		private final String name;
		
		private static final ConcurrentHashMap<String, RuleAnnotationCategory> NAME_TO_CATEGORY_MAP;
		
		static {
			NAME_TO_CATEGORY_MAP = new ConcurrentHashMap<>();
			for (final RuleAnnotationCategory value : values()) {
				NAME_TO_CATEGORY_MAP.put(value.name, value);
			}
		}
		
		private RuleAnnotationCategory() {
			this.name = StringUtils.EMPTY;
		}

		private RuleAnnotationCategory(final String name) {
			this.name = name;
		}
		
		/**
		 * Returns the name associated with the {@link RuleAnnotationCategory} constant on which this method is called.
		 * 
		 * @return name
		 */
		public String getName() {
			return name;
		}
		
		/**
		 * Returns the category for given a name.
		 *
		 * @param name the name the category is associated with.
		 * @return the category mapped with the name or {@link #UNKNOWN} if no match is found
		 */
		public static RuleAnnotationCategory fromName(final String name) {
			final RuleAnnotationCategory category = NAME_TO_CATEGORY_MAP.get(name);
			return category != null ? category : UNKNOWN;
		}
	}
	
	public enum DatabaseAnnotationCategory {
		
		CLOSE("Close"),
		DECLARE("Declare"),
		READ("Read"),
		WRITE("Write"),
		UNKNOWN;
			
		private final String name;
		
		private static final ConcurrentHashMap<String, DatabaseAnnotationCategory> NAME_TO_CATEGORY_MAP;
		
		static {
			NAME_TO_CATEGORY_MAP = new ConcurrentHashMap<>();
			for (final DatabaseAnnotationCategory value : values()) {
				NAME_TO_CATEGORY_MAP.put(value.name, value);
			}
		}
		
		private DatabaseAnnotationCategory() {
			this.name = StringUtils.EMPTY;
		}

		private DatabaseAnnotationCategory(final String name) {
			this.name = name;
		}
		
		/**
		 * Returns the name associated with the {@link RuleAnnotationCategory} constant on which this method is called.
		 * 
		 * @return name
		 */
		public String getName() {
			return name;
		}
		
		/**
		 * Returns the category for given a name.
		 *
		 * @param name the name the category is associated with.
		 * @return the category mapped with the name or {@link #UNKNOWN} if no match is found
		 */
		public static DatabaseAnnotationCategory fromName(final String name) {
			final DatabaseAnnotationCategory category = NAME_TO_CATEGORY_MAP.get(name);
			return category != null ? category : UNKNOWN;
		}
	}
	@Nullable
	private EntityId projectId;

	@Nullable
	private List<AnnotationType> types;

	/**
	 * Gets the id of the linked Project.
	 *
	 * @return the id of the linked Project
	 */
	public EntityId getProjectId() {
		return projectId != null ? projectId : EntityId.VOID;
	}

	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the id of the linked Project
	 */
	@JsonProperty
	public void setProjectId(final EntityId projectId) {
		this.projectId = projectId;
	}

	/**
	 * Sets the id of the linked Project.
	 *
	 * @param projectId the id of the linked Project
	 */
	public void setProjectId(final Long projectId) {
		this.projectId = EntityId.of(projectId);
	}

	/**
	 * Gets the linked {@link AnnotationType}s.
	 *
	 * @return the linked {@link AnnotationType}s
	 */
	@Nullable
	public List<AnnotationType> getTypes() {
		return types;
	}

	/**
	 * Sets the linked {@link AnnotationType}s.
	 *
	 * @param types the linked {@link AnnotationType}s
	 */
	public void setTypes(@Nullable final List<AnnotationType> types) {
		this.types = types;
	}

	@Override
	public String toString() {
		final List<AnnotationType> types = this.types;
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("rid", recordId)
				.append("id", id)
				.append("name", name)
				.append("projectId", projectId)
				.append("type", types == null ? "-" : types.toString())
				.toString();
	}

}
