/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.UserIdentifiableCreator;
import innowake.mining.shared.model.UserIdentifiableUpdater;
import innowake.mining.shared.model.WorkingState;

/**
 * Annotation entity class.
 */
@MiningDataType(name = MiningEnitityNames.ANNOTATION)
public class AnnotationPojo extends MiningPojo implements UserIdentifiableCreator, UserIdentifiableUpdater {
	
	private final EntityId project;
	@MiningDataPoint(displayName = "Annotation Description", description = "The Description of the Annotation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Description"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "4"),
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_TEXT),
	})
	@Usage(Usages.SORT_BY)
	private final String name;
	@MiningDataPoint(displayName = "State", description = "State of the Annotation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "STATE" /* AnnotationFieldName.STATE */),
			@UsageAttribute(key = SearchFilterAttributes.RSQL_FRAGMENT, value = "stateLink.name=in=($'{$query})")
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT),
	})
	@Usage(Usages.SORT_BY)
	private final WorkingState state;
	@MiningDataPoint(displayName = "Annotation Type", description = "Type of the Annotation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "TYPE" /* AnnotationFieldName.TYPE */),
			@UsageAttribute(key = SearchFilterAttributes.RSQL_FRAGMENT, value = "typeLink.name=in=($'{$query})")
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(Usages.SORT_BY)
	private final AnnotationType type;
	private final Optional<Long> categoryId;
	@MiningDataPoint(displayName = "Category", description = "Category of the Annotation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "CATEGORY" /* AnnotationFieldName.CATEGORY */),
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_SHOW_NONE_OPTION, value = "true")
	})
	@Usage(Usages.SORT_BY)
	private final Optional<String> categoryName;
	private final String createdBy;
	private final Optional<String> updatedBy;
	private final EntityId module;
	private final String moduleName;
	private final Optional<String> modulePath;
	private final Optional<ModuleLocation> location;
	private final Optional<UUID> source;
	private final Optional<BinaryString> sourceAttachment;
	@MiningDataPoint(displayName = "English Translation", description = "Annotation english translation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Description"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "8")
	})
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.DISPLAY_AS, value = ViewModeAttributes.DISPLAY_AS_HTML)
	})
	private final Optional<String> englishTranslation;
	@MiningDataPoint(displayName = "Identification Reason", description = "Reason for Identification as Business Rule Candidate")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
				value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "METADATA" /* AnnotationFieldName.METADATA */),
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
	private final Collection<String> reasons;
	private final List<UUID> ddEntries;
	
	private Optional<String> createdByName;
	private Optional<String> updatedByName;
	
	@JsonCreator
	public AnnotationPojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("nid") @JsonAlias("id") final Long nid,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("name") final String name,
			@JsonProperty("state") final WorkingState state,
			@JsonProperty("type") final AnnotationType type,
			@JsonProperty("categoryId") @Nullable final Long categoryId,
			@JsonProperty("categoryName") @Nullable final String categoryName,
			@JsonProperty("createdByUserId") final String createdBy,
			@JsonProperty("updatedByUserId") @Nullable final String updatedBy,
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("moduleName") final String moduleName,
			@JsonProperty("modulePath") @Nullable final String modulePath,
			@JsonProperty("location") @Nullable final ModuleLocation location,
			@JsonProperty("source") @Nullable final UUID source,
			@JsonProperty("sourceAttachment") @Nullable final BinaryString sourceAttachment,
			@JsonProperty("englishTranslation") @Nullable final String englishTranslation,
			@JsonProperty("reasons") final Collection<String> reasons,
			@JsonProperty("dataDictionaryEntries") final List<UUID> ddEntries,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties) {
		super(EntityId.of(uid, nid), customProperties);
		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.name = name;
		this.state = state;
		this.type = type;
		this.categoryId = Optional.ofNullable(categoryId);
		this.categoryName = Optional.ofNullable(categoryName);
		this.createdBy = createdBy;
		this.updatedBy = Optional.ofNullable(updatedBy);
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.modulePath = Optional.ofNullable(modulePath);
		this.moduleName = moduleName;
		this.location = Optional.ofNullable(location);
		this.source = Optional.ofNullable(source);
		this.sourceAttachment = Optional.ofNullable(sourceAttachment);
		this.englishTranslation = Optional.ofNullable(englishTranslation);
		this.reasons = reasons;
		this.ddEntries = ddEntries;
		createdByName = Optional.empty();
		updatedByName = Optional.empty();
	}

	@JsonIgnore
	public EntityId getProject() {
		return project;
	}
	
	@JsonProperty("project")
	public UUID getProjectUid() {
		return project.getUid();
	}
	
	@JsonProperty("projectId")
	public Long getProjectNid() {
		return project.getNid();
	}
	
	public String getName() {
		return name;
	}
	
	public WorkingState getState() {
		return state;
	}
	
	public AnnotationType getType() {
		return type;
	}
	
	public Optional<Long> getCategoryId() {
		return categoryId;
	}
	
	public Optional<String> getCategoryName() {
		return categoryName;
	}
	
	@JsonIgnore
	public EntityId getModule() {
		return module;
	}
	
	@JsonProperty("module")
	public UUID getModuleUid() {
		return module.getUid();
	}
	
	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return module.getNid();
	}
	
	public String getModuleName() {
		return moduleName;
	}
	
	public Optional<String> getModulePath() {
		return modulePath;
	}
	
	public Optional<ModuleLocation> getLocation() {
		return location;
	}
	
	public Optional<UUID> getSource() {
		return source;
	}
	
	public Optional<BinaryString> getSourceAttachment() {
		return sourceAttachment;
	}
	
	public Optional<String> getEnglishTranslation() {
		return englishTranslation;
	}
	
	public Collection<String> getReasons() {
		return reasons;
	}
	
	public List<UUID> getDataDictionaryEntries() {
		return ddEntries;
	}
	
	public Optional<Integer> getOffset() {
		return location.map(ModuleLocation::getOffset);
	}
	
	public Optional<Integer> getLength() {
		return location.map(ModuleLocation::getLength);
	}
	
	@Override
	public String getCreatedByUserId() {
		return createdBy;
	}
	
	@Override
	public Optional<String> getCreatedByUserName() {
		return createdByName;
	}
	
	@Override
	public Optional<String> getUpdatedByUserId() {
		return updatedBy;
	}
	
	@Override
	public Optional<String> getUpdatedByUserName() {
		return updatedByName;
	}

	@Override
	public void setUpdatedByUserName(@Nullable String userName) {
		this.updatedByName = Optional.ofNullable(userName);
	}

	@Override
	public void setCreatedByUserName(@Nullable String userName) {
		this.createdByName = Optional.ofNullable(userName);
	}

	/**
	 * @return prototype equivalent of this pojo
	 */
	@SuppressWarnings("null")
	public AnnotationPojoPrototype convertToPrototype() {
		final AnnotationPojoPrototype prototype = new AnnotationPojoPrototype()
				.withId(identity())
				.setName(name)
				.setState(state)
				.setType(type)
				.setCreatedByUserId(createdBy)
				.setCustomProperties(getCustomProperties());

		prototype.setModule(module);

		/* reasons can be null in metadata import */
		if (reasons != null) {
			prototype.setReasons(reasons);
		}
		
		location.ifPresent(prototype::setLocation);
		categoryId.ifPresent(prototype::setCategoryId);
		updatedBy.ifPresent(prototype::setUpdatedByUserId);
		sourceAttachment.ifPresent(prototype::setSourceAttachment);
		englishTranslation.ifPresent(prototype::setEnglishTranslation);

		return prototype;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
			.appendSuper(super.toString())
			.append("project", project)
			.append("name", name)
			.append("state", state)
			.append("type", type)
			.append("categoryId", categoryId)
			.append("categoryName", categoryName)
			.append("createdBy", createdBy)
			.append("updatedBy", updatedBy)
			.append("module", module)
			.append("moduleName", moduleName)
			.append("modulePath", modulePath)
			.append("location", location)
			.append("source", source)
			.append("sourceAttachment", sourceAttachment)
			.append("englishTranslation", englishTranslation)
			.append("reasons", reasons)
			.append("ddEntries", ddEntries)
			.toString();
	}
}
