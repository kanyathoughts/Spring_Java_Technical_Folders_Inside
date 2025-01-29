/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.EditModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

/**
 * {@code field_info} entity class.
 */
@MiningDataType(name = MiningEnitityNames.FIELD_INFO)
public class FieldInfoPojo {

	private final UUID id;

	private final EntityId module;

	@MiningDataPoint(displayName = "Field/Column Name", description = "Name of the data field or table column")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_TEXT)
	})
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Data Schema")
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1")
	})
	private final String name;

	@MiningDataPoint(displayName = "Ordinal", description = "Index of the data field or table column")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_NUMBER)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "0")
	})
	private final int ordinal;

	@MiningDataPoint(displayName = "Reference", description = "Fields or columns referenced by this column")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_TEXT)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "4")
	})
	private final Optional<String> reference;

	@MiningDataPoint(displayName = "Comment", description = "Comment describing the field or column")
	@Usage(value = Usages.EDIT_MODE, attributes = {
			@UsageAttribute(key = EditModeAttributes.EDIT_AS, value = EditModeAttributes.EDIT_AS_TEXT_AREA),
			@UsageAttribute(key = EditModeAttributes.EDIT_ENDPOINT, value = "/api/v1/projects/${$projectId}/modules/${$moduleId}/fields/${ordinal}"),
			@UsageAttribute(key = EditModeAttributes.EDIT_ENDPOINT_FIELD_NAME, value = "comment"),
			@UsageAttribute(key = EditModeAttributes.TOGETHER_WITH, value = "ordinal")
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_TEXT)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "7")
	})
	private final Optional<String> comment;

	@MiningDataPoint(scalarType = ScalarType.JSON)
	private final Optional<Map<String, Object>> properties;

	public FieldInfoPojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("ordinal") final int ordinal,
			@JsonProperty("name") final String name,
			@JsonProperty("scope") @Nullable final String reference,
			@JsonProperty("usage") @Nullable final String comment,
			@JsonProperty("modifiers") @Nullable final Map<String, Object> properties) {

		this.id = id;
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.name = name;
		this.ordinal = ordinal;

		this.reference = Optional.ofNullable(reference);
		this.comment = Optional.ofNullable(comment);
		this.properties = Optional.ofNullable(properties);
	}

	public UUID getId() {
		return id;
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

	public String getName() {
		return name;
	}

	public int getOrdinal() {
		return ordinal;
	}

	public Optional<String> getReference() {
		return reference;
	}

	public Optional<String> getComment() {
		return comment;
	}

	public Optional<Map<String, Object>> getProperties() {
		return properties;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("module", module)
				.append("name", name)
				.append("ordinal", ordinal)
				.append("reference", reference.orElse(null))
				.append("comment", comment.orElse(null))
				.append("properties", properties.orElse(null))
				.toString();
	}
}