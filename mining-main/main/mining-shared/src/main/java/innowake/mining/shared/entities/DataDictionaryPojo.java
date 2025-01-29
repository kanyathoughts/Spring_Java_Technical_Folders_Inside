/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.UserIdentifiableCreator;
import innowake.mining.shared.model.UserIdentifiableUpdater;
import innowake.mining.shared.model.WorkingState;

/**
 * DataDictionary entity class.
 */
@MiningDataType(name = MiningEnitityNames.DATA_DICTIONARY)
public class DataDictionaryPojo extends MiningPojo implements UserIdentifiableCreator, UserIdentifiableUpdater {
	
	private final EntityId module;
	private final Optional<ModuleLocation> location;
	private final String name;
	private final String description;
	private final Optional<String> format;
	/* required to be present as Annotation due to the initialization order */
	@MiningDataPoint(scalarType = ScalarType.JSON)
	private final Map<DataDictionaryVariableScope, Map<String, String>> scopes;
	@Usage(Usages.SORT_BY)
	private final Optional<Long> length;
	private final String createdBy;
	private final Optional<String> updatedBy;
	private final Optional<String> picClause;
	private final Optional<DefinedLocation> definedLocation;
	private final Optional<WorkingState> state;
	private final Optional<Boolean> isBusiness;
	private final Optional<String> fieldTransformation;
	private final Optional<String> sourceInput;
	private final Optional<String> targetOutput;
	private final Optional<Boolean> isReferenced;
	private final Optional<String> usage;
	private final Boolean isCandidate;
	private final Optional<Long> fieldLevel;
	private final Optional<String> parentGroup;
	private final Optional<String> groupPath;
	private final Optional<Long> indentation;
	private final Optional<String> initialValue;
	private final Optional<String> translatedFieldValue;
	private final List<EntityId> annotations;
	
	private Optional<String> createdByName;
	private Optional<String> updatedByName;
	
	@JsonCreator
	public DataDictionaryPojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("nid") @JsonAlias("id") final Long nid,
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("location") final @Nullable ModuleLocation location,
			@JsonProperty("name") @JsonAlias("dataElementName") final String name,
			@JsonProperty("description") final String description,
			@JsonProperty("format") final @Nullable String format,
			@JsonProperty("scopes") final Map<DataDictionaryVariableScope, Map<String, String>> scopes,
			@JsonProperty("length") final @Nullable Long length,
			@JsonProperty("createdByUserId") final String createdBy,
			@JsonProperty("updatedByUserId") final @Nullable String updatedBy,
			@JsonProperty("picClause") final @Nullable String picClause,
			@JsonProperty("definedLocation") final @Nullable DefinedLocation definedLocation,
			@JsonProperty("state") final @Nullable WorkingState state,
			@JsonProperty("isBusiness") final @Nullable Boolean isBusiness,
			@JsonProperty("fieldTransformation") @Nullable final String fieldTransformation,
			@JsonProperty("sourceInput") final @Nullable String sourceInput,
			@JsonProperty("targetOutput") final @Nullable String targetOutput,
			@JsonProperty("isReferenced") final @Nullable Boolean isReferenced,
			@JsonProperty("usage") final @Nullable String usage,
			@JsonProperty("isCandidate") final Boolean isCandidate,
			@JsonProperty("fieldLevel") final @Nullable Long fieldLevel,
			@JsonProperty("parentGroup") final @Nullable String parentGroup,
			@JsonProperty("groupPath") final @Nullable String groupPath,
			@JsonProperty("indentation") final @Nullable Long indentation,
			@JsonProperty("initialValue") final @Nullable String initialValue,
			@JsonProperty("translatedFieldValue") final @Nullable String translatedFieldValue,
			@JsonProperty("annotations") final List<EntityId> annotations,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties) {
		super(EntityId.of(uid, nid), customProperties);
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.location = Optional.ofNullable(location);
		this.name = name;
		this.description = description;
		this.format = Optional.ofNullable(format);
		this.scopes = scopes;
		this.length = Optional.ofNullable(length);
		this.createdBy = createdBy;
		this.updatedBy = Optional.ofNullable(updatedBy);
		this.picClause = Optional.ofNullable(picClause);
		this.definedLocation = Optional.ofNullable(definedLocation);
		this.state = Optional.ofNullable(state);
		this.isBusiness = Optional.ofNullable(isBusiness);
		this.fieldTransformation = Optional.ofNullable(fieldTransformation);
		this.sourceInput = Optional.ofNullable(sourceInput);
		this.targetOutput = Optional.ofNullable(targetOutput);
		this.isReferenced = Optional.ofNullable(isReferenced);
		this.usage = Optional.ofNullable(usage);
		this.isCandidate = isCandidate;
		this.fieldLevel = Optional.ofNullable(fieldLevel);
		this.parentGroup = Optional.ofNullable(parentGroup);
		this.groupPath = Optional.ofNullable(groupPath);
		this.indentation = Optional.ofNullable(indentation);
		this.initialValue = Optional.ofNullable(initialValue);
		this.translatedFieldValue = Optional.ofNullable(translatedFieldValue);
		this.annotations = annotations;
		createdByName = Optional.empty();
		updatedByName = Optional.empty();
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
	
	public Optional<ModuleLocation> getLocation() {
		return location;
	}
	
	@JsonProperty("dataElementName")
	public String getName() {
		return name;
	}
	
	public String getDescription() {
		return description;
	}
	
	public Optional<String> getFormat() {
		return format;
	}
	
	public Map<DataDictionaryVariableScope, Map<String, String>> getScopes() {
		return scopes;
	}
	
	public Optional<Long> getLength() {
		return length;
	}
	
	public Optional<String> getPicClause() {
		return picClause;
	}
	
	public Optional<DefinedLocation> getDefinedLocation() {
		return definedLocation;
	}
	
	public Optional<WorkingState> getState() {
		return state;
	}
	
	public Optional<Boolean> getIsBusiness() {
		return isBusiness;
	}
	
	public Optional<String> getFieldTransformation() {
		return fieldTransformation;
	}
	
	public Optional<String> getSourceInput() {
		return sourceInput;
	}
	
	public Optional<String> getTargetOutput() {
		return targetOutput;
	}
	
	public Optional<Boolean> getIsReferenced() {
		return isReferenced;
	}
	
	public Optional<String> getUsage() {
		return usage;
	}
	
	public Boolean getIsCandidate() {
		return isCandidate;
	}
	
	public Optional<Long> getFieldLevel() {
		return fieldLevel;
	}
	
	public Optional<String> getParentGroup() {
		return parentGroup;
	}
	
	public Optional<String> getGroupPath() {
		return groupPath;
	}
	
	public Optional<Long> getIndentation() {
		return indentation;
	}
	
	public Optional<String> getInitialValue() {
		return initialValue;
	}
	
	public Optional<String> getTranslatedFieldValue() {
		return translatedFieldValue;
	}

	public List<EntityId> getAnnotations() {
		return annotations;
	}
	
	@Override
	public Optional<String> getUpdatedByUserName() {
		return updatedByName;
	}

	@Override
	public void setUpdatedByUserName(@Nullable final String userName) {
		updatedByName = Optional.ofNullable(userName);
	}

	@Override
	public Optional<String> getCreatedByUserName() {
		return createdByName;
	}

	@Override
	public void setCreatedByUserName(@Nullable final String userName) {
		createdByName = Optional.ofNullable(userName);
	}

	@Override
	public Optional<String> getUpdatedByUserId() {
		return updatedBy;
	}

	@Override
	public String getCreatedByUserId() {
		return createdBy;
	}

	/**
	 * @return prototype equivalent of this pojo
	 */
	public DataDictionaryPojoPrototype convertToPrototype() {
		final DataDictionaryPojoPrototype prototype = new DataDictionaryPojoPrototype()
				.withId(identity())
				.setCreatedByUserId(createdBy)
				.setCustomProperties(getCustomProperties())
				.setDescription(description)
				.setIsCandidate(getIsCandidate())
				.setModule(getModule())
				.setName(getName())
				.setScopes(getScopes());

		location.ifPresent(prototype::setLocation);
		definedLocation.ifPresent(prototype::setDefinedLocation);
		fieldLevel.ifPresent(prototype::setFieldLevel);
		fieldTransformation.ifPresent(prototype::setFieldTransformation);
		format.ifPresent(prototype::setFormat);
		groupPath.ifPresent(prototype::setGroupPath);
		indentation.ifPresent(prototype::setIndentation);
		isBusiness.ifPresent(prototype::setIsBusiness);
		isReferenced.ifPresent(prototype::setIsReferenced);
		length.ifPresent(prototype::setLength);
		parentGroup.ifPresent(prototype::setParentGroup);
		picClause.ifPresent(prototype::setPicClause);
		usage.ifPresent(prototype::setUsage);
		updatedBy.ifPresent(prototype::setUpdatedByUserId);
		targetOutput.ifPresent(prototype::setTargetOutput);
		state.ifPresent(prototype::setState);
		translatedFieldValue.ifPresent(prototype::setTranslatedFieldValue);
		sourceInput.ifPresent(prototype::setSourceInput);

		return prototype;
	}
}
