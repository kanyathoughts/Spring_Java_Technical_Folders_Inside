/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;

/*
	DataDictionary
	+	EntityId							module				module,moduleId
	+	ModuleLocation						location
	+	String								name				dataElementName
	+	String								description
	+	String								format
	+	Map<String, Map<String, String>>	scopes
	+	Long								length
	+	String								createdBy
	+	String								updatedBy
	+	String								picClause
	+	String								definedLocation
	+	WorkingState						state
	+	Boolean								isBusiness
	+	String								fieldTransformation
	+	String								sourceInput
	+	String								targetOutput
	+	Boolean								isReferenced
	+	String								usage
	+	Boolean								isCandidate
	+	Long								fieldLevel
	+	String								parentGroup
	+	String								groupPath
	+	Long								indentation
	+	String								translatedFieldValue
*/

/**
 * DataDictionary entity request class.
 */
public class DataDictionaryPojoPrototype extends MiningSequentialPojoPrototype<DataDictionaryPojoPrototype> {
	
	public final Definable<EntityId> module = new Definable<>(false, "DataDictionary.module");
	public final Definable<ModuleLocation> location = new Definable<>(true, "DataDictionary.location");
	@JsonProperty("dataElementName")
	public final Definable<String> name = new Definable<>(false, "DataDictionary.name");
	public final Definable<String> description = new Definable<>(false, "DataDictionary.description");
	public final Definable<String> format = new Definable<>(true, "DataDictionary.format");
	public final Definable<Map<DataDictionaryVariableScope, Map<String, String>>> scopes = new Definable<>(false, "DataDictionary.scopes");
	public final Definable<Long> length = new Definable<>(true, "DataDictionary.length");
	public final Definable<String> createdByUserId = new Definable<>(false, "DataDictionary.createdByUserId");
	public final Definable<String> updatedByUserId = new Definable<>(false, "DataDictionary.updatedByUserId");
	public final Definable<String> picClause = new Definable<>(true, "DataDictionary.picClause");
	public final Definable<DefinedLocation> definedLocation = new Definable<>(true, "DataDictionary.definedLocation");
	public final Definable<WorkingState> state = new Definable<>(true, "DataDictionary.state");
	public final Definable<Boolean> isBusiness = new Definable<>(true, "DataDictionary.isBusiness");
	public final Definable<String> fieldTransformation = new Definable<>(true, "DataDictionary.fieldTransformation");
	public final Definable<String> sourceInput = new Definable<>(true, "DataDictionary.sourceInput");
	public final Definable<String> targetOutput = new Definable<>(true, "DataDictionary.targetOutput");
	public final Definable<Boolean> isReferenced = new Definable<>(true, "DataDictionary.isReferenced");
	public final Definable<String> usage = new Definable<>(true, "DataDictionary.usage");
	public final Definable<Boolean> isCandidate = new Definable<>(false, "DataDictionary.isCandidate");
	public final Definable<Long> fieldLevel = new Definable<>(true, "DataDictionary.fieldLevel");
	public final Definable<String> parentGroup = new Definable<>(true, "DataDictionary.parentGroup");
	public final Definable<String> groupPath = new Definable<>(true, "DataDictionary.groupPath");
	public final Definable<Long> indentation = new Definable<>(true, "DataDictionary.indentation");
	public final Definable<String> initialValue = new Definable<>(true, "DataDictionary.initialValue");
	public final Definable<String> translatedFieldValue = new Definable<>(true, "DataDictionary.translatedFieldValue");
	public DataDictionaryPojoPrototype() {
		super("DataDictionary");
	}
	
	@JsonAlias("moduleId")
	public DataDictionaryPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}
	
	public DataDictionaryPojoPrototype setLocation(final @Nullable ModuleLocation location) {
		this.location.set(location);
		return this;
	}
	
	@JsonAlias("dataElementName")
	public DataDictionaryPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
	
	public DataDictionaryPojoPrototype setDescription(final String description) {
		this.description.set(description);
		return this;
	}
	
	public DataDictionaryPojoPrototype setFormat(final @Nullable String format) {
		this.format.set(format);
		return this;
	}
	
	public DataDictionaryPojoPrototype setScopes(final Map<DataDictionaryVariableScope, Map<String, String>> scopes) {
		this.scopes.set(scopes);
		return this;
	}
	
	public DataDictionaryPojoPrototype setLength(final @Nullable Long length) {
		this.length.set(length);
		return this;
	}
	
	public DataDictionaryPojoPrototype setCreatedByUserId(final String createdBy) {
		this.createdByUserId.set(createdBy);
		return this;
	}
	
	@SuppressWarnings("null")
	public DataDictionaryPojoPrototype setUpdatedByUserId(final String updatedBy) {
		/* for backwards compatibility ignore null value send by UI */
		if (updatedBy != null) {
			this.updatedByUserId.set(updatedBy);
		}
		return this;
	}
	
	public DataDictionaryPojoPrototype setPicClause(@Nullable final String picClause) {
		this.picClause.set(picClause);
		return this;
	}
	
	public DataDictionaryPojoPrototype setDefinedLocation(@Nullable final DefinedLocation definedLocation) {
		this.definedLocation.set(definedLocation);
		return this;
	}
	
	public DataDictionaryPojoPrototype setState(@Nullable final WorkingState state) {
		this.state.set(state);
		return this;
	}
	
	public DataDictionaryPojoPrototype setIsBusiness(@Nullable final Boolean isBusiness) {
		this.isBusiness.set(isBusiness);
		return this;
	}
	
	public DataDictionaryPojoPrototype setFieldTransformation(final @Nullable String fieldTransformation) {
		this.fieldTransformation.set(fieldTransformation);
		return this;
	}
	
	public DataDictionaryPojoPrototype setSourceInput(final @Nullable String sourceInput) {
		this.sourceInput.set(sourceInput);
		return this;
	}
	
	public DataDictionaryPojoPrototype setTargetOutput(final @Nullable String targetOutput) {
		this.targetOutput.set(targetOutput);
		return this;
	}
	
	public DataDictionaryPojoPrototype setIsReferenced(@Nullable final Boolean isReferenced) {
		this.isReferenced.set(isReferenced);
		return this;
	}
	
	public DataDictionaryPojoPrototype setUsage(@Nullable final String usage) {
		this.usage.set(usage);
		return this;
	}
	
	public DataDictionaryPojoPrototype setIsCandidate(final Boolean isCandidate) {
		this.isCandidate.set(isCandidate);
		return this;
	}
	
	public DataDictionaryPojoPrototype setFieldLevel(@Nullable final Long fieldLevel) {
		this.fieldLevel.set(fieldLevel);
		return this;
	}
	
	public DataDictionaryPojoPrototype setParentGroup(@Nullable final String parentGroup) {
		this.parentGroup.set(parentGroup);
		return this;
	}
	
	public DataDictionaryPojoPrototype setGroupPath(@Nullable final String groupPath) {
		this.groupPath.set(groupPath);
		return this;
	}
	
	public DataDictionaryPojoPrototype setIndentation(@Nullable final Long indentation) {
		this.indentation.set(indentation);
		return this;
	}
	
	public DataDictionaryPojoPrototype setInitialValue(final String initialValue) {
		this.initialValue.set(initialValue);
		return this;
	}
	
	public DataDictionaryPojoPrototype setTranslatedFieldValue(final String translatedFieldValue) {
		this.translatedFieldValue.set(translatedFieldValue);
		return this;
	}
	
}
