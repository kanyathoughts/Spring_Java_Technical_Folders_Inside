/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Optional;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.ndt.core.parsing.ast.model.statement.Format;

/**
 * Format of data fields that can be the target of a Data Dictionary entry.
 */
public class DataFieldFormat extends Format {

	private final String fieldName;
	private final EntityId moduleId;
	private final ModuleLocation location;
	private final String languageType;
	private final int byteLength;
	private final Optional<Boolean> group;
	@Nullable
	private final Long dataDictionaryEntryId;
	@Nullable
	private final String usage;
	@Nullable
	private final DefinedLocation definedLocation;
	
	/**
	 * Constructs a new {@link DataFieldFormat} with the given {@code languageType} and {@code byteLength}.
	 * <p>
	 * The class will determine automatically whether the language type represents a group.
	 * 
	 * @param languageType the language-specific type of the field
	 * @param byteLength byte length of the field
	 */
	public DataFieldFormat(final String languageType, final int byteLength) {
		this.languageType = languageType;
		this.byteLength = byteLength;
		this.fieldName = StringUtils.EMPTY;
		this.moduleId = EntityId.VOID;
		this.location = new ModuleLocation();
		this.group = Optional.empty();
		this.dataDictionaryEntryId = null;
		this.usage = "DISPLAY";
		this.definedLocation = null;
	}

	/**
	 * Constructs a new {@link DataFieldFormat} with the given {@code languageType} and {@code byteLength} and marks the field as {@code group}.
	 * 
	 * @param fieldName the name of the field
	 * @param moduleId the module ID of the field
	 * @param location the {@linkplain ModuleLocation} of the field
	 * @param languageType the language-specific type of the field
	 * @param byteLength byte length of the field
	 * @param dataDictionaryEntryId id of data-dictionary-entry the field is part of
	 * @param usage the usage of the field
	 * @param definedLocation the location of where the data dictionary entry field is defined
	 */
	@JsonCreator
	public DataFieldFormat(
			@JsonProperty("fieldName") final String fieldName,
			@JsonProperty("moduleId") final EntityId moduleId,
			@JsonProperty("location") final ModuleLocation location,
			@JsonProperty("languageType") final String languageType,
			@JsonProperty("byteLength") final int byteLength,
			@Nullable @JsonProperty("dataDictionaryEntryId") final Long dataDictionaryEntryId,
			@Nullable @JsonProperty("usage") final String usage,
			@Nullable @JsonProperty("definedLocation") final DefinedLocation definedLocation) {
		this.fieldName = fieldName;
		this.moduleId = moduleId;
		this.location = location;
		this.languageType = languageType;
		this.byteLength = byteLength;
		this.group = Optional.empty();
		this.dataDictionaryEntryId = dataDictionaryEntryId;
		this.usage = usage;
		this.definedLocation = definedLocation;
	}
	
	/**
	 * Returns the field name.
	 *
	 * @return the field name
	 */
	public String getFieldName() {
		return fieldName;
	}

	
	/**
	 * Returns the module ID of the field.
	 *
	 * @return the module ID
	 */
	public EntityId getModuleId() {
		return moduleId;
	}

	
	/**
	 * Returns the {@linkplain ModuleLocation} of the field.
	 *
	 * @return the {@linkplain ModuleLocation}
	 */
	public ModuleLocation getLocation() {
		return location;
	}
	
	@Nullable
	public Long getDataDictionaryEntryId() {
		return dataDictionaryEntryId;
	}
	
	/**
	 * Returns the field usage.
	 *
	 * @return the field usage
	 */
	@Nullable
	public String getUsage() {
		return usage;
	}
	
	/**
	 * Returns the location where the data dictionary entry field is defined
	 *
	 * @return the location
	 */
	public @Nullable DefinedLocation getDefinedLocation() {
		return definedLocation;
	}

	@Override
	public String getLanguageType() {
		return languageType;
	}

	@Override
	public int getByteLength() {
		return byteLength;
	}

	@Override
	public boolean isGroup() {
		if (group.isPresent()) {
			return group.get().booleanValue();
		} else {
			return super.isGroup();
		}
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.appendSuper(super.toString())
				.append("moduleId", moduleId)
				.append("fieldName", fieldName)
				.append("languageType", languageType)
				.append("byteLength", byteLength)
				.append("usage", usage);
		return builder.toString();
	}
}
