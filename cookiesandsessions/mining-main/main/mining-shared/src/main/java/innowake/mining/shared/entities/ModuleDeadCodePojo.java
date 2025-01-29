/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * {@code module_dead_code} entity class. Formerly know as {@code ExcelSheetDeadCode}.
 */
@MiningDataType(name = "Module")
public final class ModuleDeadCodePojo {

	private final EntityId module;
	@Nullable private final Integer startingLine;
	@Nullable private final Integer numberOfLines;
	@Nullable private final String deadCode;

	@JsonCreator
	public ModuleDeadCodePojo(
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("startingLine") @Nullable final Integer startingLine,
			@JsonProperty("numberOfLines") @Nullable final Integer numberOfLines,
			@JsonProperty("deadCode") @Nullable final String deadCode) {
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.startingLine = startingLine;
		this.numberOfLines = numberOfLines;
		this.deadCode = deadCode;
	}

	/**
	 * @return the {@link EntityId} of this {@code module_dead_code} entity
	 */
	@JsonIgnore
	public EntityId getModule() {
		return module;
	}

	/**
	 * @return the {@link UUID} of this {@code module_dead_code} entity
	 */
	@JsonProperty("module")
	public UUID getModuleUid() {
		return module.getUid();
	}

	/**
	 * @return the numeric id of this {@code module_dead_code} entity
	 */
	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return module.getNid();
	}

	/**
	 * @return the starting line of this {@code module_dead_code} entity
	 */
	@Nullable
	public Integer getStartingLine() {
		return startingLine;
	}

	/**
	 * @return the number of lines of this {@code module_dead_code} entity
	 */
	@Nullable
	public Integer getNumberOfLines() {
		return numberOfLines;
	}
	
	/**
	 * @return the dead code as text of this {@code module_dead_code} entity
	 */
	@Nullable
	public String getDeadCode() {
		return deadCode;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module)
				.append("startingLine", startingLine)
				.append("numberOfLines", numberOfLines)
				.append("deadCode", deadCode)
				.toString();
	}
}
