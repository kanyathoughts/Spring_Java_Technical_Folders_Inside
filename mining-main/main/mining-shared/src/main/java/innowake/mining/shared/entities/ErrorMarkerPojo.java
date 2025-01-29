/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.model.AstNodeLocation;
import org.apache.commons.lang.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * {@code error_marker} entity class. Formerly known as {@code ExcelSheetErrors}.
 */
@MiningDataType(name = MiningEnitityNames.ERROR_MARKER)
public final class ErrorMarkerPojo {

	private final EntityId project;
	@Nullable private final EntityId module;

	@MiningDataPoint(displayName = "Severity", description = "The severity of this error marker")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1")
	})
	private final Severity severity;
	private final ErrorKey key;

	@MiningDataPoint(displayName = "Cause", description = "The message of this error marker")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2")
	})
	private final String cause;
	@Nullable private final AstNodeLocation location;
	@Nullable private final Integer line;

	@JsonCreator
	public ErrorMarkerPojo(
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("severity") @Nullable final Severity severity,
			@JsonProperty("key") @Nullable final ErrorKey key,
			@JsonProperty("cause") @Nullable final String cause,
			@JsonProperty("location") @Nullable final AstNodeLocation location,
			@JsonProperty("line") @Nullable final Integer line) {
		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.module = module != null ? module : EntityId.orNull(moduleUid, moduleNid);
		this.severity = severity == null ? Severity.ERROR : severity;
		this.key = key == null ? ErrorKey.MODULE_ABORT : key;
		this.cause = cause == null ? "" : cause;
		this.location = location;
		this.line = line;
	}

	/**
	 * @return the {@link EntityId} of the project this error marker belongs to
	 */
	@JsonIgnore
	public EntityId getProject() {
		return project;
	}

	/**
	 * @return the {@link UUID} of the project this error marker belongs to
	 */
	@JsonProperty("project")
	public UUID getProjectUid() {
		return project.getUid();
	}

	/**
	 * @return the numeric id of the project this error marker belongs to
	 */
	@JsonProperty("projectId")
	public Long getProjectNid() {
		return project.getNid();
	}

	/**
	 * @return the {@link EntityId} of the module this error marker belongs to or {@code null} if this error is an general error reported for a project.
	 */
	@Nullable
	@JsonIgnore
	public EntityId getModule() {
		return module;
	}

	/**
	 * @return the {@link UUID} of the module this error marker belongs to or {@code null} if this error is an general error reported for a project.
	 */
	@Nullable
	@JsonProperty("module")
	public UUID getModuleUid() {
		return module != null ? module.getUid() : null;
	}

	/**
	 * @return the numeric id of the module this error marker belongs to or {@code null} if this error is an general error reported for a project.
	 */
	@Nullable
	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return module != null ? module.getNid() : null;
	}

	/**
	 * @return the severity of this error marker
	 */
	public Severity getSeverity() {
		return severity;
	}

	/**
	 * @return the type of this error marker
	 */
	public ErrorKey getKey() {
		return key;
	}

	/**
	 * @return the message of this error marker
	 */
	public String getCause() {
		return cause;
	}

	/**
	 * @return the {@link AstNodeLocation} of this error marker
	 */
	public AstNodeLocation getLocation() {
		return location != null ? location : new AstNodeLocation();
	}

	/**
	 * @return the line of this error marker
	 */
	@Nullable
	public Integer getLine() {
		return line;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module)
				.append("severity", severity)
				.append("key", key)
				.append("cause", cause)
				.append("location", location)
				.append("line", line)
				.toString();
	}
}
