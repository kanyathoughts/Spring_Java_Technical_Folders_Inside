/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import innowake.mining.shared.model.AstNodeLocation;
import org.apache.commons.lang.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * {@code error_marker} entity request class. Formerly know as {@code ExcelSheetErrors}.
 */
public final class ErrorMarkerPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> project = new Definable<>(false, "ErrorMarker.project");
	public final Definable<EntityId> module = new Definable<>(true, "ErrorMarker.module");
	public final Definable<Severity> severity = new Definable<>(true, "ErrorMarker.severity");
	public final Definable<ErrorKey> key = new Definable<>(true, "ErrorMarker.key");
	public final Definable<String> cause = new Definable<>(true, "ErrorMarker.cause");
	public final Definable<AstNodeLocation> location = new Definable<>(true, "ErrorMarker.location");
	public final Definable<Integer> line = new Definable<>(true, "ErrorMarker.line");

	@JsonAlias("projectId")
	public ErrorMarkerPojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	@JsonAlias("moduleId")
	public ErrorMarkerPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public ErrorMarkerPojoPrototype setSeverity(final Severity severity) {
		this.severity.set(severity);
		return this;
	}

	public ErrorMarkerPojoPrototype setKey(final ErrorKey key) {
		this.key.set(key);
		return this;
	}

	public ErrorMarkerPojoPrototype setCause(final String cause) {
		this.cause.set(cause);
		return this;
	}

	public ErrorMarkerPojoPrototype setLocation(@Nullable final AstNodeLocation location) {
		this.location.set(location);
		return this;
	}

	public ErrorMarkerPojoPrototype setLine(final Integer line) {
		this.line.set(line);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module.orElse(null))
				.append("severity", severity.orElse(null))
				.append("key", key.orElse(null))
				.append("cause", cause.orElse(null))
				.append("location", location.orElse(null))
				.append("line", line.orElse(null))
				.toString();
	}
}
