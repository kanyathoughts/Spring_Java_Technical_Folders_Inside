/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * {@code effort_summary} entity class.
 */
public class EffortSummaryPojo {

	private final EntityId project;
	private final Long index;
	private final EffortSummaryType type;
	private final Map<String, Object> properties;

	public EffortSummaryPojo(
			@JsonProperty("project") final EntityId project,
			@JsonProperty("index") final Long index,
			@JsonProperty("type") final EffortSummaryType type,
			@JsonProperty("properties") final Map<String, Object> properties) {
		this.project = project;
		this.index = index;
		this.type = type;
		this.properties = properties;
	}

	public EntityId getProject() {
		return project;
	}

	public Long getIndex() {
		return index;
	}

	public EffortSummaryType getType() {
		return type;
	}

	public Map<String, Object> getProperties() {
		return properties;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("project", project)
				.append("index", index)
				.append("type", type)
				.append("properties", properties)
				.toString();
	}
}