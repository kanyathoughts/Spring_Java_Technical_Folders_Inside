/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * {@code effort_summary} entity request class.
 */
public class EffortSummaryPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> project = new Definable<>(false, "EffortSummary.project");
	public final Definable<Long> index = new Definable<>(false, "EffortSummary.index");
	public final Definable<EffortSummaryType> type = new Definable<>(false, "EffortSummary.type");
	public final Definable<Map<String,Object>> properties = new Definable<>(false, "EffortSummary.properties");

	public EffortSummaryPojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public EffortSummaryPojoPrototype setIndex(final Long index) {
		this.index.set(index);
		return this;
	}

	public EffortSummaryPojoPrototype setType(final EffortSummaryType type) {
		this.type.set(type);
		return this;
	}

	public EffortSummaryPojoPrototype setProperties(final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("project", project.orElse(null))
				.append("index", index.orElse(null))
				.append("type", type.orElse(null))
				.append("properties", properties.orElse(null))
				.toString();
	}
}