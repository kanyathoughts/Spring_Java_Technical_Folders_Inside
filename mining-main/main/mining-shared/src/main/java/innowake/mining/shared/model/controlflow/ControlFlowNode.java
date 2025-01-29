/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.controlflow;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * Generic representation of a node on the control flow.
 */
public final class ControlFlowNode {
	
	public final UUID id;
	
	public final UUID module;
	
	public final ControlFlowEntity entity;
	
	@Nullable
	public String type;
	
	@Nullable
	public UUID parent;
	
	@Nullable
	public String label;
	
	public final Map<String, Object> properties;
	
	@Nullable
	public Integer offset;
	
	@Nullable
	public Integer length;
	
	public Set<String> superTypes;
	
	public ControlFlowNode(final UUID id, final UUID module, final ControlFlowEntity entity) {
		this.id = id;
		this.module = module;
		this.entity = entity;
		properties = new HashMap<>();
		superTypes = new HashSet<>();
	}
	
	@JsonCreator
	public ControlFlowNode(
			@JsonProperty("id") final UUID id,
			@JsonProperty("module") final UUID module,
			@JsonProperty("entity") final ControlFlowEntity entity,
			@JsonProperty("type") final String type,
			@JsonProperty("parent") final UUID parent,
			@JsonProperty("label") final String label,
			@JsonProperty("properties") final Map<String, Object> properties,
			@JsonProperty("offset") final Integer offset,
			@JsonProperty("length") final Integer length,
			@JsonProperty("superTypes") final Set<String> superTypes) {
		this.id = id;
		this.module = module;
		this.entity = entity;
		this.type = type;
		this.parent = parent;
		this.label = label;
		this.properties = properties;
		this.offset = offset;
		this.length = length;
		this.superTypes = superTypes;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("entity", entity)
				.append("type", type)
				.append("label", label)
				.append("superTypes", superTypes)
				.append("module", module)
				.append("parent", parent)
				.append("offset", offset)
				.append("length", length)
				.append("properties", properties)
			.toString();
	}
}
