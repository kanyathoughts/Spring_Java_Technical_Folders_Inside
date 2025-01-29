/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.controlflow;

import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.lang.Nullable;

/**
 * Generic representation of a edge on the control flow.
 */
public final class ControlFlowEdge {
	
	public final UUID fromId;
	public final UUID toId;
	
	@Nullable
	public String label;
	
	public ControlFlowEdge(final UUID fromId, final UUID toId) {
		this.fromId = fromId;
		this.toId = toId;
	}
	
	@JsonCreator
	public ControlFlowEdge(
			@JsonProperty("fromId") final UUID fromId,
			@JsonProperty("toId") final UUID toId,
			@JsonProperty("label") final String label) {
		this(fromId, toId);
		this.label = label;
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof ControlFlowEdge)) {
			return false;
		}
		final ControlFlowEdge edge = (ControlFlowEdge) o;
		return Objects.equals(fromId, edge.fromId) && Objects.equals(toId, edge.toId) && Objects.equals(label, edge.label);
	}

	@Override
	public int hashCode() {
		return Objects.hash(fromId, toId, label);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("label", label)
				.append("from", fromId)
				.append("to", toId)
			.toString();
	}
}
