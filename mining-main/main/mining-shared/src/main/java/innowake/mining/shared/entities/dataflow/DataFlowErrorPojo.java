/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.dataflow;

import java.io.Serializable;
import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;

import innowake.lib.core.api.lang.Nullable;

/**
 * Model class for a DataFlowError entity
 */
public class DataFlowErrorPojo implements Serializable, Comparable<DataFlowErrorPojo> {

	public enum Severity {
		/**
		 * Data flow could not be analyzed correctly due to a known limitation of data lineage. Examples for such errors are unhandled statements
		 * or module types.
		 */
		WARNING,
		/**
		 * Data flow could not be analyzed due to an unexpected error. Results are likely incomplete or incorrect.
		 */
		ERROR
	}

	private final UUID nodeId;
	private final Severity severity;
	private final String text;

	@JsonCreator
	public DataFlowErrorPojo(
			final UUID nodeId,
			final Severity severity,
			final String text) {
		this.nodeId = nodeId;
		this.severity = severity;
		this.text = text;
	}

	public UUID getDataFlowNodeId() {
		return nodeId;
	}

	public Severity getSeverity() {
		return severity;
	}

	public String getText() {
		return text;
	}

	@Override
	public String toString() {
		return severity + ": " + text;
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		final DataFlowErrorPojo that = (DataFlowErrorPojo) o;
		return Objects.equals(nodeId, that.nodeId)
				&& Objects.equals(severity, that.severity)
				&& Objects.equals(text, that.text);
	}

	@Override
	public int hashCode() {
		return Objects.hash(nodeId, severity, text);
	}

	@Override
	public int compareTo(@Nullable final DataFlowErrorPojo o) {
		if (o == null) {
			return 1;
		}
		final int comp = severity.compareTo(o.getSeverity());
		return comp == 0 ? text.compareTo(o.getText()) : comp;
	}
}
