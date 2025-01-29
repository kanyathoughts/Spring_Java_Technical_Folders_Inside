/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo.Severity;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Error marker for {@link DataFlowGraphNode}.
 */
public class DataFlowError implements Serializable, Comparable<DataFlowError> {

	private final Severity severity;
	private final String text;
	
	/**
	 * Creates a new DataFlowError instance.
	 * @param severity severity of the error
	 * @param text description of the error
	 */
	@ConstructorProperties({"severity", "text"})
	public DataFlowError(final Severity severity, final String text) {
		this.severity = severity;
		this.text = text;
	}
	
	/**
	 * @return the {@link Severity}
	 */
	public Severity getSeverity() {
		return severity;
	}
	
	/**
	 * @return the text of the {@link Throwable} in case an error has occurred
	 */
	public String getText() {
		return text;
	}
	
	/**
	 * Converts {@link DataFlowError} to Map.
	 *
	 * @return The Map of the given {@link DataFlowError}.
	 */
	public Map<String, String> toMap()
	{
		final Map<String, String> data = new HashMap<>();
		data.put("severity", severity.toString());
		data.put("text", text);
		return data;
	}
	
	@Override
	public String toString() {
		return severity + ": " + text;
	}

	@Override
	public int hashCode() {
		return Objects.hash(severity, text);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final DataFlowError other = (DataFlowError) obj;
		return severity == other.severity && Objects.equals(text, other.text);
	}

	@Override
	public int compareTo(@Nullable final DataFlowError o) {
		if (o == null) {
			return 1;
		}
		final int comp = severity.compareTo(o.getSeverity());
		return comp == 0 ? text.compareTo(o.getText()) : comp;
	}
}
