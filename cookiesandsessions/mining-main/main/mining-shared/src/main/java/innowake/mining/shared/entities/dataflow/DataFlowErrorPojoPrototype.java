/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.entities.dataflow;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.mining.shared.Definable;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * TaxonomyCategoryPojoPrototype entity request class.
 */
public class DataFlowErrorPojoPrototype implements PojoPrototype {

	public final Definable<UUID> nodeId = new Definable<>(false, "DataFlowError.nodeId");
	public final Definable<DataFlowErrorPojo.Severity> severity = new Definable<>(false, "DataFlowError.severity");
	public final Definable<String> text = new Definable<>(false, "DataFlowError.text");

	public DataFlowErrorPojoPrototype setNodeId(final UUID nodeId) {
		this.nodeId.set(nodeId);
		return this;
	}

	public DataFlowErrorPojoPrototype setSeverity(final DataFlowErrorPojo.Severity severity) {
		this.severity.set(severity);
		return this;
	}

	public DataFlowErrorPojoPrototype setText(final String text) {
		this.text.set(text);
		return this;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("nodeId", nodeId.orElse(null));
		builder.append("severity", severity.orElse(null));
		builder.append("text", text.orElse(null));
		return builder.toString();
	}
}
