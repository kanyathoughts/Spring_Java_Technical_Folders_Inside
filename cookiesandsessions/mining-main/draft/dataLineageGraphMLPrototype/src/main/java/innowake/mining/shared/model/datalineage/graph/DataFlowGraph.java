/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage.graph;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator.Mode;

/**
 * A Data Flow Graph consisting of {@link DataFlowGraphNode}. This is the default export format for Data Lineage. This is class is intended
 * for JSON serialization.
 */
public class DataFlowGraph implements Serializable {

	private final List<DataFlowGraphNode> nodes;

	/**
	 * Construct a new graph from a list of nodes
	 * @param nodes the list of nodes in the graph
	 */
	@JsonCreator(mode = Mode.PROPERTIES)
	public DataFlowGraph(final @JsonProperty("nodes") List<DataFlowGraphNode> nodes) {
		this.nodes = nodes;
	}

	/**
	 * Gets the nodes in the graph.
	 *
	 * @return the nodes in the graph
	 */
	public List<DataFlowGraphNode> getNodes() {
		return nodes;
	}
}
