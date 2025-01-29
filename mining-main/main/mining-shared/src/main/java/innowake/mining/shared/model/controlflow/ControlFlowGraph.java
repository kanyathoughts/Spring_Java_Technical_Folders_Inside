/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.controlflow;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;

/**
 * POJO class that represents a Control Flow Graph.
 */
public final class ControlFlowGraph {
	
	public final List<ControlFlowNode> nodes;
	public final List<ControlFlowEdge> edges;
	public final List<AnnotationPojo> annotations;
	public final List<ModuleLightweightPojo> relatedModules;
	
	public ControlFlowGraph() {
		nodes = new ArrayList<>();
		edges = new ArrayList<>();
		annotations = new ArrayList<>();
		relatedModules = new ArrayList<>();
	}
	
	@JsonCreator
	public ControlFlowGraph(
			@JsonProperty("nodes") final List<ControlFlowNode> nodes,
			@JsonProperty("edges") final List<ControlFlowEdge> edges,
			@JsonProperty("annotations") final List<AnnotationPojo> annotations,
			@JsonProperty("relatedModules") final List<ModuleLightweightPojo> relatedModules) {
		super();
		this.nodes = nodes;
		this.edges = edges;
		this.annotations = annotations;
		this.relatedModules = relatedModules;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("nodes", nodes)
				.append("edges", edges)
				.append("annotations", annotations)
				.append("relatedModules", relatedModules)
			.toString();
	}
}
