package innowake.mining.shared.model.datalineage.util;

import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

public abstract class Element {
	private final DataFlowGraphNode dataFlowGraphNode;

    public Element(final DataFlowGraphNode node) {
        this.dataFlowGraphNode = node;
    }

    public DataFlowGraphNode getFlowGraphNode() {
    	return dataFlowGraphNode;
    }
    
    public String getName() {
    	return dataFlowGraphNode.getName();
    }
}
