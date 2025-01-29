package innowake.mining.shared.model.datalineage.util;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Abstract parent class for all kinds of data lineage graph nodes.
 */
public abstract class Element {
	private final DataFlowGraphNode dataFlowGraphNode;

    protected Element(final DataFlowGraphNode node) {
        this.dataFlowGraphNode = node;
    }

    public DataFlowGraphNode getFlowGraphNode() {
    	return dataFlowGraphNode;
    }
    
    public String getName() {
    	return dataFlowGraphNode.getName();
    }
    
    @Override
    public boolean equals(final @Nullable Object other) {
    	if (other == null) {
    		return false;
    	}
    	
    	if ( ! (other instanceof Element)) {
    		return false;
    	}
    	final Element element = (Element) other;
    	return dataFlowGraphNode.getId().equals(element.getFlowGraphNode().getId());
    }
    
    @Override
    public int hashCode() {
    	return dataFlowGraphNode.getId().hashCode();
    }
}
