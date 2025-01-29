package innowake.mining.shared.model.datalineage.util;

import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Graph element that represents a statement in our data lineage graph.
 */
public class Statement extends Element {
	
	public Statement(final DataFlowGraphNode node) {
		super(node);
	}
}
