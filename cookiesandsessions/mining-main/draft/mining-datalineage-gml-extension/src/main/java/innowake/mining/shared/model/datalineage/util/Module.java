package innowake.mining.shared.model.datalineage.util;

import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Graph element that represents a module in our data lineage graph.
 */
public class Module extends Element {

	public Module(final DataFlowGraphNode node) {
		super(node);
	}
}
