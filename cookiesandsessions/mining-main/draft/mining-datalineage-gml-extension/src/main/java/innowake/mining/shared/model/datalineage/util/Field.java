package innowake.mining.shared.model.datalineage.util;

import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Graph element that represents a field in our data lineage graph.
 */
public class Field extends Element {

	public Field(final DataFlowGraphNode node) {
		super(node);
	}
}
