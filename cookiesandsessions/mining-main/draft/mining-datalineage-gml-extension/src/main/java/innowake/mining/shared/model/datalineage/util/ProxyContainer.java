package innowake.mining.shared.model.datalineage.util;

import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 *  Groups {@link DataInterface} inside a module.
 */
public class ProxyContainer extends Module {
	
	public ProxyContainer(final DataFlowGraphNode node) {
		super(node);
	}
}
