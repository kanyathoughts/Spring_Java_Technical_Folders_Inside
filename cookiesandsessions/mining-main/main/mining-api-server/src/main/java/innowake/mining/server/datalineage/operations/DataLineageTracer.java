/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataLineageResult;

/**
 * Interface for Data Lineage components that trace the data flow inside a Module.
 * <p>
 * Whenever data flows in or out of a Module, implementations are supposed to create a {@link ProxyContainerPojo} describing this data flow.
 * The Proxy Containers can then be linked with other Modules through an appropriate {@link DataLineageResolver} interface.
 * <p>
 * The methods of this interface return {@link DataLineageResult} which contains the changes (e.g. new {@link DataFlowNodePojo Data Flow Nodes},
 * {@linkplain ProxyContainerPojo Proxy Containers} and {@linkplain DataFlow Data Flows}) that are to be merged into the global Data Lineage model.
 */
public interface DataLineageTracer {

	/**
	 * Returns whether this tracer supports the given Module.
	 *
	 * @param module the Module
	 * @return {@code true} if supported
	 */
	boolean isSupported(final ModuleLightweightPojo module);

	/**
	 * Traces the data flow inside the given module, returning a {@link DataLineageResult} with the changes to be made (i.e. new Data Flow Nodes
	 * and Proxy Containers to be created).
	 *
	 * @param context the context for the operation
	 * @param module the module to trace
	 * @return the changes that need to be applied to the Data Lineage model
	 */
	DataLineageResult trace(DataLineageContext context, ModuleLightweightPojo module);

	/**
	 * Discovers Proxy Containers of a certain type on the given module. This method allows to implement a "lightweight" variant of
	 * {@link #trace(DataLineageContext, ModuleLightweightPojo)} where not the full data flow of the module needs to be traced, but only the point where
	 * data enters or leaves the module must be discovered. Furthermore, this is restricted to proxy containers of a certain type, so the implementation
	 * only need to discover certain things (e.g. entry points, database accesses) inside the module.
	 * <p>
	 * If there is no meaningful way to implement this kind of "lightweight" trace, implementations can delegate to 
	 * {@link #trace(DataLineageContext, ModuleLightweightPojo)} to do a full trace instead (which should also discover the proxy containers).
	 *
	 * @param context the context for the operation
	 * @param module the module on which to discover proxy containers
	 * @param type the type of proxy container to discover
	 * @return the changes that need to be applied to the Data Lineage model
	 */
	DataLineageResult discoverProxyContainersOfType(DataLineageContext context, ModuleLightweightPojo module, ProxyContainerPojo.Type type);
}
