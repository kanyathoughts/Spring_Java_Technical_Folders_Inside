/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataLineageResult;

/**
 * Interface for Data Lineage components that resolve and link data flow between modules. More specifically they are responsible of linking the
 * fields of two or more {@linkplain ProxyContainerPojo Proxy Containers} with each other, by creating {@link DataFlow} between them.
 * <p>
 * The methods of this interface return {@link DataLineageResult} which contains the changes (e.g. new {@linkplain ProxyContainerPojo Proxy Containers}
 * and {@linkplain DataFlow Data Flows}) that are to be merged into the global Data Lineage model.
 */
public interface DataLineageResolver {

	/**
	 * Returns whether this resolver supports the given Proxy Container.
	 *
	 * @param module the Module containing the Proxy Container
	 * @param proxyContainer the Proxy Container
	 * @return {@code true} if supported
	 */
	boolean isSupported(final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer);

	/**
	 * Resolves and links the given Proxy Container, returning a {@link DataLineageResult} with the changes to be made (i.e. new {@link DataFlow}
	 * to be created.
	 *
	 * @param context the context for the operation
	 * @param module the module containing Proxy Container
	 * @param proxyContainer the Proxy Container to resolve
	 * @return the changes that need to be applied to resolve the container
	 */
	DataLineageResult resolve(DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer);

	/**
	 * Passes a reference of {@link DataLineageCoreService}. Required, because resolvers may have to call
	 * {@link DataLineageCoreService#getProxyContainersOfType(DataLineageContext, EntityId, ProxyContainerPojo.Type)} in order to discover Proxy Containers
	 * on other Modules for linking.
	 *
	 * @param dataLineageCoreService the {@code DataLineageCoreService} instance
	 */
	void setDataLineageCoreService(DataLineageCoreService dataLineageCoreService);
}
