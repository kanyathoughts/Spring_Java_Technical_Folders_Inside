/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import innowake.lib.core.api.lang.Prototype;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to module services.
 */
public class ModuleServiceProvider extends ServiceProvider<ModuleServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public ModuleServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllModules}.
	 *
	 * @return the service instance
	 */
	public FindAllModules findAllModules() {
		return new FindAllModules(connectionInfo);
	}
	
	/**
	 * Access to {@link FindModuleByPath}.
	 *
	 * @return the service instance
	 */
	public FindModuleByPath findModuleByPath() {
		return new FindModuleByPath(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllLinkedModules}.
	 * 
	 * @return the service instance
	 */
	public FindAllLinkedModules findAllLinkedModules() {
		return new FindAllLinkedModules(connectionInfo);
	}
	
	/**
	 * Access to {@link FindModuleById}.
	 *
	 * @return the service instance
	 */
	public FindModuleById findModuleById() {
		return new FindModuleById(connectionInfo);
	}

	/**
	 * Access to {@link GetModuleCount}.
	 *
	 * @return the service instance
	 */
	public GetModuleCount getModuleCount() {
		return new GetModuleCount(connectionInfo);
	}

	/**
	 * Access to {@link CreateModule}.
	 *
	 * @return the service instance
	 */
	public CreateModule createModule() {
		return new CreateModule(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateModule}.
	 *
	 * @return the service instance
	 */
	public UpdateModule updateModule() {
		return new UpdateModule(connectionInfo);
	}
	
	/**
	 * Access to {@link DeleteModule}.
	 *
	 * @return the service instance
	 */
	public DeleteModule deleteModule() {
		return new DeleteModule(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAnnotationsByModule}.
	 *
	 * @return the service instance
	 */
	public FindAnnotationsByModule findAnnotationsByModule() {
		return new FindAnnotationsByModule(connectionInfo);
	}
	
	/**
	 * Access to {@link FindModuleByName}.
	 *
	 * @return the service instance
	 */
	public FindModuleByName findModuleByName() {
		return new FindModuleByName(connectionInfo); 
	}

	/**
	 * Access to {@link DeleteAllModules}.
	 *
	 * @return the service instance
	 */
	public DeleteAllModules deleteAllModules() {
		return new DeleteAllModules(connectionInfo);
	}
	
	/**
	 * Access to {@link FindTaxonomiesByModule}.
	 *
	 * @return the service instance
	 */
	public FindTaxonomiesByModule findTaxonomiesByModule() {
		return new FindTaxonomiesByModule(connectionInfo);
	}
	
	/**
	 * Access to {@link GetHotSpots}
	 *
	 * @return the service instance
	 */
	public GetHotSpots getHotSpots() {
		return new GetHotSpots(connectionInfo);
	}
	
	/**
	 * Access to {@link TraverseDependencies}.
	 *
	 * @return the service instance
	 */
	public TraverseDependencies traverseDependencies() {
		return new TraverseDependencies(connectionInfo);
	}
	
	/**
	 * Access to {@link IdentifyModuleDescriptions}.
	 *
	 * @return the service instance
	 */
	@Prototype(irisId = "WMIN-597")
	public IdentifyModuleDescriptions identifyModuleDescriptions() {
		return new IdentifyModuleDescriptions(connectionInfo);
	}

	/**
	 * Access to {@link HasAstNodes}
	 *
	 * @return the service instance
	 */
	public HasAstNodes hasAstNodes() {
		return new HasAstNodes(connectionInfo);
	}

	/**
	 * Access to {@link StoreAstNodes}.
	 *
	 * @return the service instance
	 */
	public StoreAstNodes storeAstNodes() {
		return new StoreAstNodes(connectionInfo);
	}
	
	/**
	 * Access to {@link GetAggregatedUtilityValues}.
	 *
	 * @return the service instance
	 */
	public GetAggregatedUtilityValues getAggregatedUtilityValues() {
		return new GetAggregatedUtilityValues(connectionInfo);
	}
}
