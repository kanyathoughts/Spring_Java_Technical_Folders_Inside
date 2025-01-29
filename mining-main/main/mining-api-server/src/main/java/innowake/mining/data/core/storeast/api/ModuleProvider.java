/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.core.storeast.api;

import innowake.mining.data.core.storeast.impl.ModuleProviderImpl;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.retrace.Retracing;

/**
 * Returns the module ID for a given original object. 
 * 
 * @param <T> the concrete type of {@link Retracing}
 */
public interface ModuleProvider<T> {

	/**
	 * Creates a new instance.
	 * 
	 * @param retracingProvider the {@link RetracingProvider}
	 * @return a new instance
	 */
	public static ModuleProvider<ModulePojo> createInstance(final RetracingProvider<ModulePojo> retracingProvider) {
		return new ModuleProviderImpl(retracingProvider);
	}

	/**
	 * Returns the of the root module. 
	 *
	 * @return the root module ID
	 */
	public ModulePojo getRootModule();
	
	/**
	 * Returns the module of the given original object. 
	 *
	 * @param originalObject the original object
	 * @return the module ID of the original object
	 */
	public ModulePojo getModule(T originalObject);
}
