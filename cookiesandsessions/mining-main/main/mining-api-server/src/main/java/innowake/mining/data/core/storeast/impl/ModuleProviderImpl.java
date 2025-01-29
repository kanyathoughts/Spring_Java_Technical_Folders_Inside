/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Core implementation for {@link ModuleProvider}.
 */
public final class ModuleProviderImpl extends AbstractModuleProvider<ModulePojo> {

	/**
	 * Constructor.
	 * 
	 * @param retracingProvider the {@link RetracingProviderImpl}
	 */
	public ModuleProviderImpl(final RetracingProvider<ModulePojo> retracingProvider) {
		init(retracingProvider);
	}

	@Override
	public ModulePojo getModule(final ModulePojo originalObject) {
		return originalObject;
	}
}
