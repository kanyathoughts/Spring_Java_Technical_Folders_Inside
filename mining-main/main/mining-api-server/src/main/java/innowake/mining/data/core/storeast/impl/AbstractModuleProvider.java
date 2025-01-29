/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.retrace.Retracing;

/**
 * Abstract base implementation for {@link ModuleProvider}.
 * 
 * @param <T> the concrete type of {@link Retracing}
 */
public abstract class AbstractModuleProvider<T> implements ModuleProvider<T> {

	@Nullable
	private ModulePojo rootModule;

	@Override
	public ModulePojo getRootModule() {
		return assertNotNull(rootModule);
	}
	
	protected void init(final RetracingProvider<T> retracingProvider) {
		rootModule = getModule(retracingProvider.getRoot());
	}
}
