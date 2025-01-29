/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import java.util.HashMap;
import java.util.Map;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.testing.ModulePojoDummy;

/**
 * Test implementation for {@link ModuleProvider}.
 */
final class TestModuleProvider<T> extends AbstractModuleProvider<T> {

	private final Map<T, ModulePojo> moduleCache = new HashMap<>();
	
	private int moduleIdCnt = 0;
	
	/**
	 * Constructor.
	 * 
	 * @param retracingProvider the {@link RetracingProviderImpl}
	 */
	TestModuleProvider(final RetracingProvider<T> retracingProvider) {
		init(retracingProvider);
	}

	@Override
	public ModulePojo getModule(final T name) {
		return moduleCache.computeIfAbsent(name, this::toModule);
	}

	private ModulePojo toModule(@SuppressWarnings("unused") final T name) {
		return new ModulePojoDummy().prepare(m -> m.setNid(Long.valueOf(++moduleIdCnt))).build();
	}
}
