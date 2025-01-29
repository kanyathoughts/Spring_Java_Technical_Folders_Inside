/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.Assert.assertNotNull;

import java.util.HashMap;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;

/**
 * Implementation of {@link IAssemblingDataProvider} for tests.
 */
public class TestAssemblingProvider implements IAssemblingDataProvider<ModulePojo> {
	
	private final Map<String, ModulePojo> nameToObject = new HashMap<>();
	
	@Override
	@Nullable
	public ModulePojo find(final ModulePojo root, final String name, final IAssemblingObjectType expectedType) {
		return nameToObject.get(name);
	}

	@Override
	@Nullable
	public String getPath(final ModulePojo object) {
		return null;
	}

	@Override
	@Nullable
	public String getSource(final ModulePojo object) throws AssemblingException {
		return object.getContent().orElse(null);
	}

	@Override
	@Nullable
	public IAssemblingObjectType getType(final ModulePojo object) {
		return null;
	}

	@Override
	@Nullable
	public String getName(final ModulePojo object) {
		return object.getName();
	}

	@Override
	@Nullable
	public ModulePojo getHashable(final ModulePojo object) {
		return object;
	}
	
	@Override
	public boolean isObjectProxy(final ModulePojo object) {
		return false;
	}
	
	public void add(final ModulePojo object) {
		assertNotNull(object);
		this.nameToObject.put(object.getName(), object);
	}

}