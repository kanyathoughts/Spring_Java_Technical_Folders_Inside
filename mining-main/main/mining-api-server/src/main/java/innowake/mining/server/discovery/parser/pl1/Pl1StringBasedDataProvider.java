/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.pl1;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;

/**
 * Default data provider based on {@link String}
 */
public class Pl1StringBasedDataProvider implements IAssemblingDataProvider<String> {

	
	@Nullable
	@Override
	public String find(final String root, final String name, final IAssemblingObjectType expectedType) {
		throw new UnsupportedOperationException("not implemented");
	}

	@Nullable
	@Override
	public String getPath(final String string) {
		throw new UnsupportedOperationException("not implemented");
	}

	@Nullable
	@Override
	public String getSource(final String string) {
		return string;
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(final String string) {
		throw new UnsupportedOperationException("not implemented");
	}

	@Nullable
	@Override
	public String getName(final String string) {
		throw new UnsupportedOperationException("not implemented");
	}

	@Nullable
	@Override
	public Object getHashable(final String string) {
		return string;
	}
	
	@Override
	public boolean isObjectProxy(final String string) {
		return false;
	}
}
