/*
 * Copyright (c) 2020 Deloitte innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.basic;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;


/**
 *  Default data provider based on {@link SourcePojo}.
 */
public class BasicDataProvider implements IAssemblingDataProvider<SourcePojo> {

	@Nullable
	@Override
	public SourcePojo find(final SourcePojo root, final String name, final IAssemblingObjectType expectedType) {
		throw new UnsupportedOperationException("not yet implemented");
	}

	@Nullable
	@Override
	public String getPath(final SourcePojo sourceObject) {
		return sourceObject.getPath();
	}

	@Nullable
	@Override
	public String getSource(final SourcePojo sourceObject) {
		return sourceObject.getContent().toString();
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(final SourcePojo p) {
		return null;
	}

	@Nullable
	@Override
	public String getName(final SourcePojo p) {
		return p.getName();
	}

	@Nullable
	@Override
	public Object getHashable(final SourcePojo p) {
		return p;
	}
	
	@Override
	public boolean isObjectProxy(final SourcePojo object) {
		return false;
	}
}
