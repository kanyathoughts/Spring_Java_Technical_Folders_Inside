/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.plsql;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;

/**
 * PlSql data provider based on {@link SourcePojo}
 */
public class PlSqlDataProvider implements IAssemblingDataProvider<SourcePojo> {

	@Nullable
	@Override
	public SourcePojo find(final SourcePojo root, final String name, final IAssemblingObjectType expectedType) {
		throw new UnsupportedOperationException("not implemented");
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
	public IAssemblingObjectType getType(final SourcePojo sourceObject) {
		throw new UnsupportedOperationException("not implemented");
	}

	@Nullable
	@Override
	public String getName(final SourcePojo sourceObject) {
		return sourceObject.getName();
	}

	@Nullable
	@Override
	public Object getHashable(final SourcePojo sourceObject) {
		return sourceObject;
	}

	@Override
	public boolean isObjectProxy(final SourcePojo sourceObject) {
		return false;
	}
}
