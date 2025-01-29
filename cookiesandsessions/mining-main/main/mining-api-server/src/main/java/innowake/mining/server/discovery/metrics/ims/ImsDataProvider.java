/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.ims;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;

/**
 * Data provider for IMS parser.
 */
public class ImsDataProvider implements IAssemblingDataProvider<SourcePojo> {

	@Nullable
	@Override
	public SourcePojo find(SourcePojo root, String name, IAssemblingObjectType expectedType) {
		return null;
	}

	@Nullable
	@Override
	public String getPath(SourcePojo object) {
		return object.getPath();
	}

	@Nullable
	@Override
	public String getSource(SourcePojo object) throws AssemblingException {
		return object.getContent().toString();
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(SourcePojo object) {
		return null;
	}

	@Nullable
	@Override
	public String getName(SourcePojo object) {
		return object.getName();
	}

	@Nullable
	@Override
	public Object getHashable(SourcePojo object) {
		return object;
	}

	@Override
	public boolean isObjectProxy(SourcePojo object) {
		return false;
	}

}
