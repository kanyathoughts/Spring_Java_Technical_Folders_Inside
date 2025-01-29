/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;

/**
 * Default "assembling" data provider that does not support any kind of assembling and just returns the content of a source object.
 */
public class DefaultDataProvider implements IAssemblingDataProvider<SourcePojo> {
	@Nullable
	@Override
	public SourcePojo find(@NonNull final SourcePojo root, @NonNull final String name, @NonNull final IAssemblingObjectType expectedType) {
		return null;
	}

	@Nullable
	@Override
	public String getPath(@NonNull final SourcePojo object) {
		return object.getPath();
	}

	@Nullable
	@Override
	public String getSource(@NonNull final SourcePojo object) throws AssemblingException {
		return object.getContent().toString();
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(@NonNull final SourcePojo object) {
		return null;
	}

	@Nullable
	@Override
	public String getName(@NonNull final SourcePojo object) {
		return object.getName();
	}

	@Nullable
	@Override
	public Object getHashable(@NonNull final SourcePojo object) {
		return object;
	}

	@Override
	public boolean isObjectProxy(@NonNull final SourcePojo object) {
		return false;
	}
}
