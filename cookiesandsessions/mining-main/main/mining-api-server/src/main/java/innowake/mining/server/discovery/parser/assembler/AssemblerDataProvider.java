/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.assembler;

import innowake.lib.core.lang.Nullable;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.ndt.assembler.parser.InstructionRegistry;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;

/**
 * Data Provider for {@link InstructionRegistry} which enables the resolving of macros.
 * 
 * As long as there is no "AssemblerClipse" we provide this empty implementation
 */
public final class AssemblerDataProvider implements IAssemblingDataProvider<SourcePojo> {
	
	private final SourceObjectResolver sourceObjectResolver;

	public AssemblerDataProvider(final SourceObjectResolver sourceObjectResolver) {
		this.sourceObjectResolver = sourceObjectResolver;
	}

	@Nullable
	@Override
	public SourcePojo find(final SourcePojo root, final String name, final IAssemblingObjectType expectedType) {
		/* return null, if name contains null-bytes */
		for (int i = 0; i < name.length(); i++) {
			if (name.charAt(i) == 0x00) {
				return null;
			}
		}

		return sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.ASSEMBLER));
	}

	@Nullable
	@Override
	public String getPath(final SourcePojo object) {
		return object.getPath();
	}

	@Nullable
	@Override
	public String getSource(final SourcePojo object) {
		return object.getContent().toString();
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(final SourcePojo object) {
		return null;
	}

	@Nullable
	@Override
	public String getName(final SourcePojo object) {
		return object.getName();
	}

	@Nullable
	@Override
	public Object getHashable(final SourcePojo object) {
		return object;
	}

	@Override
	public boolean isObjectProxy(final SourcePojo object) {
		return false;
	}
	
}
