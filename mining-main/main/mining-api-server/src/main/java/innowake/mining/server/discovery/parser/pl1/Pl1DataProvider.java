/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.pl1;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.parsing.assembling.pl1.Pl1Assembler.Pl1AssemblingObjectType;

/**
 * Default data provider based on {@link SourcePojo}
 */
public class Pl1DataProvider implements IAssemblingDataProvider<SourcePojo> {

	private final SourceObjectResolver sourceObjectResolver;
	
	public Pl1DataProvider(final SourceObjectResolver sourceObjectResolver) {
		this.sourceObjectResolver = sourceObjectResolver;
	}
	
	@Nullable
	@Override
	public SourcePojo find(final SourcePojo root, final String name, final IAssemblingObjectType expectedType) {
		return sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.PL1, Type.COPYBOOK));
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
		return sourceObject.getType() == Type.COPYBOOK ? Pl1AssemblingObjectType.INCLUDE : null;
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
