/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.DataProvider;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider2;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.natural.INaturalAssemblingDataProvider;
import innowake.ndt.core.assembling.natural.NaturalAssemblingObjectType;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.natural.INaturalParseResult;

/**
 * Assembling provider for natural.
 */
public class NaturalAssemblingDataProvider implements INaturalAssemblingDataProvider<ModulePojo>, IAssemblingDataProvider2<ModulePojo> {
	
	private DataProvider dataProvider;
	
	/**
	 * Constructor.
	 * 
	 * @param dataProvider the {@link DataProvider} to delegate to
	 */
	public NaturalAssemblingDataProvider(final DataProvider dataProvider) {
		this.dataProvider = dataProvider;
	}

	@Override
	@Nullable
	public ModulePojo find(final ModulePojo root, final String name, final IAssemblingObjectType expectedType) {
		return dataProvider.find(root, name, expectedType);
	}
	
	@Override
	@Nullable
	public ModulePojo find2(final ModulePojo root, final String name, final IAssemblingObjectType... expectedTypes) {
		return dataProvider.find2(root, name, expectedTypes);
	}

	@Override
	@Nullable
	public String getPath(final ModulePojo object) {
		return dataProvider.getPath(object);
	}

	@Override
	@Nullable
	public String getSource(final ModulePojo object) throws AssemblingException {
		return dataProvider.getSource(object);
	}

	@Override
	@Nullable
	public IAssemblingObjectType getType(final ModulePojo object) {
		switch (object.getType()) {
			case COPYCODE:
				return NaturalAssemblingObjectType.COPYCODE;
			case LDA:
				return NaturalAssemblingObjectType.LDA;
			case GDA:
				return NaturalAssemblingObjectType.GDA;
			case PDA:
				return NaturalAssemblingObjectType.PDA;
			default:
				return dataProvider.getType(object);
		}
	}

	@Override
	@Nullable
	public String getName(final ModulePojo object) {
		return dataProvider.getName(object);
	}

	@Override
	@Nullable
	public Object getHashable(final ModulePojo object) {
		return dataProvider.getHashable(object);
	}

	@Override
	public boolean isObjectProxy(final ModulePojo object) {
		return dataProvider.isObjectProxy(object);
	}

	@Override
	@Nullable
	public IAst<? extends ModulePojo> getDependencyLightweightAst(@Nullable final ModulePojo object) throws Exception {
		return null;
	}

	@Override
	@Nullable
	public INaturalParseResult getParseResult(@Nullable final ModulePojo object) throws Exception {
		return null;
	}

	
}
