/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.generic;

import org.eclipse.jface.text.IDocument;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.parsing.Parser;


/**
 * Base eclipse specific data provider for {@link Parser}'s parsing configuration.
 */
public class DocumentAssemblingProvider implements IAssemblingDataProvider<IDocument> {

	@Override
	@Nullable
	public IDocument find(final IDocument arg0, final String arg1, final IAssemblingObjectType arg2) {
		return null;
	}

	@Override
	@Nullable
	public Object getHashable(final IDocument arg0) {
		return null;
	}

	@Override
	@Nullable
	public String getName(final IDocument arg0) {
		return null;
	}

	@Override
	@Nullable
	public String getPath(final IDocument arg0) {
		return null;
	}

	@Override
	@Nullable
	public String getSource(final IDocument arg0) throws AssemblingException {
		return arg0.get();
	}

	@Override
	@Nullable
	public IAssemblingObjectType getType(final IDocument arg0) {
		return null;
	}

	@Override
	public boolean isObjectProxy(final IDocument arg0) {
		return false;
	}

}
