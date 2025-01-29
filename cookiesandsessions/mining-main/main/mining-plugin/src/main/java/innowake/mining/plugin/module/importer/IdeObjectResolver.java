/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import org.eclipse.core.resources.IFile;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.ide.core.object.IIdeObject;

/**
 * Resolves {@link IFile}s to {@link IIdeObject}s.
 */
public interface IdeObjectResolver {
	
	/**
	 * Resolves an {@link IIdeObject} for the {@link IFile}.
	 *
	 * @param file the {@link IFile}
	 * @return the {@link IIdeObject}
	 */
	@Nullable
	public <O extends IIdeObject<O>> O findObject(IFile file);
}
