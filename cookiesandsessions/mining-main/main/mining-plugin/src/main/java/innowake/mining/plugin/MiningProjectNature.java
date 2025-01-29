/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;

import innowake.lib.core.api.lang.Nullable;


/**
 * A project nature for Mining features.
 */
public class MiningProjectNature implements IProjectNature {

	@Nullable
	private IProject project;
	
	/* a unique nature id for the mining nature */
	public static final String NATURE_ID = "innowake.mining.plugin.miningnature";
	
	@Override
	public void configure() throws CoreException {
	}

	@Override
	public void deconfigure() throws CoreException {
	}

	@Nullable
	@Override
	public IProject getProject() {
		return this.project;
	}

	@Override
	public void setProject(@Nullable final IProject project) {
		this.project = project;
	}

}
