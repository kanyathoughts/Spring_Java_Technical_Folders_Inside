/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.discovery.sync;

import org.eclipse.core.resources.IProject;

import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.cobolclipse.core.CobolclipseCore;
import innowake.product.base.core.api.ApiException;

/**
 * Cobol language project settings.
 * <li>Add the cobol Mainframe project nature.
 * <li>Copy the initial .cobol-path file into the project root folder.
 */
public class CobolProjectConfigurator extends AbstractProjectConfigurator {

	public CobolProjectConfigurator() {
		super(ResolveTarget.COBOL);
	}

	@Override
	public void configure(final IProject project) {
		try {
			copy(".cobol-path", project);
			addProjectNature(CobolclipseCore.NATURE_MF, project);
			super.configure(project);
		} catch (final ApiException e) {
			LOG.error(() -> "Error while applying Cobol configurator: " + e);
		}

	}

}
