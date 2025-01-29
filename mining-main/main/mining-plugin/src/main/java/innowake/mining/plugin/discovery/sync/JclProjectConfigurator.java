/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.discovery.sync;

import static innowake.mining.shared.discovery.config.core.IdentificationMapper.getRelativePath;

import org.eclipse.core.resources.IProject;

import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.batchclipse.core.BatchclipseCore;
import innowake.product.base.core.api.ApiException;

/**
 * Jcl language project configuration.
 * <li>Set the batchclipe project nature.
 */
public class JclProjectConfigurator extends AbstractProjectConfigurator {

	public JclProjectConfigurator() {
		super(ResolveTarget.JCL);
	}

	@Override
	public void configure(final IProject project) {
		try {
			copy(".batch-path", project);
			copy("jclconfig.cfg", project, getRelativePath(ResolveTarget.JCL) + "/jclconfig.cfg");
			addProjectNature(BatchclipseCore.NATURE, project);

			super.configure(project);
		} catch (final ApiException e) {
			LOG.error(() -> "Error while applying JCL configurator: " + e);
		}
	}

}
