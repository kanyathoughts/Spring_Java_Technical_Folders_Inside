/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.module.ui.handler;

import java.util.Optional;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

import innowake.base.eclipse.common.core.NatureDescription;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningProjectNature;
import innowake.mining.plugin.base.ui.SelectionUtil;

/**
 * Handler for adding the nature {@link MiningProjectNature} to the selected project.
 */
public class AddMiningNatureHandler extends AbstractHandler {

	@Nullable
	@Override
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final Optional<IProject> optionalProject = SelectionUtil.getProjectFromSelectedResource();
		optionalProject.ifPresent(project -> {
			try {
				NatureDescription.addNature(project, "Mining Nature", MiningProjectNature.NATURE_ID, null, false, null);
			} catch (final CoreException e) {
				Logging.warn(String.format("Could not add Mining nature to the project %s. Error %s", project.getName(), e.getMessage()));
			}
		});
		return null;
	}

}
