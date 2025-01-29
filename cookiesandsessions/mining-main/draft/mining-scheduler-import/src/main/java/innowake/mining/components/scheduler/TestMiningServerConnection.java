/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.components.scheduler;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.equinox.security.storage.StorageException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.components.scheduler.connection.ConnectionFactory;
import innowake.mining.components.scheduler.connection.ProjectData;
import innowake.product.base.core.api.ApiException;
import innowake.product.base.ui.api.script.IPopupExecuteContext;
import innowake.product.base.ui.api.script.PopupScript;
import innowake.product.base.ui.api.util.SelectionUtil;

/**
 * Used to test if the mining server connection is successful.
 */
public class TestMiningServerConnection extends PopupScript {

	@Override
	public void execute(final IPopupExecuteContext context, final IProgressMonitor monitor) throws ApiException {
		@Nullable
		final IProject project = SelectionUtil.getFirstProject(context.getSelection());
		if (project == null) {
			context.err().println("Please select a project");
			return;
		}
		
		try {
			context.out().println("Use eclipse project " + project.getName());
			
			final Optional<ProjectData> apiProject = ConnectionFactory.getApiProject(project);
			if ( ! apiProject.isPresent()) {
				context.err().println("Please ensure the eclipse project is linked to a mining project.");
				return;
			}
			final ProjectData projectData = apiProject.get();
			context.out().println( String.format("Use mining project of client '%s' (%d) with name '%s' (%d).", 
					projectData.getClientName(), projectData.getClientId(), projectData.getProjectName(), projectData.getProjectId()));
			
			final Optional<ConnectionInfo> cInfoOpt = ConnectionFactory.connectionFor(project);
			if ( ! cInfoOpt.isPresent()) {
				context.err().println("Please ensure the mining configuration are available on the selected project.");
				return;
			}
			
			final ConnectionInfo ci = cInfoOpt.get();
			context.out().println("Connecting to " + ci.getUrl());
			
			final Result<Map<String, String>> infoResult = MiningApiClient.infoService(ci).info().execute();
			context.out().println(infoResult.getStatusMessage());
			
		} catch (final StorageException | IOException e) {
			context.err().println(e.getMessage());
			throw new IllegalStateException(e);
		}
	}
}
