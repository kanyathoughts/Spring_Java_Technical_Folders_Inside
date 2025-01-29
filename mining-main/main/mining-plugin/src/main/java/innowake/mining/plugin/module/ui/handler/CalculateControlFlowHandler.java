/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import java.util.List;
import java.util.function.Supplier;

import org.eclipse.core.resources.IProject;

import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.base.ui.MultiFileBaseHandler;
import innowake.mining.plugin.client.ApiClient;

/**
 * Used to take one or more selected modules and execute the backend function via a REST call for the control flow calculation.
 */
public class CalculateControlFlowHandler extends MultiFileBaseHandler {

	private static final String CONFIRMATION_MESSAGE_PATTERN = "Do you really want to execute the control flow calculation on %d files?";

	@Override
	protected Supplier<Result<String>> createResultSupplier(final IProject project, final Long projectId, final List<String> filePaths) {
		return () -> {
			return MiningServiceExecutor
					.create(() -> ApiClient.controlFlowService(project).calculateControlFlowGraphs().setProjectId(projectId).setModulePaths(filePaths))
					.setExceptionConsumer(
							exception -> MiningPlugin.getDefaultNonNull().getPluginLog().error("Error while calculating control flow graphs.", exception))
					.getResult();
		};
	}

	@Override
	protected String getConfirmationMessagePattern() {
		return CONFIRMATION_MESSAGE_PATTERN;
	}
	
}
