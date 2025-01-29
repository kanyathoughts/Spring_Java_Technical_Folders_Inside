/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
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
 * Identifies candidates on the currently selected file.
 */
public class IdentifyCandidatesHandler extends MultiFileBaseHandler {

	private static final String CONFIRMATION_MESSAGE_PATTERN = "Do you really want to execute the Candidate identification on %d files?";

	@Override
	protected Supplier<Result<String>> createResultSupplier(final IProject project, final Long projectId, final List<String> filePaths) {
		return () -> {
			return MiningServiceExecutor
					.create(() -> ApiClient.candidateService(project).identifyAllCandidates().setProjectId(projectId).setModulePaths(filePaths))
					.setExceptionConsumer(exception -> MiningPlugin.getDefaultNonNull().getPluginLog().error("Error while identifying candidates.", exception))
					.getResult();
		};
	}

	@Override
	protected String getConfirmationMessagePattern() {
		return CONFIRMATION_MESSAGE_PATTERN;
	}
}
