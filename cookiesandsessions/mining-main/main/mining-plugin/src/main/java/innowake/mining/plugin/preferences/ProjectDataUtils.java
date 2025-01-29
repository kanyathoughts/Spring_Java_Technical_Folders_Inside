/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.eclipse.jface.dialogs.DialogPage;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Utilities for {@link ProjectData}
 */
public class ProjectDataUtils {

	/**
	 * Obtains the {@link ProjectData} from a specified {@link ConnectionInfo}
	 *
	 * @param connectionInfo the connection details to obtain the {@code ProjectData} from
	 * @param page the {@code DialogPage} used to output error messages if they occur
	 * @return a list with the {@code ProjectData}
	 */
	public static List<ProjectData> getProjectData(final ConnectionInfo connectionInfo, DialogPage page) {
		final List<ProjectData> projectViews = new ArrayList<>();
		final Optional<ClientPojo[]> clients = MiningServiceExecutor
				.create(() -> ApiClient.clientService(connectionInfo).findAllClients())
				.setInvalidResultConsumer(invalidResult -> {
					page.setErrorMessage(invalidResult.getStatusMessage());
					Logging.error(invalidResult.getExtendedStatusMessage());
				})
				.setExceptionConsumer(exception -> {
					page.setErrorMessage(exception.getLocalizedMessage());
					Logging.error(exception.getLocalizedMessage(), exception);
				})
				.execute();
		final Optional<ProjectPojo[]> projects = MiningServiceExecutor
				.create(() -> ApiClient.projectService(connectionInfo).findAllProjects())
				.setInvalidResultConsumer(invalidResult -> {
					page.setErrorMessage(invalidResult.getStatusMessage());
					Logging.error(invalidResult.getExtendedStatusMessage());
				})
				.setExceptionConsumer(exception -> {
					page.setErrorMessage(exception.getLocalizedMessage());
					Logging.error(exception.getLocalizedMessage(), exception);
				})
				.execute();
		
		if ( ! clients.isPresent() || ! projects.isPresent()) {
			return projectViews;
		}

		final Map<Long, ClientPojo> clientMap = new HashMap<>();
		Arrays.stream(clients.get())
			  .filter(client -> client.getId().longValue() != 0)
			  .forEach(client -> clientMap.put(client.getId(), client));
		
		for (final ProjectPojo project : projects.get()) {
			final ClientPojo client = clientMap.get(project.getClientNid());
			if (client != null) {
				projectViews.add(new ProjectData(project, client));
			}
		}
		return projectViews;
	}
}
