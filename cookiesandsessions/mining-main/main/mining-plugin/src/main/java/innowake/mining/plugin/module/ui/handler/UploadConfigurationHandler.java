/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import com.google.gson.JsonSyntaxException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.module.importer.DiscoveryConfigurationsImporter;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;
import innowake.mining.shared.discovery.config.ConfigResources;

/**
 * Uploads discovery configurations to the Mining server.
 */
public class UploadConfigurationHandler extends AbstractBaseHandler {
	
	private static final String SEARCH_ORDER = "discovery-search-order.xml";
	private final DiscoveryConfigurationsImporter configImporter;

	/**
	 * Constructor added for the purpose of tests in mining-test-plugin, please refrain from using it.
	 * @param configurationImporter object to be set. 
	 */
	public UploadConfigurationHandler(final DiscoveryConfigurationsImporter configurationImporter) {
		this.configImporter = configurationImporter;
	}

	public UploadConfigurationHandler() {
		this.configImporter = new DiscoveryConfigurationsImporter();
	}
	
	@Nullable
	@Override
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final IProject project;
		try {
			project = getProject();
		} catch (final ValidationException e) {
			Logging.error("Error while fetching the IProject information : " + e.getLocalizedMessage());
			throw new ExecutionException(e.getLocalizedMessage());
		}
		upload(project);
		
		return null;
	}
	
	/**
	 * Uploads config files.
	 * 
	 * @param project the selected {@link IProject}.
	 */
	public void upload(final IProject project) {
		final ConnectionInfo connectionInfo;
		final Long projectId;
		try {
			connectionInfo = MiningPreferences.getConnectionInfo(project).orElseThrow(
					() -> new ValidationException("Configuration error", String.format("API server for project '%s' not configured", project.getName())));
			final ProjectData projectData = MiningPreferences.getApiProject(project)
					.orElseThrow(() -> new ValidationException("Project data not found for %s ", project.getName()));
			projectId = projectData.getProjectId();
		} catch (final ValidationException e) {
			Logging.error("Error occured while fetching configuration/project info", e);
			return;
		}
		
		final List<String> configFiles = Arrays.stream(ConfigResources.values())
												.map(ConfigResources::getResourceName)
												.collect(Collectors.toList());
		configFiles.add(SEARCH_ORDER);
		
		try {
			final IResource[] members = project.members();
			final List<IFile> iFiles = configFiles.stream()
													.flatMap(file -> findFile(members, file).stream())
													.filter(Objects::nonNull)
													.collect(Collectors.toList());
			
			if (iFiles.isEmpty()) {
				Logging.error("Cannot determine files from current selection");
				return;
			}
			configImporter.setConnectionInfo(connectionInfo);
			configImporter.setProjectId(projectId);
			configImporter.setFiles(iFiles);
			
			configImporter.uploadConfigurations();
		} catch (final CoreException | JsonSyntaxException e) {
			throw new IllegalStateException(e);
		}
	
	}
	
	private List<IFile> findFile(final IResource[] members, final String name) {
		final List<IFile> result = new ArrayList<>();
		for (final IResource member : members) {
			try {
				final List<IFile> files = ResourceUtil.getFilesRecursively(member).stream()
						.filter(file -> file.getName().equalsIgnoreCase(name))
						.collect(Collectors.toList());
				result.addAll(files);
			} catch (final CoreException e) {
				Logging.error("Error occured while finding files", e);
				throw new IllegalStateException(e);
			}
		}
		return result;
	}
}
