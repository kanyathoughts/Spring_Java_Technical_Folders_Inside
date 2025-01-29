/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.command;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.deeplink.EclipseApi;
import innowake.mining.plugin.deeplink.EclipseApiException;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.ModuleLocation;

/**
 * This class takes care of executing parsed commands
 */
public class Executor {
	private Executor() {}
	
	/**
	 * Executes the given command.
	 *
	 * @param request the command to execute.
	 * @throws RequestExecutionException if an error happens, while executing the command
	 */
	public static void execute(final Request request) throws RequestExecutionException {
		switch(request.getCommandType()) {
			case SHOW_MODULE:
				executeShowModule((ShowModuleRequest) request);
				break;
			case SHOW_ANNOTATION:
				executeShowAnnotation((ShowAnnotationRequest) request);
				break;
			default:
				Logging.warn("Received an unknown request: " + request);
				break;
		}
	}
	
	/**
	 * Executes a command of the type SHOW_ANNOTATION. 
	 * For each mining project matching the project if, it fetches the module location and reference.
	 *
	 * @param request The command containing the necessary parameters.
	 * @throws RequestExecutionException if an error occurs executing the command.
	 */
	public static void executeShowAnnotation(final ShowAnnotationRequest request) throws RequestExecutionException {
		final List<IProject> projects = EclipseApi.getMiningProjects(request.getProjectId());
		for (final IProject project : projects) {
			final ConnectionInfo connectionInfo;
			connectionInfo = MiningPreferences.getConnectionInfo(project).orElseThrow(RequestExecutionException::new);
			final Optional<AnnotationPojo> annotationRes;
			try {
				annotationRes = MiningServiceExecutor
						.create(() ->
								ApiClient.annotationService(connectionInfo)
								.findAnnotationById()
								.setProjectId(request.getProjectId())
								.setAnnotationId(request.getId()))
						.execute();
			} catch (final ExecutionException executionException) {
				/* In case exception consumer is specified for handling some other exception with the above MiningServiceExecutor in future,
				 * then executionException must be thrown from the consumer to prevent IOException from being consumed. */
				if (executionException.getCause() instanceof IOException) {
					throw new RequestExecutionException("Failed to fetch annotation." , executionException.getCause());	
				}
				throw executionException;
			}
			
			annotationRes.ifPresent(annotation -> {
				final Optional<ModulePojo> moduleRes = MiningServiceExecutor
							.create(() ->
									ApiClient.moduleService(connectionInfo)
									.findModuleById()
									.setProjectId(request.getProjectId())
									.setModuleId(annotation.getModule()))
							.setExceptionConsumer(exception -> {
								throw new IllegalStateException(exception);
							})
							.execute();

				moduleRes.ifPresent(module -> openAnnotation(annotation.getLocation(), module, project));
			});
		}
	}
	
	private static void openAnnotation(final Optional<ModuleLocation> location, final ModulePojo module, final IProject project) {
		final int offset = location.isPresent() ? location.get().getOffset().intValue() : 0;
		final int length = location.isPresent() ? location.get().getLength().intValue() : 0;

		final IFile file = project.getFile(module.getPath().orElse(null));
		try {
			EclipseApi.openInternalFile(file, offset, length);
		} catch (final EclipseApiException e) {
			throw new IllegalStateException(e);
		}
	}
	
	/**
	 * Executes a SHOW_MODULE command. It fetches the module location and attempts to open the file.
	 *
	 * @param request Command containing the necessary parameters
	 */
	public static void executeShowModule(final ShowModuleRequest request) {
		final List<IProject> projects = EclipseApi.getMiningProjects(request.getProjectId());
		for (final IProject project : projects) {
			final String path = request.getPath();
			if (path == null) {
				Logging.error("Invalid request: No path provided. " + request.toString());
				continue;
			}
			final IFile file = project.getFile(path);
			try {
				EclipseApi.openInternalFile(file, 0, 0);
			} catch (final EclipseApiException e) {
				Logging.error("Failed to open file " + file.toString(), e);
			}
		}
	}

}
