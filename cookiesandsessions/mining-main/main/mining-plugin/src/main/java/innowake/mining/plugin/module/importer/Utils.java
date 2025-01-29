/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import java.io.IOException;
import java.util.Optional;
import java.util.function.Function;

import org.eclipse.core.runtime.SubMonitor;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;

/**
 * Utility class for importing Excel.
 */
final class Utils {
	
	private static final String INVALID_RESULT_MESSAGE_PREFIX = "InvalidResult.extendedStatusMessage:";
	
	private Utils() {}

	/**
	 * Returns true if there were already modules on the server and the user does not want to overwrite those. 
	 *
	 * @param existingModulesHandler function which controls the overwriting of existing modules in the backend
	 * @param connectionInfo the connection info of the mining backend
	 * @param projectId the ID of the project the import should be associated with
	 * @param monitor the progress monitor
	 * @return true if there were already modules on the server and the user does not want to overwrite those
	 * @throws IOException in case of an error while service call
	 * @throws ValidationException in case a service returns an invalid result
	 */
	static boolean checkForCancelOnExistingModules(
			final Function<Long, Boolean> existingModulesHandler, 
			final ConnectionInfo connectionInfo, 
			final Long projectId, 
			final SubMonitor monitor) throws IOException, ValidationException {
		
		monitor.setTaskName("Count existing modules");
		final Optional<Long> value = getModuleCount(connectionInfo, projectId);
		monitor.split(1);
		
		if (value.isPresent() && value.get().longValue() != 0) {
			if (existingModulesHandler.apply(value.get()).booleanValue()) {
				monitor.setTaskName("Deleting existing modules");
				deleteAllModules(connectionInfo, projectId);
				monitor.split(1);
			} else {
				return true;
			}
		}

		return false;
	}

	private static Optional<Long> getModuleCount(
			final ConnectionInfo connectionInfo, 
			final Long projectId) throws IOException, ValidationException {
		try {
			return MiningServiceExecutor
					.create(() ->
							ApiClient.moduleService(connectionInfo)
							.getModuleCount()
							.setProjectId(projectId))
					.setInvalidResultConsumer(invalidResult -> {
						throw new IllegalStateException(INVALID_RESULT_MESSAGE_PREFIX + invalidResult.getExtendedStatusMessage());
					})
					.execute();
		} catch (final IllegalStateException illegalStateException) {
			extractAndThrowFromIllegalStateException(illegalStateException, INVALID_RESULT_MESSAGE_PREFIX, "Count Retrieval Error",
					"Could not determine the number of modules on the server\n\n");
		} catch (final ExecutionException executionException) {
			/* In case exception consumer is specified for handling some other exception with the above MiningServiceExecutor in future,
			 * then executionException must be thrown from the consumer to prevent IOException from being consumed. */
			extractAndThrowFromExecutionException(executionException);
		}
		return Optional.empty();
	}

	private static void deleteAllModules(
			final ConnectionInfo connectionInfo, 
			final Long projectId) throws IOException, ValidationException {
		try {
			MiningServiceExecutor
					.create(() ->
							ApiClient.moduleService(connectionInfo)
							.deleteAllModules()
							.setProjectId(projectId))
					.setInvalidResultConsumer(invalidResult -> {
						throw new IllegalStateException(INVALID_RESULT_MESSAGE_PREFIX + invalidResult.getExtendedStatusMessage());
					})
					.execute();
		} catch (final IllegalStateException illegalStateException) {
			extractAndThrowFromIllegalStateException(illegalStateException, INVALID_RESULT_MESSAGE_PREFIX, "Deletion Error",
					"Could not delete modules for project:\n%s");
		} catch (final ExecutionException executionException) {
			/* In case exception consumer is specified for handling some other exception with the above MiningServiceExecutor in future,
			 * then executionException must be thrown from the consumer to prevent IOException from being consumed. */
			extractAndThrowFromExecutionException(executionException);
		}
	}
	
	private static void extractAndThrowFromExecutionException(final ExecutionException executionException) throws IOException {
		if (executionException.getCause() instanceof IOException) {
			throw (IOException) executionException.getCause();
		}
		throw executionException;
	}
	
	private static String extractAndThrowFromIllegalStateException(final IllegalStateException illegalStateException, final String invalidResultMessagePrefix,
			final String validationTitle, final String validationMessage) throws ValidationException {
		if (illegalStateException.getMessage().startsWith(invalidResultMessagePrefix)) {
			final String invalidResultExtendedStatusMessage = illegalStateException.getMessage().replace(invalidResultMessagePrefix, "");
			throw new ValidationException(validationTitle, validationMessage + invalidResultExtendedStatusMessage);
		}
		throw illegalStateException;
	}
}
