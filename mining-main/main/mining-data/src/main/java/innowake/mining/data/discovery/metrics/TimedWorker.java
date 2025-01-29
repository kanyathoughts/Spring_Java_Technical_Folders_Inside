/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.discovery.metrics;

import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.TimeUnit;

import innowake.lib.core.lang.Assert;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * A worker to execute long running tasks with the possibility to set a timeout 
 * and cancel the task via a monitor.</br>
 * 
 * <b>Note:</b> Every job worker thread is supposed to have its own instance of this class!
 */
public interface TimedWorker {

	/**
	 * Exception thrown during execution of the worker.
	 */
	public class WorkerException extends Exception {

		/**
		 * Construct a worker exception.
		 *
		 * @param message the detail message
		 */
		public WorkerException(final String message) {
			super(message);
		}

		/**
		 * Construct a worker exception.
		 *
		 * @param message the detail message
		 * @param cause the original cause
		 */
		public WorkerException(final String message, final Throwable cause) {
			super(message, cause);
		}

	}

	/**
	 * Exception being thrown when the execution of a task has been cancelled due to a {@link CancellationException}.
	 */
	public class WorkerCancellationException extends WorkerException {

		/**
		 * Constructor.
		 *
		 * @param message the detail message
		 */
		public WorkerCancellationException(final String message) {
			super(message);
		}

	}

	/**
	 * A provider for error messages. Can be used to give context
	 * information to a message.
	 */
	@FunctionalInterface
	public interface MessageProvider {

		/**
		 * Constructs a message from a given cause.
		 *
		 * @param cause the original cause
		 * @return the adapted error message
		 */
		String get(final String cause);

		/**
		 * Creates a message provider that adds context information
		 * about a {@link ModelArtifact}.
		 *
		 * @param artifact the context
		 * @return a message provider
		 */
		static MessageProvider from(final ModelArtifact artifact) {
			return cause -> String.format(
					"[%s (ID:%s)] %s. While parsing %s.",
					artifact.getPath().orElse(artifact.getName() + "<no path available>"),
					Assert.assertNotNull(artifact.getModuleId()),
					cause,
					artifact.getType()
			);
		}

		/**
		 * Creates a message provider that adds context information
		 * about an source object and a {@link ResolveTarget}.
		 *
		 * @param sourceObject context file
		 * @param type the type of the context file
		 * @return a message provider
		 */
		static MessageProvider from(final SourcePojo sourceObject, final ResolveTarget type) {
			return cause -> String.format(
					"[%s] %s. While parsing %s.",
					sourceObject.getPath(),
					cause,
					type
			);
		}

		/**
		 * Creates a message provider that adds context information
		 * about a {@link ResolveTarget}.
		 *
		 * @param type the context type
		 * @return a message provider
		 */
		static MessageProvider from(final ResolveTarget type) {
			return cause -> String.format(
					"%s. While parsing %s.",
					cause,
					type
			);
		}

		/**
		 * Returns a default message provider which just returns
		 * the cause.
		 *
		 * @return a message provider
		 */
		static MessageProvider defaultProvider() {
			return cause -> cause;
		}
	}

	/**
	 * Executes a given task.
	 *
	 * @param <T> the type of the task's result
	 * @param task the given task to be executed.
	 * @param timeout the timeout; if negative the task is executed synchronously
	 *               in the same thread as the caller
	 * @param unit the unit of the timeout
	 * @param message provider to be used to construct error messages
	 * @return the result of the task
	 * @throws TimedWorker.WorkerException if the task couldn't be completed normally
	 */
	public <T> T execute(final Callable<T> task, final long timeout, final TimeUnit unit, final TimedWorker.MessageProvider message) throws WorkerException;

	/**
	 * Shuts down this worker. After shutting down, this worker accepts no more
	 * requests for execution.
	 */
	public void shutdown();
}
