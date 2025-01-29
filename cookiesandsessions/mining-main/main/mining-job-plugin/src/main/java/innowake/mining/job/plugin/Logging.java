/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Logging facade for mining job plugin which logs to the default eclipse error log.
 */
public class Logging {

	private Logging() {}
	
	/**
	 * Logs the message with severity {@code INFO}.
	 *
	 * @param message the log message
	 */
	public static void info(final String message) {
		log(IStatus.INFO, message);
	}
	
	/**
	 * Logs the object with severity {@code INFO}.
	 * 
	 * The {@link #toString()} method of the object is used as message.
	 *
	 * @param object the object to log
	 */
	public static void info(final Object object) {
		info(object.toString());
	}
	
	/**
	 * Logs the message with the severity {@code WARNING}.
	 *
	 * @param message the message to log
	 */
	public static void warn(final String message) {
		log(IStatus.WARNING, message);
	}
	
	/**
	 * Logs the message with severity {@code ERROR}.
	 *
	 * @param message the log message
	 */
	public static void error(final String message) {
		log(IStatus.ERROR, message);
	}
	
	/**
	 * Logs the message with severity {@code ERROR}.
	 *
	 * @param message the log message
	 * @param throwable the throwable to log
	 */
	public static void error(final String message, final Throwable throwable) {
		log(IStatus.ERROR, message, throwable);
	}
	
	private static void log(final int severity, final String message) {
		MiningJobPlugin.getDefaultNonNull().getLog().log(new Status(severity, MiningJobPlugin.ID, message));
	}
	
	private static void log(final int severity, final String message, final Throwable throwable) {
		MiningJobPlugin.getDefaultNonNull().getLog().log(new Status(severity, MiningJobPlugin.ID, message, throwable));
	}
	
}
