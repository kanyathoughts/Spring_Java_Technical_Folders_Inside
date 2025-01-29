/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Logging facade for mining plugin which logs to the default eclipse error log.
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
	 * Logs the message with the severity {@code WARNING}.
	 *
	 * @param message the message to log
	 * @param throwable the throwable to log 
	 */
	public static void warn(final String message, final Throwable throwable) {
		log(IStatus.WARNING, message, throwable);
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
		MiningPlugin.getDefaultNonNull().getLog().log(new Status(severity, MiningPlugin.ID, message));
	}
	
	private static void log(final int severity, final String message, final Throwable throwable) {
		MiningPlugin.getDefaultNonNull().getLog().log(new Status(severity, MiningPlugin.ID, message, throwable));
	}
	
}
