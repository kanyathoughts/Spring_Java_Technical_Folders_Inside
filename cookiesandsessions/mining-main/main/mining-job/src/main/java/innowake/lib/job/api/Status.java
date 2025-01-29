/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.task.Task;

/**
 * Generic status of an operation which can either be a whole {@link Job} or of a single {@link Task}.
 */
public class Status implements Serializable {

	/** {@link Status} with a severity of {@link Severity#OK} */
	public static final Status OK = new Status(Severity.OK);

	private final Severity severity;

	@Nullable
	private final String message;
	@Nullable
	private final String stackTrace;

	/**
	 * Constructor.
	 *
	 * @param severity the {@link Severity} applied to this status
	 */
	public Status(final Severity severity) {
		this.severity = severity;
		message = null;
		stackTrace = null;
	}

	/**
	 * Constructor.
	 *
	 * @param throwable the {@link Throwable} encountered during job or task execution
	 */
	public Status(final Throwable throwable) {
		this.severity = Severity.ERROR;
		message = throwable.getMessage();
		final StringWriter writer = new StringWriter();
		throwable.printStackTrace(new PrintWriter(writer));
		stackTrace = writer.toString();
	}

	/**
	 * @return the {@link Severity}
	 */
	public final Severity getSeverity() {
		return severity;
	}

	/**
	 * @return the message of the {@link Throwable} in case an error has occurred
	 */
	@Nullable
	public final String getMessage() {
		return message;
	}

	/**
	 * @return the stack trace of the {@link Throwable} in case an error has occurred
	 */
	@Nullable
	public final String getStackTrace() {
		return stackTrace;
	}

}
