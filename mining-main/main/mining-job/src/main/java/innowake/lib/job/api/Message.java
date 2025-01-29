/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import java.io.Serializable;

import innowake.lib.job.api.task.Task;

/**
 * A message that has been written during the execution of a {@link Job} or {@link Task}.
 * @deprecated Use {@link innowake.mining.shared.model.job.Message} instead. Required for DB migration from OrientDB to Postgres.
 * Will be removed in the near future.
 */
@Deprecated(forRemoval = true) /* used for schema migration from Orient to Postgres */
public class Message implements Serializable {
	
	private static final long serialVersionUID = 1L;

	/**
	 * The message severity.
	 */
	public enum Severity {
		INFO,
		WARNING,
		ERROR;
	}
	
	private final Severity severity;
	private final String text;
	
	/**
	 * Constructor.
	 * 
	 * @param severity the {@link Severity} of the message
	 * @param text the actual message text
	 */
	public Message(final Severity severity, final String text) {
		this.severity = severity;
		this.text = text;
	}

	/**
	 * @return the {@link Severity} of the message
	 */
	public Severity getSeverity() {
		return severity;
	}

	/**
	 * @return the text of the message
	 */
	public String getText() {
		return text;
	}
}
