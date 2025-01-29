/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.job.Message.Severity;

/**
 * Implementation of a {@link TaskExceptionHandler} that will add the error text of the {@link Throwable}
 * as a {@link Message} of type {@link Severity#ERROR} to the {@link Job}.
 *
 * @param <S> the concrete return type of the task
 */
public class ReportMessageExceptionHandler<S extends Serializable> implements TaskExceptionHandler<S> {

	private final JobMonitor jobMonitor;

	/**
	 * Constructor.
	 *
	 * @param jobMonitor the {@link JobMonitor} used to write {@linkplain Message Messages}
	 */
	public ReportMessageExceptionHandler(final JobMonitor jobMonitor) {
		this.jobMonitor = jobMonitor;
	}

	@Override
	public void handle(final String taskId, final Result<S> result) {
		jobMonitor.addMessage(new Message(Message.Severity.ERROR, assertNotNull(result.status.getMessage())));
	}

}
