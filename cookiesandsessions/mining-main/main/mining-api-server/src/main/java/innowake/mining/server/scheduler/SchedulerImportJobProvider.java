/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.scheduler;

import java.io.Serializable;

import innowake.mining.server.job.ControlMImportJob;
import innowake.mining.server.job.SchedulerImportJob;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerType;

/**
 * Provides the appropriate {@link SchedulerImportJob} for the given {@link SchedulerType}.
 */
public class SchedulerImportJobProvider {

	private final SchedulerImportPojoPrototype prototype;

	public SchedulerImportJobProvider(final SchedulerImportPojoPrototype prototype) {
		this.prototype = prototype;
	}

	/**
	 * Returns the appropriate {@link SchedulerImportJob} for the given {@link SchedulerType}.
	 *
	 * @param schedulerType the {@link SchedulerType}
	 * @return the {@link SchedulerImportJob}
	 */
	public SchedulerImportJob<? extends Serializable> getJob(final SchedulerType schedulerType) {
		switch (schedulerType) {
			case CONTROL_M:
				return new ControlMImportJob(prototype);
			case CA7:
				throw new UnsupportedOperationException("CA7 is not supported yet");
			default:
				throw new IllegalArgumentException("Unknown scheduler type: " + schedulerType);
		}
	}

}
