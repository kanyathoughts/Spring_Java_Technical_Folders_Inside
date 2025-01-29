/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import innowake.lib.job.api.management.JobManager;

/**
 * Base interface for {@linkplain JobManager JobManagers} providing additional internal functionalities.
 */
public interface JobManagerInternal extends JobManager {

	/**
	 * Prepares the provided {@code bean} by executing full injection on it.
	 * 
	 * @param bean the bean to prepare
	 */
	void prepareBean(Object bean);
}
