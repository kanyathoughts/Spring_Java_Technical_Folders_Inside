/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import innowake.lib.job.api.ProgressMonitor;

/**
 * Base interface for {@link ProgressMonitor} implementations providing additional internal functionalities.
 */
public interface ProgressMonitorInternal extends ProgressMonitor {

	/**
	 * Initializes the {@link ProgressMonitor}, which also runs full injection on it.
	 * 
	 * @param jobManager the {@link JobManagerInternal}
	 */
	void initialize(JobManagerInternal jobManager);
}
