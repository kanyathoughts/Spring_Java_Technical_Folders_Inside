/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import innowake.lib.job.api.ProgressMonitor;

import java.util.UUID;

/**
 * keeps the parameters are needed in MonitoredTaskManager
 */
public class MonitoredTaskParameter {

	private UUID monitoredTaskManagerId;
	private ProgressMonitor progressMonitor;
	private final int taskId;

	public MonitoredTaskParameter(final UUID monitoredTaskManagerId, final ProgressMonitor progressMonitor, final int taskId) {
		this.monitoredTaskManagerId = monitoredTaskManagerId;
		this.progressMonitor = progressMonitor;
		this.taskId = taskId;
	}

	/**
	 * The method gets the progressMonitor
	 * @return ProgressMonitor
	 */
	public ProgressMonitor getProgressMonitor() {
		return progressMonitor;
	}

	/**
	 * The method return the taskId
	 * @return  taskId
	 */
	public int getTaskId() {
		return taskId;
	}

	/**
	 * The method returns the UUID of monitorTaskManager
	 * @return UUID of monitorTaskManager
	 */
	public UUID getMonitoredTaskManagerId() {
		return monitoredTaskManagerId;
	}
}
