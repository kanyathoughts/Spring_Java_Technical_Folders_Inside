/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.progress;

import org.eclipse.core.runtime.IProgressMonitor;

import innowake.lib.core.api.lang.Nullable;

/**
 * This decorates a {@link IProgressMonitor} for testing purposes, to check
 * if specific values have been set on it.
 */
public class ProgressMonitorDecorator implements IProgressMonitor {
	
	private final IProgressMonitor monitor;
	
	@Nullable
	private String lastSubTaskName;
	private int lastTotalWork;
	private int lastWorked;
	
	/**
	 * Constructor.
	 * 
	 * @param monitor the original {@link IProgressMonitor}
	 */
	public ProgressMonitorDecorator(final IProgressMonitor monitor) {
		this.monitor = monitor;
	}

	@Override
	public void beginTask(@Nullable final String name, final int totalWork) {
		monitor.beginTask(name, totalWork);
		this.lastTotalWork = totalWork;
	}

	@Override
	public void done() {
		monitor.done();
	}

	@Override
	public void internalWorked(final double work) {
		monitor.internalWorked(work);
	}

	@Override
	public boolean isCanceled() {
		return monitor.isCanceled();
	}

	@Override
	public void setCanceled(final boolean value) {
		monitor.setCanceled(value);
	}

	@Override
	public void setTaskName(@Nullable final String name) {
		monitor.setTaskName(name);
	}

	@Override
	public void subTask(@Nullable final String name) {
		monitor.subTask(name);
		this.lastSubTaskName = name;
	}

	@Override
	public void worked(final int work) {
		monitor.worked(work);
		this.lastWorked = work;
	}
	
	/**
	 * @return the last set sub-task name
	 */
	@Nullable
	public String getLastSubTaskName() {
		return lastSubTaskName;
	}
	
	/**
	 * @return the last set amount of total work units
	 */
	public int getLastTotalWork() {
		return lastTotalWork;
	}
	
	/**
	 * @return the last set amount of worked units
	 */
	public int getLastWorked() {
		return lastWorked;
	}

}
