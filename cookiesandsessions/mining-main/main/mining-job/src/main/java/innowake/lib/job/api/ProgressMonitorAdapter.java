/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import innowake.lib.core.IProgress;
import innowake.lib.core.api.lang.Nullable;

public class ProgressMonitorAdapter implements IProgress{

	private final ProgressMonitor progressMonitor;
	
	/**
	 * @param progressMonitor the progressMonitor to adapt between
	 */
	public ProgressMonitorAdapter(final ProgressMonitor progressMonitor) {
		this.progressMonitor = progressMonitor;
	}
	
	@Override
	public void beginTask(@Nullable final String name, final int totalWork) {
		if(name != null) {			
			progressMonitor.setStepDescription(name);
		} else {
			progressMonitor.setStepDescription("Unnamed");
		}
		progressMonitor.begin(totalWork);
	}

	@Override
	public IProgress createSubProgress(final int ticks) {
		return new ProgressMonitorAdapter(progressMonitor.subMonitor(ticks));
	}

	@Override
	public void done() {
		/* Not implemented for ProgressMonitor*/
	}

	@Override
	public boolean isCanceled() {
		return progressMonitor.isCanceled();
	}

	@Override
	public void setCanceled(final boolean canceled) {
		if(canceled) {			
			progressMonitor.cancel();
		}
	}

	@Override
	public void subTask(@Nullable final String name) {
		if(name != null) {			
			progressMonitor.setStepDescription(name);
		} else {
			progressMonitor.setStepDescription("Unnamed");
		}
	}

	@Override
	public void worked(final int work) {
		progressMonitor.worked(work);
	}
	
	
}
