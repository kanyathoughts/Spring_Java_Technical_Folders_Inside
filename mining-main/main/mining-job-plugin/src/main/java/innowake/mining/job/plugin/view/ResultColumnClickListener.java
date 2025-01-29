/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view;

import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.shared.model.job.ResultStatus;

/**
 * This click listener will operate on the "Result" column of the {@link JobView} to enable
 * handling of job results. As the {@link TreeViewer} doesn't support embedding hyperlinks or
 * buttons, the whole table cell is clickable as a workaround.
 */
public class ResultColumnClickListener extends MouseAdapter {
	
	private final ColumnViewer viewer;
	private final int columnIndex;
	
	private boolean isOffline = true; /* We initially start in offline mode */
	
	/**
	 * Constructor.
	 * 
	 * @param viewer the {@link ColumnViewer} instance
	 * @param columnIndex the column index of the "Result" column in the {@link JobView}
	 */
	public ResultColumnClickListener(final ColumnViewer viewer, final int columnIndex) {
		this.viewer = viewer;
		this.columnIndex = columnIndex;
	}
	
	/**
	 * Sets if in offline mode, which leads to this click listener to not respond to any clicks.
	 * 
	 * @param isOffline {@code true} for offline mode; {@code false} otherwise
	 */
	public void setOffline(final boolean isOffline) {
		this.isOffline = isOffline;
	}
	
	@Override 
	public void mouseDown(@Nullable final MouseEvent e) {
		/* Only respond to clicks when not in offline mode. */
		if (e != null && ! isOffline) {
			final Point point = new Point(e.x, e.y);
			final ViewerCell cell = viewer.getCell(point);
			
			/* Only respond to clicks in the "Result" column. */
			if (cell != null && cell.getColumnIndex() == columnIndex) {
				final Rectangle rect = cell.getTextBounds();
				if (rect != null && rect.contains(point) && cell.getElement() instanceof RemoteJobInfo) {
					final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) cell.getElement();
					final ResultStatus resultStatus = remoteJobInfo.getJobInfo().getResultStatus();
					
					/* Only respond to clicks if the job actually has a result to be handled. */
					if (resultStatus != null && resultStatus.hasCollectableResult()) {
						MiningJobPlugin.getDefaultNonNull().getRemoteJobManager().handleJobResult(remoteJobInfo);
					}
				}
			}
		}
	}
}
