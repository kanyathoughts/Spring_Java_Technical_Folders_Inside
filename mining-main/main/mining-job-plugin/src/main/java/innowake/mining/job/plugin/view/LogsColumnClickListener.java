/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.job.plugin.view;

import java.io.IOException;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.FileDialog;
import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.plugin.base.JobLogUtil;

/**
 * This click listener will operate on the "Logs" column of the {@link JobView} to enable
 * handling of job logs. As the {@link TreeViewer} doesn't support embedding hyperlinks or
 * buttons, the whole table cell is clickable as a workaround.
 */
public class LogsColumnClickListener extends MouseAdapter {
	
	private final ColumnViewer viewer;
	private final int columnIndex;
	private static final String LOG_EXTENSION = "log";
	private static final String ERROR_TITLE = "Unable to write log file to given location";
	
	private boolean isOffline = true; /* We initially start in offline mode */
	
	/**
	 * Constructor.
	 * 
	 * @param viewer the {@link ColumnViewer} instance
	 * @param columnIndex the column index of the "Logs" column in the {@link JobView}
	 */
	public LogsColumnClickListener(final ColumnViewer viewer, final int columnIndex) {
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
	public void mouseDown(@Nullable final MouseEvent event) {
		/* Only respond to clicks when not in offline mode. */
		if (event != null && !isOffline) {
			final Point point = new Point(event.x, event.y);
			final ViewerCell cell = viewer.getCell(point);
			/* Only respond to clicks in the "Logs" column. */
			if (cell != null && cell.getColumnIndex() == columnIndex) {
				final Rectangle rect = cell.getTextBounds();
				if (rect != null && rect.contains(point) && cell.getElement() instanceof RemoteJobInfo) {
					final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) cell.getElement();
					final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(remoteJobInfo.getProjectName());
					final String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm-ss"));
					final String logFileName = new StringBuilder(timestamp).append("-").append(remoteJobInfo.getJobInfo()
							.getJobId()).append(".").append(LOG_EXTENSION).toString();
					generateLogFile(remoteJobInfo.getJobInfo().getJobId(), project, logFileName);
				}
			}
		}
	}

	private void generateLogFile(final String jobId, final IProject project, final String logFileName) {
		final String projectLocation = project.getLocation().toString();
		final String fileLocation = getFileLocation(logFileName, projectLocation);
		if (StringUtils.isNotEmpty(fileLocation)) {
			final Job job = Job.create("Download log file", monitor -> {
				//TODO
				/* WMIN-12947: Reactivate when saving a zip file works
				try {
					JobLogUtil.saveJobLogAsZipOrAsLog(jobId, project, fileLocation, true);
				} catch (Exception e) {
					MiningJobPlugin.handleError(ERROR_TITLE, e.getCause(), true);
				} */

				final String logFileContent = JobLogUtil.generateLogContent(jobId, project);
				if (StringUtils.isNotEmpty(logFileContent)) {
					try {
						FileUtils.writeStringToFile(Paths.get(fileLocation).toFile(), logFileContent, project.getDefaultCharset());
					} catch (IOException | CoreException e) {
						MiningJobPlugin.handleError(ERROR_TITLE, e.getCause(), true);
					}
				}
			});
			job.schedule();
		}
	}
	
	private static final String getFileLocation(final String logFileName, final String projectLocation) {
		final FileDialog saveDialog = new FileDialog(WorkbenchUtil.getActiveShell(), SWT.SAVE);
		saveDialog.setFilterPath(projectLocation);
		saveDialog.setFileName(logFileName);
		return saveDialog.open();
	}
}