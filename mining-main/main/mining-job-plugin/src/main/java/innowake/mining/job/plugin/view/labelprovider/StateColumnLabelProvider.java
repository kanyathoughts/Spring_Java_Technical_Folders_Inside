/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Label provider for the "State" column of the {@link JobView} showing the current state of the job preceded by a matching icon.
 */
public class StateColumnLabelProvider extends AbstractColumnLabelProvider {

	private static final ImageDescriptor WAITING_IMAGE = createImageDescriptor("waiting");
	private static final ImageDescriptor RUNNING_IMAGE = createImageDescriptor("progress");
	private static final ImageDescriptor CANCELED_IMAGE = createImageDescriptor("canceled");
	private static final ImageDescriptor ERROR_IMAGE = createImageDescriptor("error");
	private static final ImageDescriptor SUCCESS_IMAGE = createImageDescriptor("success");

	@Override
	public StyledString getStyledText(@Nullable final Object element) {
		if (element instanceof RemoteJobInfo) {
			final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
			final JobInformation jobInfo = remoteJobInfo.getJobInfo();
			return getStyledString(getStatusText(jobInfo), jobInfo.getStatus());
		}
		return new StyledString();
	}

	@Override
	public Image getImage(@Nullable final Object element) {
		if ( ! (element instanceof RemoteJobInfo)) {
			return super.getImage(element);
		}
		
    	final JobStatus status = ((RemoteJobInfo) element).getJobInfo().getStatus();
    	final ImageDescriptor imageDescriptor;
		switch (status) {
			case CANCEL_REQUESTED:
			case RUNNING:
				imageDescriptor = RUNNING_IMAGE;
				break;
			case CANCELED:
				imageDescriptor = CANCELED_IMAGE;
				break;
			case FAILURE:
			case TIMEOUT:
				imageDescriptor = ERROR_IMAGE;
				break;
			case SUCCESS:
				imageDescriptor = SUCCESS_IMAGE;
				break;
			default:
				imageDescriptor = WAITING_IMAGE;
				break;
		}
    	
		return createImage(imageDescriptor, status);
	}
	
	private String getStatusText(final JobInformation jobInfo) {
		final JobStatus status = jobInfo.getStatus();
		switch (status) {
			case CANCEL_REQUESTED:
				return "Canceling";
			case CANCELED:
				return "Canceled";
			case RUNNING:
				return "Running";
			case FAILURE:
				return "Error";
			case SCHEDULED:
				return "Scheduled";
			case SUCCESS:
				return "Success";
			case TIMEOUT:
				return "Timeout";
			case UNKNOWN:
				return "Unknown";
			default:
				return status.toString();
		}
	}
}
