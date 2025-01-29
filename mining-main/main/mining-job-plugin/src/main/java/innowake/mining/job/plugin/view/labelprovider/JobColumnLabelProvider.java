/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.job.plugin.view.JobViewContentProvider.ParentAwareMessage;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.job.Message.Severity;

/**
 * Label provider for the "Job" column of the {@link JobView} provides the following content:
 * <ul>
 * 	<li>For root entries it shows the job description (or job Id if no description is available) and an icon matching the highest severity job message.</li>
 * 	<li>For child message entries it shows an icon matching the severity of the message, followed by the message text.</li>
 * </ul>
 */
public class JobColumnLabelProvider extends AbstractColumnLabelProvider {

	private static final ImageDescriptor INFO_IMAGE_DESCRIPTOR = createImageDescriptor("info");
	private static final ImageDescriptor WARNING_IMAGE_DESCRIPTOR = createImageDescriptor("warning");
	private static final ImageDescriptor ERROR_IMAGE_DESCRIPTOR = createImageDescriptor("error");
	
	@Override
	public StyledString getStyledText(@Nullable final Object element) {
		if (element instanceof RemoteJobInfo) {
			final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
			final JobInformation jobInfo = remoteJobInfo.getJobInfo();
			final String jobDescription = jobInfo.getJobDescription();
			return getStyledString(jobDescription != null ? jobDescription : "Remote job '" + jobInfo.getJobId() + "'", jobInfo.getStatus());
		} else if (element instanceof ParentAwareMessage) {
			final ParentAwareMessage message = (ParentAwareMessage) element;
			return getStyledString(message.getText(), message.getParent().getJobInfo().getStatus());
		}
		return new StyledString();
	}

	@Override
	public Image getImage(@Nullable final Object element) {
		if (element instanceof RemoteJobInfo) {
			final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
			final JobInformation jobInfo = remoteJobInfo.getJobInfo();
			final List<Message> messages = jobInfo.getMessages();
			final Severity highestSeverity = Severity.getHighest(messages.stream().map(Message::getSeverity).collect(Collectors.toList()));
			
			if (highestSeverity != null) {
				return createImage(getImageDescriptor(highestSeverity), jobInfo.getStatus());
			}
		} else if (element instanceof ParentAwareMessage) {
			final ParentAwareMessage message = (ParentAwareMessage) element;
			return createImage(getImageDescriptor(message.getSeverity()), message.getParent().getJobInfo().getStatus());
		}

		return super.getImage(element);
	}
	
	private ImageDescriptor getImageDescriptor(final Severity severity) {
		switch (severity) {
			case INFO:
				return INFO_IMAGE_DESCRIPTOR;
			case WARNING:
				return WARNING_IMAGE_DESCRIPTOR;
			case ERROR:
				return ERROR_IMAGE_DESCRIPTOR;
			default:
				throw new IllegalArgumentException("Unsupported severity: " + severity.name());
		}
	}

}
