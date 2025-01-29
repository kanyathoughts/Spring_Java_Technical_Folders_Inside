/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;
 
import org.eclipse.jface.viewers.StyledString;
 
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.JobInformation;
 
/**
 * Label provider for the "Step" column of the {@link JobView} provides the following content:
 * <ul>
 *  <li>For root entries it shows the step description</li>
 * </ul>
 */
public class StepColumnLabelProvider extends AbstractColumnLabelProvider {
 
    @Override
    public StyledString getStyledText(@Nullable final Object element) {
        if (element instanceof RemoteJobInfo) {
            final RemoteJobInfo remoteJobInfo = (RemoteJobInfo) element;
            final JobInformation jobInfo = remoteJobInfo.getJobInfo();
            final String stepDescription = jobInfo.getStepDescription();
            return new StyledString(stepDescription != null ? stepDescription : "");
        }
        return new StyledString();
    }
}