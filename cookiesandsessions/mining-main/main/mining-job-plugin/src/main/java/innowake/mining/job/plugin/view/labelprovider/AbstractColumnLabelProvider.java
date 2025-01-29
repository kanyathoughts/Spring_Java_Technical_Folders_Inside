/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import java.net.URL;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.resource.LocalResourceManager;
import org.eclipse.jface.resource.ResourceManager;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.view.JobView;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Abstract base implementation for all {@link LabelProvider}s used by the {@link JobView}.
 */
public abstract class AbstractColumnLabelProvider extends LabelProvider implements IStyledLabelProvider {
	
	private static final Bundle BUNDLE = Platform.getBundle(MiningJobPlugin.ID);
	
	@Nullable
	private ResourceManager resourceManager;
	protected boolean isOffline = true; /* We initially start in offline mode */
	
	/**
	 * Creates an {@link ImageDescriptor} based on the provided {@code imageName}.
	 * 
	 * @param imageName the name of the image without suffix
	 * @return the {@link ImageDescriptor}
	 */
	protected static ImageDescriptor createImageDescriptor(final String imageName) {
		final URL url = BUNDLE.getResource("icons/" + imageName + ".png");
        return ImageDescriptor.createFromURL(url);
    }
	
	@Override
	public void dispose() {
		if (resourceManager != null) {
			resourceManager.dispose();
			resourceManager = null;
		}
	}
	
	/**
	 * Sets if in offline mode, which leads to every label being printed by the {@link GreyTextStyler}.
	 * 
	 * @param isOffline {@code true} for offline mode; {@code false} otherwise
	 */
	public void setOffline(final boolean isOffline) {
		this.isOffline = isOffline;
	}

	/**
	 * Returns a {@link StyledString} with the provided {@code text}. If the job status is
	 * {@link JobStatus#CANCEL_REQUESTED} or {@link JobStatus#CANCELED} or if in offline mode,
	 * then the {@link GreyTextStyler} will be used.
	 *
	 * @param text the text of the label
	 * @param status the {@link JobStatus}
	 * @return the {@link StyledString}
	 */
	protected StyledString getStyledString(final String text, final JobStatus status) {
		Styler styler = null;
		if (status == JobStatus.CANCEL_REQUESTED || status == JobStatus.CANCELED || isOffline) {
			styler = GreyTextStyler.INSTANCE;
		}
		
		return new StyledString(text, styler);
	}
	
	/**
	 * Creates an image with the provided {@code imageDescriptor}. If the job status is
	 * {@link JobStatus#CANCEL_REQUESTED} or {@link JobStatus#CANCELED} or if in offline mode,
	 * then the image will be displayed in grayscale.
	 * 
	 * @param imageDescriptor the {@link ImageDescriptor}
	 * @param status the {@link JobStatus}
	 * @return the {@link Image}
	 */
	protected Image createImage(final ImageDescriptor imageDescriptor, final JobStatus status) {
		Image image = getResourceManager().createImage(imageDescriptor);
        if (status == JobStatus.CANCEL_REQUESTED || status == JobStatus.CANCELED || isOffline) {
        	image = new Image(Display.getCurrent(), image, SWT.IMAGE_GRAY);
        }
        
        return image;
	}
	
	private ResourceManager getResourceManager() {
		if (resourceManager == null) {
			resourceManager = new LocalResourceManager(JFaceResources.getResources());
		}
		return Assert.assertNotNull(resourceManager);
	}
}
