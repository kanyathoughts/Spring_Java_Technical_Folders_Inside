/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.widgets.Display;
import innowake.lib.core.api.lang.Nullable;

/**
 * This {@link Styler} changes the text color to blue when logs file is available to download.
 */
public class LinkStyler extends Styler {
	
	protected static final LinkStyler INSTANCE = new LinkStyler(false);
	protected static final LinkStyler INSTANCE_OFFLINE = new LinkStyler(true);
	
	private final boolean isOffline;
	
	private LinkStyler(final boolean isOffline) {
		this.isOffline = isOffline;
	}

	@Override
	public void applyStyles(@Nullable final TextStyle textStyle) {
		if (textStyle != null) {
			textStyle.foreground = isOffline ? Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY): Display.getCurrent().getSystemColor(SWT.COLOR_BLUE);
			textStyle.underline = true;
		}
	}
}

