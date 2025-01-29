/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.widgets.Display;

import innowake.lib.core.api.lang.Nullable;

/**
 * This {@link Styler} changes the text color to a dark gray.
 */
public class GreyTextStyler extends Styler {
	
	/** Static instance of the {@link GreyTextStyler}. */
	public static final GreyTextStyler INSTANCE = new GreyTextStyler();
	
	private GreyTextStyler() { }

	@Override
	public void applyStyles(@Nullable final TextStyle textStyle) {
		if (textStyle != null) {
			textStyle.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY);
		}
	}

}
