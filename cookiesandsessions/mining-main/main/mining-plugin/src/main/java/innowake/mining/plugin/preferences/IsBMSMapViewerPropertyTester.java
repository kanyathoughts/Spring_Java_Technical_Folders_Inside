/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.cobolclipse.ui.view.bms.BMSView;

/**
 * Property Tester class to test if current selection is in BMS Map View
 */
public class IsBMSMapViewerPropertyTester extends PropertyTester {

	@Override
	public boolean test(
			@Nullable Object receiver, 
			@Nullable String property, 
			@Nullable Object[] args, 
			@Nullable Object expectedValue) {
		if ("isBMSMapViewer".equalsIgnoreCase(property)) {
			final IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPartService().getActivePart();
			if (activePart instanceof BMSView) {
				final BMSView activeBMSView = (BMSView) activePart;
				if (activeBMSView.getSelection() != null) {
					return true;
				}
			}
		}
		return false;
	}
}
