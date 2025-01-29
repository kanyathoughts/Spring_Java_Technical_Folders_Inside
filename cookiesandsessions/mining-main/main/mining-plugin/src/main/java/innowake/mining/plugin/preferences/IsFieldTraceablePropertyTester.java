/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Collection;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.cobolclipse.ui.editor.CobolEditor;
import innowake.ndt.cobolclipse.ui.view.bms.BMSView;
import innowake.ndt.natclipse.ui.editor.NaturalEditor;


/**
 * Tests if the current selection can be traced using Field Tracer.
 */
public class IsFieldTraceablePropertyTester extends PropertyTester {

	@Override
	public boolean test(
			@Nullable Object receiver, 
			@Nullable String property, 
			@Nullable Object[] args, 
			@Nullable Object expectedValue) {
		
		if ("isFieldTraceable".equalsIgnoreCase(property)) {
			final IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPartService().getActivePart();
			/* active not necessarily means focused, which is why we need further checks */
			if ((activePart instanceof CobolEditor || activePart instanceof NaturalEditor) &&
				(receiver instanceof Collection<?>)) {
				final Collection<?> receiverCollection = (Collection<?>) receiver;
				if (receiverCollection.size() == 1 && 
						receiverCollection.iterator().next() instanceof TextSelection) {
					return true;
				}

			} else if (activePart instanceof BMSView) {
				final BMSView activeBMSView = (BMSView) activePart;
				if (activeBMSView.getSelection() != null) {
					return true;
				}
			}
		}
		
		return false;
	}
}
