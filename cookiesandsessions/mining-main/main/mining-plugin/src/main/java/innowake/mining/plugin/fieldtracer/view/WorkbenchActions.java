/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import java.util.Optional;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

/**
 * Workbench related actions.
 */
public final class WorkbenchActions {

	private WorkbenchActions() {}
	
	/**
	 * Get the shell of the active window 
	 * @return the shell
	 */
	public static Optional<Shell> getActiveWindowShell() {
		return getActiveWorkbenchWindow()
			.map(window -> Optional.ofNullable(window.getShell()))
			.orElseGet(Optional::empty);
	}

	/**
	 * Get the active workbench window.
	 * @return - the active workbench window
	 */
	public static Optional<IWorkbenchWindow> getActiveWorkbenchWindow() {
		IWorkbenchWindow window = null;
		final IWorkbench wbench = PlatformUI.getWorkbench();
		if (wbench != null) {
			window = wbench.getActiveWorkbenchWindow();
		}
		return Optional.ofNullable(window);
	}
	
	/**
	 * Get access to the shared images workbench instance.
	 *
	 * @return The shared images.
	 */
	public static ISharedImages getSharedImages() {
		return PlatformUI.getWorkbench().getSharedImages();
	}
}
