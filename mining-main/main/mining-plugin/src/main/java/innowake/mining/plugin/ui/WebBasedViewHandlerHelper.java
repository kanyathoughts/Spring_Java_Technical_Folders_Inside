/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.ui;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.WorkbenchUtil;

/**
 * Helper methods for web based {@link IHandler handlers}.
 */
public class WebBasedViewHandlerHelper {

	private WebBasedViewHandlerHelper() {}

	/**
	 * Shows and returns the web based view.
	 * <p>
	 * The view is only made visible without changing the focus, otherwise the Mining property tester might not work correctly.
	 * @param viewId 
	 *
	 * @param event the execution event that contains the application context
	 * @return the activated view instance
	 * @throws PartInitException if the view could not be initialized
	 * @throws ExecutionException if the active workbench window variable is not found
	 */
	@SuppressWarnings("unchecked")
	private static <T> T showWebBasedView(final String viewId, @Nullable final ExecutionEvent event) throws PartInitException, ExecutionException {
		return (T) HandlerUtil.getActiveWorkbenchWindowChecked(event)
				.getActivePage()
				.showView(viewId, null, IWorkbenchPage.VIEW_VISIBLE);
	}

	/**
	 * Launches the web based view.
	 *
	 * @param viewId the ID of the view to open
	 * @param event the {@link ExecutionEvent} that contains the application context
	 * @throws ExecutionException if the active workbench window variable is not found
	 */
	public static void openWebBasedView(final String viewId, @Nullable final ExecutionEvent event) throws ExecutionException {
		openWebBasedView(viewId, event, null);
	}

	/**
	 * Launches the web based view.
	 *
	 * @param viewId the ID of the view to open
	 * @param event the {@linkplain ExecutionEvent} that contains the application context
	 * @param value the value to be modified
	 * @param <T> the type of the value to be modified
	 * @throws ExecutionException if the active workbench window variable is not found
	 */
	@SuppressWarnings("unchecked")
	public static <T> void openWebBasedView(final String viewId, @Nullable final ExecutionEvent event, @Nullable final T value) throws ExecutionException {
		boolean viewWasAlreadyOpen = false;
		final IWorkbenchPage activePage = WorkbenchUtil.getActivePage();
		WebBasedView<T> view = null; 
		if (activePage != null) {
			final IViewPart foundView = activePage.findView(viewId);
			viewWasAlreadyOpen = foundView != null;
			view = (WebBasedView<T>) foundView;
		} 
		
		if (view == null) {
			try {
				view = showWebBasedView(viewId, event);
			} catch (final PartInitException e) {
				throw new ExecutionException("Could not initialize the web based view.", e);
			}
		}

		if (value == null) {
			view.init();
		} else {
			view.init(value);
		}

		/* If the view was already open then the browser component does not properly refresh and we need to trigger an explicit refresh, otherwise the user has
		 * to trigger the view two times. */
		if (viewWasAlreadyOpen) {
			view.refresh();
		}
		WorkbenchUtil.bringToTop(view);
	}
}
