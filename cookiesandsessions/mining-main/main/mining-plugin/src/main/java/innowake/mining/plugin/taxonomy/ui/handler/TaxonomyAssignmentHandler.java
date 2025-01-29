/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.taxonomy.ui.handler;

import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.module.ui.handler.AbstractBaseHandler;
import innowake.mining.plugin.taxonomy.ui.TaxonomyAssignmentView;

/**
 * Handler for activating the web based Taxonomy Assignment.
 */
public class TaxonomyAssignmentHandler extends AbstractBaseHandler {

	@Nullable
	@Override
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		boolean viewWasAlreadyOpen = false;
		final IWorkbenchPage activePage = WorkbenchUtil.getActivePage();
		final List<String> pathPatterns;
		try {
			if (activePage != null) {
				final IViewPart foundView = activePage.findView(TaxonomyAssignmentView.ID);
				viewWasAlreadyOpen = foundView != null;
			}
			pathPatterns = getFileAndFolderPathsWithAntPattern();
		} catch (final ValidationException e) {
			Logging.error(e.getLocalizedMessage(), e);
			MessageDialog.openError(HandlerUtil.getActiveShell(event), e.getTitle(), e.getMessage());
			return null;
		}

		TaxonomyAssignmentView view = null;
		try {
			view = (TaxonomyAssignmentView) HandlerUtil.getActiveWorkbenchWindowChecked(event)
					.getActivePage().showView(TaxonomyAssignmentView.ID, null, IWorkbenchPage.VIEW_VISIBLE);
			view.init(pathPatterns);
		} catch (final PartInitException e) {
			throw new ExecutionException("Could not initialize the web based Taxonomy Assignment view.", e);
		}

		/* If the view was already open then the browser component does not properly refresh and we need to trigger an explicit refresh */
		if (viewWasAlreadyOpen) {
			view.refresh();
		}
		WorkbenchUtil.bringToTop(view);

		return null;
	}

}
