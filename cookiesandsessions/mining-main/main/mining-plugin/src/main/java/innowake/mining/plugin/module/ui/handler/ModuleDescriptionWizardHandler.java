/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.module.ui.wizard.ModuleDescriptionWizard;


/**
 * Handler for showing the module description maintenance wizard.
 */
public class ModuleDescriptionWizardHandler extends AbstractHandler {

	@Nullable
	@Override
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final ModuleDescriptionWizard wizard = new ModuleDescriptionWizard();
		wizard.init(WorkbenchUtil.getWorkbench(), SelectionUtil.getResourceSelection().orElse(null));
		
		final WizardDialog dialog = new WizardDialog(HandlerUtil.getActiveShell(event), wizard);
		dialog.setPageSize(500, 400);
		dialog.setModal(false);
		dialog.create();
		dialog.open();

		return null;
	}

}
