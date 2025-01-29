/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.taxonomy.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.taxonomy.ui.wizard.TaxonomyMaintenanceWizard;

/**
 * Handler used to show the {@link TaxonomyMaintenanceWizard}.
 */
public class TaxonomyMaintenanceWizardHandler extends AbstractHandler {

	/**
	 * Public id of the command.
	 */
	public static final String ID = "innowake.mining.commands.taxonomyMaintenance";
	
	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final TaxonomyMaintenanceWizard wizard = new TaxonomyMaintenanceWizard();
		wizard.init(WorkbenchUtil.getWorkbench(), SelectionUtil.getResourceSelection().orElse(null));
		
		final Job job = Job.create("Maintain Taxonomy", (@Nullable IProgressMonitor monitor) -> {
			Display.getDefault().asyncExec(() -> {
				final WizardDialog dialog = new WizardDialog(HandlerUtil.getActiveShell(event), wizard) {
					
					@Override
					protected void createButtonsForButtonBar(@Nullable final Composite parent) {
					    super.createButtonsForButtonBar(parent);

					    final Button finish = getButton(IDialogConstants.FINISH_ID);
					    finish.setVisible(false);
					    setButtonLayoutData(finish);

					    final Button cancel = getButton(IDialogConstants.CANCEL_ID);
					    cancel.setText("Close");
					    setButtonLayoutData(cancel);
					}
				};
				dialog.setPageSize(500, 500);
				dialog.setModal(false);
				dialog.create();
				dialog.open();
			});
		});
		job.setSystem(false);
		job.setJobGroup(MiningJobGroup.INSTANCE);
		job.schedule();

		return null;
	}
}
