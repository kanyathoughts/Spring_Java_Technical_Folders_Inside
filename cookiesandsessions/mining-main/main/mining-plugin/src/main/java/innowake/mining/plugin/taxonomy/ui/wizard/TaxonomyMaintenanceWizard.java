/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.taxonomy.ui.wizard;

import java.util.Optional;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.client.ApiClient;

/**
 * Wizard for creating, editing and deleting taxonomies.
 */
public class TaxonomyMaintenanceWizard extends Wizard implements INewWizard {
	
	/**
	 * Public wizard id.
	 */
	public static final String ID = "innowake.mining.taxonomy.wizard.taxonomyMaintenance";

	private final TaxonomyMaintenanceFirstPage page1 = new TaxonomyMaintenanceFirstPage();
	private final TaxonomyMaintenanceSecondPage page2 = new TaxonomyMaintenanceSecondPage();
	
	@Override
	public void init(@Nullable final IWorkbench workbench, @Nullable final IStructuredSelection resourceSelection) {
		setWindowTitle("Mining");
		setHelpAvailable(false);
		setForcePreviousAndNextButtons(true);
		
		final Optional<IProject> project = resourceSelection != null ? SelectionUtil.getProject(resourceSelection) : Optional.empty();
		page1.setProject(project);

		page1.setConnected(ApiClient.checkConnection(project.orElse(null)));

		addPage(page1);
		addPage(page2);
	}

	@Override
	public boolean performFinish() {
		return true;
	}	
}
