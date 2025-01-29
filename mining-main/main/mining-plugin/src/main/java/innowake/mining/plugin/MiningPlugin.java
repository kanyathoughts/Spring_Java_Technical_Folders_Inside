/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.ISourceViewerExtension2;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.osgi.framework.BundleContext;

import innowake.base.eclipse.common.core.NatureDescription;
import innowake.base.eclipse.common.ui.AbstractUIPlugin;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.annotation.ui.editor.AnnotationModelListener;
import innowake.mining.plugin.annotation.ui.editor.EditorDecorationListener;
import innowake.mining.plugin.annotation.ui.editor.EditorDecorationManager;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.deeplink.DeepLinkServerStartUp;
import innowake.mining.plugin.preferences.ProjectValidator;
import innowake.mining.shared.model.FeatureId;
import innowake.ndt.natclipse.ui.editor.ProtectedProjectionViewer;

/**
 * Activator class for the mining plugin.
 */
public class MiningPlugin extends AbstractUIPlugin {

	/**
	 * Plugin ID of the Mining Plugin
	 */
	public static final String ID = "innowake.mining.plugin";

	@Nullable
	private static MiningPlugin plugin;
	
	@Nullable
	private EditorDecorationListener editorDecorationListener;
	
	/**
	 * Called if a new instance of the mining plugin is created.
	 * Used to store the singleton instance.
	 */
	public MiningPlugin() {
		super(ID);
	}

	/**
	 * Return the shared instance (if set) for this plugin.
	 *
	 * @return The shared plugin instance or {@code null}.
	 */
	@Nullable
	public static final synchronized MiningPlugin getDefault() {
		return plugin;
	}
	
	/**
	 * Return the shared instance (if set) for this plugin. If not set this method throws an {@code IllegalStateException}.
	 * 
	 * @return the shared plugin instance; not {@code null}
	 * @throws IllegalStateException if not set
	 */
	public static final MiningPlugin getDefaultNonNull() {
		if (plugin != null) {
			return plugin;
		}
		throw new IllegalStateException("Plugin " + MiningPlugin.ID + " is null.");
	}

	@Override
	protected void startInternal(@Nullable final BundleContext context) throws Exception {
		setPlugin(this);
		
		if (WorkbenchUtil.isWorkbenchRunning()) {
			editorDecorationListener = new EditorDecorationListener();
			WorkbenchUtil.getWorkbench().getDisplay().asyncExec(
					() -> {
						assertNotNull(WorkbenchUtil.getActiveWindow()).getPartService().addPartListener(editorDecorationListener);
						final IEditorPart activeEditor = WorkbenchUtil.getActiveEditor();
						if (activeEditor != null) {
							final IResource resource = activeEditor.getEditorInput().getAdapter(IResource.class);
							if (resource != null && ProjectValidator.isValidWithMiningNature(resource.getProject())) {
								@Nullable final ITextViewer viewer = activeEditor.getAdapter(ITextViewer.class);
								if ( ! (viewer instanceof ProtectedProjectionViewer) && viewer instanceof ISourceViewerExtension2) {
									((ISourceViewerExtension2) viewer).getVisualAnnotationModel().addAnnotationModelListener(new AnnotationModelListener());
								}
							}
						}
						EditorDecorationManager.INSTANCE.install(activeEditor);
					});					
		}
		/* Add the project nature "Mining nature" to all the applicable projects in the workspace */
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		Arrays.stream(root.getProjects()).filter(ProjectValidator::isValid).forEach(project -> {
			try {
				NatureDescription.addNature(project, "Mining Nature", MiningProjectNature.NATURE_ID, null, false, null);
			} catch (final CoreException e) {
				Logging.warn(String.format("Could not add Mining nature to the project %s. Error %s", project.getName(), e.getMessage()));
			}
		});	
		
		CompletableFuture.runAsync(() -> {
			if (Features.INSTANCE.isEnabled(FeatureId.ECLIPSE_DEEP_LINK)) {
				Logging.info("STARTING DEEP LINKS");
				DeepLinkServerStartUp.startUp();
			}
		});

		Features.INSTANCE.refreshCache();
	}

	@Override
	protected void stopInternal(@Nullable final BundleContext context) throws Exception {
		setPlugin(null);
		if (WorkbenchUtil.isWorkbenchRunning()) {
			final Display display = WorkbenchUtil.getWorkbench().getDisplay();
			if ( ! display.isDisposed()) {
				display.asyncExec(
						() -> assertNotNull(WorkbenchUtil.getActiveWindow()).getPartService().removePartListener(editorDecorationListener));
			}
		}
	}
	
	private static synchronized void setPlugin(@Nullable final MiningPlugin miningPlugin) {
		plugin = miningPlugin;
	}
}
