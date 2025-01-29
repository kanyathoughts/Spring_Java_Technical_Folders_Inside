/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.controlflow.graph;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.client.MiningServiceExecutor.create;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.NonNull;
import innowake.mining.client.service.module.FindModuleById;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.ui.AbstractWebView;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Represents the control-flow view for module.
 */
public class ControlFlowView extends AbstractWebView {

	/**
	 * Public view id.
	 */
	public static final String ID = "innowake.mining.controlflow.graph";
	private static final String URL = "#/project-%s/module-%s/control-flow";

	@Override
	protected String buildUrl(final @NonNull String uiServerUrl, final @NonNull Long projectId, final @NonNull Long id, final String linkHash) {
		final String formattedURL = String.format(URL, projectId, linkHash);
		return String.format("%s%s%s", uiServerUrl, (uiServerUrl.endsWith("/") ? "" : "/"), formattedURL);
	}

	@Override
	public void createPartControl(@Nullable final Composite parent) {
		super.createPartControl(parent);
		final Browser browser = getBrowserNotNull();
		browser.addProgressListener(ProgressListener.completedAdapter(event -> new Clicked(getBrowserNotNull())));
		browser.setUrl(createUrl(getEditorInput()));
	}

	private class Clicked extends BrowserFunction {

		private static final String JAVASCRIPT_METHOD = "java__callback__clicked";

		private Clicked(final Browser browser) {
			super(browser, JAVASCRIPT_METHOD);
		}

		@Nullable
		@Override
		public Object function(@Nullable final Object[] arguments) {
			if (arguments != null && arguments.length == 2) {
				try {
					final Long moduleId = Long.valueOf(((String) assertNotNull(arguments)[0]));
					final Integer offset = Integer.valueOf(((Double) assertNotNull(arguments)[1]).intValue());
					final FindModuleById findModuleById = assertNotNull(moduleService).findModuleById().setModuleId(EntityId.of(moduleId));
					final Optional<ModulePojo> module = create(() -> findModuleById).execute();
					if (module.isPresent()) {
						module.ifPresent(m -> m.getPath().ifPresent(path -> open(path, offset)));
					} else {
						Logging.error("Module could not be retrieved successfully");
					}
				} catch (final NumberFormatException e) {
					Logging.error("Error fetching moduleId / offset" + e);
				}
			}
			return null;
		}

		private void open(final String path, final Integer offset) {
			@Nullable final IResource resource = assertNotNull(project).findMember(path);
			if (resource instanceof IFile && resource.exists()) {
				try {
					if (getSite() != null && getSite().getPage() != null) {
						final ITextEditor editor = (ITextEditor) IDE.openEditor(getSite().getPage(), (IFile) resource);
						editor.selectAndReveal(offset.intValue(), 0);
					}
				} catch (final PartInitException e) {
					Logging.error("Error while opening editor on " + path, e);
				}
			}
		}
	}
}
