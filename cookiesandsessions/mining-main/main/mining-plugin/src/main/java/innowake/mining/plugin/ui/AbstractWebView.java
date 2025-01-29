/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.ui;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.client.MiningServiceExecutor.create;
import static innowake.mining.plugin.client.ApiClient.moduleService;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.ViewPart;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.module.FindModuleByPath;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ui.EditorPartListener;
import innowake.mining.plugin.base.ui.LinkWithEditorAction;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.client.ModuleServiceProvider;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Abstract view for Editor linking.
 */
public abstract class AbstractWebView extends ViewPart {

	protected static final String EMPTY_URL = "about:blank";
	/**
	 * This value was taken directly from the example of the JavaDoc of {@link SWT#getPlatform()}
	 * @see SWT#getPlatform()
	 */
	private static final String WINDOWS = "win32";
	
	protected boolean linkedWithEditor = true;
	@Nullable protected ModuleServiceProvider moduleService;
	@Nullable protected IProject project;
	@Nullable private Browser browser;
	private final EditorPartListener editorListener;
	
	/**
	 * Creates an instance.
	 */
	protected AbstractWebView() {
		editorListener = new EditorPartListener();
		editorListener.addEditorChangeConsumer(this::refreshGraphIfLinkedWithEditor);
		editorListener.addEditorCloseConsumer(editor -> resetGraphIfLinkedWithEditor());
	}
	
	protected abstract String buildUrl(String uiServerUrl, Long projectId, Long id, final String linkHash);
	
	@Override
	public void createPartControl(@Nullable final Composite parent) {
		browser = new Browser(parent, SWT.NONE);
		browser.addProgressListener(ProgressListener.completedAdapter(event -> new AuthenticateEclipseView(getBrowserNotNull())));
		final IViewSite viewSite = getViewSite();
		if (viewSite != null) {
			final IToolBarManager toolbarManager = getViewSite().getActionBars().getToolBarManager();
			toolbarManager.add(new LinkWithEditorAction(linkedWithEditor, this::linkWithEditor));
		}
	}
	
	@Override
	public void setFocus() {
		/* no action */
	}
	
	@Override
	public void init(@Nullable final IViewSite site, @Nullable final IMemento memento) throws PartInitException {
		super.init(site, memento);
		editorListener.activate(assertNotNull(site).getPage());
	}
	
	@Override
	public void dispose() {
		if (getSite() != null && getSite().getPage() != null) {
			editorListener.deactivate(getSite().getPage());
		}
		getBrowserNotNull().dispose();
		super.dispose();
	}
	
	protected void linkWithEditor(final Boolean state) {
		linkedWithEditor = state.booleanValue();
		refreshGraphIfLinkedWithEditor(getEditorInput());
	}
	
	protected String createUrl(@Nullable final FileEditorInput input) {
		if (input != null) {
			try {
				final IFile file = input.getFile();
				project = file.getProject();
				
				final String uiServerUrl = MiningPreferences.getApiServerUrl(assertNotNull(project))
						.orElseThrow(() -> new IllegalStateException("API server URL is not configured. Please fix mining project settings."));
				
				final Long projectId = MiningPreferences.getApiProject(project)
						.orElseThrow(() -> new IllegalStateException("Mining project is not configured. Please fix mining project settings.")).getProjectId();

				final FindModuleByPath findModule = updateModuleService(assertNotNull(project))
						.findModuleByPath()
						.setPath(ResourceUtil.getProjectRelativePath(file));
				final Optional<ModulePojo> module = create(() -> findModule).execute();
				if (module.isPresent()) {
					return buildUrl(uiServerUrl, projectId, module.get().getId(), assertNotNull(module.get().getLinkHash()));
				}
			} catch (final IllegalStateException | CoreException | StorageException e) {
				Logging.error(e.getMessage(), e);
			}
		}
		return EMPTY_URL;
	}
	
	protected void refreshGraph(@Nullable final FileEditorInput input) {
		final Browser checkBrowser = getBrowserNotNull();
		if ( ! checkBrowser.isDisposed()) {
			/* unfortunately this is the way SWT browser has to be handled to make it work on Windows */
			/* on macOS this leads to an empty browser view, which does not load any content until a subsequent refresh */
			final boolean empty = isBrowserEmpty();
			if ( ! empty && SWT.getPlatform().equals(WINDOWS)) {
				checkBrowser.setUrl(createUrl(null));
			}
			checkBrowser.setUrl(createUrl(input));
			if ( ! empty) {
				checkBrowser.refresh();
			}
			checkBrowser.layout();
		}
	}
	
	@Nullable
	protected FileEditorInput getEditorInput() {
		@Nullable final IEditorPart editor = WorkbenchUtil.getActiveEditor();
		if (editor != null) {
			@Nullable final IEditorInput input = editor.getEditorInput();
			if (input instanceof FileEditorInput) {
				return (FileEditorInput) input;
			}
		}
		return null;
	}
	
	protected ModuleServiceProvider updateModuleService(final IProject project) throws CoreException, StorageException {
		moduleService = moduleService(project);
		return moduleService;
	}
	
	protected Browser getBrowserNotNull() {
		return assertNotNull(browser);
	}
	
	private void refreshGraphIfLinkedWithEditor(@Nullable final FileEditorInput input) {
		if (linkedWithEditor) {
			refreshGraph(input);
		}
	}
	
	private void resetGraphIfLinkedWithEditor() {
		if (linkedWithEditor && ! getBrowserNotNull().isDisposed() && WorkbenchUtil.getActiveEditor() == null) {
			getBrowserNotNull().setUrl(createUrl(null));
		}
	}

	private boolean isBrowserEmpty() {
		return EMPTY_URL.equals(getBrowserNotNull().getUrl());
	}
	
	/**
	 * An instance of this inner class can be registered as a listener with the browser to provide valid access tokens to eclipse view.
	 */
	private class AuthenticateEclipseView extends BrowserFunction {

		private static final String JAVASCRIPT_METHOD = "java__callback__authenticate_eclipse_view";

		private AuthenticateEclipseView(final Browser browser) {
			super(browser, JAVASCRIPT_METHOD);
		}

		@Nullable
		@Override
		/**
		 * This method will provide valid access token to eclipse view. The token may vary and can be keycloak token or oauth token
		 * depending on the authroized-access profile or default authentication profile. In case the keycloak, if the access token is expired
		 * then it will internally refresh the token and if session is expired then open login page in browser for user to login.
		 * This method is invoked when the java callback to authenticate eclipse view is called from javascript.
		 *
		 * @param arguments the javascript arguments converted to java equivalents
		 * @return the access token to return to the javascript caller 
		 */
		public String function(@Nullable final Object[] arguments) {
			final Optional<ConnectionInfo> connectionInfoOptional = MiningPreferences.getConnectionInfo(assertNotNull(project));
			if (connectionInfoOptional.isPresent()) {
				return connectionInfoOptional.get().getToken();
			} else {
				Logging.error("Could not retrieve access token. Please check configuration.");
			}
			return null;
		}
	}
}
