/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.dependency.ui;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.client.MiningServiceExecutor.create;
import java.awt.Desktop;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.exceptions.ExecutionException;
import innowake.mining.client.service.module.FindModuleById;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.controlflow.graph.ControlFlowView;
import innowake.mining.plugin.ui.AbstractWebView;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Represents the graph view for dependencies between modules.
 */
public class DependencyGraphView extends AbstractWebView {

	/**
	 * Enum of the Graph Type that can be opened from the Context Menu Toolbar.
	 */
	private enum GraphType {
		DEPENDENCY_GRAPH("dependencyGraph"),
		CONTROL_FLOW_GRAPH("controlFlowGraph");

		@Nullable
		private final String name;

		private GraphType(final String name) {
			this.name = name;
		}

		private GraphType() {
			this.name = null;
		}
		
		private static GraphType fromName(final String name) {
			for (final GraphType value: values()) {
				if (value.name != null && value.name.equalsIgnoreCase(name) || value.name().equalsIgnoreCase(name)) {
					return value;
				}
			}
			throw new IllegalArgumentException("No enum constant " + name + " available");
		}
	}
	/**
	 * Public view id.
	 */
	public static final String ID = "innowake.mining.dependency.graph";
	private static final String URL = "#/project-%s/module-%s/dependencies";
	private int depthShown = 1;

	@Override
	public void createPartControl(@Nullable final Composite parent) {
		super.createPartControl(parent);
		final Browser browser = getBrowserNotNull();
		final String graphUrl = createUrl(getEditorInput());
		browser.addProgressListener(ProgressListener.completedAdapter(event -> new Clicked(getBrowserNotNull())));
		browser.addProgressListener(ProgressListener.completedAdapter(event -> new OpenMiningUiPage(getBrowserNotNull(), graphUrl)));
		browser.setUrl(createUrl(getEditorInput()), null, new String[] {"maxDepth = " + depthShown});
	}

	@Override
	protected String buildUrl(final @NonNull String uiServerUrl, final @NonNull Long projectId, final @NonNull Long moduleId,
			final String linkHash) {
		final String formattedURL = String.format(URL, projectId, linkHash);
		return String.format("%s%s%s", uiServerUrl, (uiServerUrl.endsWith("/") ? "" : "/"), formattedURL);
	}

	private class Clicked extends BrowserFunction {
		
		private static final String JAVASCRIPT_METHOD = "java__callback__clicked";

		private Clicked(final Browser browser) {
			super(browser, JAVASCRIPT_METHOD);
		}

		@Nullable
		@Override
		public Object function(@Nullable final Object[] arguments) {
			if (arguments != null && arguments.length > 1) {
				/* TypeScript only knows double as numeric type */
				final Long moduleId = Long.valueOf(((Double) assertNotNull(arguments)[0]).longValue());
				final GraphType graphType = GraphType.fromName(String.valueOf((assertNotNull(arguments)[1])));
				final FindModuleById findModuleById = assertNotNull(moduleService).findModuleById().setModuleId(EntityId.of(moduleId));
				final Optional<ModulePojo> module = create(() -> findModuleById).execute();
				if (arguments.length > 2) {
					depthShown = ((Double) assertNotNull(arguments)[2]).intValue();
				} else if (graphType == GraphType.DEPENDENCY_GRAPH) {
					Logging.error("Unable to open Dependency Graph View, invalid arguments!");
					return null;
				}
				module.ifPresent(m -> m.getPath().ifPresent(this::open));
				if (graphType == GraphType.CONTROL_FLOW_GRAPH) {
					try {
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView(ControlFlowView.ID);
					} catch (final PartInitException e) {
						Logging.error("Unable to open Control Flow Graph View!", e);
					}
				}
			} else {
				Logging.error("Unable to open Graph View, not enough arguments!");
			}
			return null;
		}

		private void open(final String path) {
			@Nullable final IResource resource = assertNotNull(project).findMember(path);
			if (resource instanceof IFile && resource.exists()) {
				try {
					IDE.openEditor(getSite().getPage(), (IFile) resource);
				} catch (final PartInitException e) {
					Logging.error("Error while opening editor on " + path, e);
				}
			}
		}
	}
	
	private class OpenMiningUiPage extends BrowserFunction {
		
		private static final String JAVASCRIPT_METHOD = "java__callback__openMiningUiPage";
		private Optional<String> serverUrl = Optional.empty();

		private OpenMiningUiPage(final Browser browser, final String graphUrl) {
			super(browser, JAVASCRIPT_METHOD);
			try {
				final java.net.URL url = new java.net.URL(graphUrl);
				final StringBuilder sb = new StringBuilder();
				sb.append(url.getProtocol()).append("://").append(url.getHost());
				final int port = url.getPort();
				if (port != -1) {
					sb.append(':').append(port);
				}
				this.serverUrl = Optional.of(sb.toString());
			} catch (final MalformedURLException e) {
				Logging.error(e.getLocalizedMessage(), e);
			}
		}
		
		@Nullable
		@Override
		public Object function(@Nullable final Object[] arguments) {
			if ( ! this.serverUrl.isPresent()) {
				Logging.error("Unable to open Mining UI: Could not determine server url!");
				return null;
			} else if (arguments == null || arguments.length < 1) {
				Logging.error("Unable to open Mining UI: Not enough arguments present!");
				return null;
			}
			
			final String url = this.serverUrl.get() + arguments[0].toString();
			if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
				try {
					Desktop.getDesktop().browse(new URI(url));
				} catch (final IOException | URISyntaxException e) {
					throw new ExecutionException("Could not open Mining UI", e);
				}
			}
			return null;
		}
	}
}
