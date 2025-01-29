/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.taxonomy.ui;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.FileEditorInput;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.ui.AbstractWebView;
import innowake.mining.plugin.ui.CallbackFunction;

/**
 * Web-based Taxonomy assignment view.
 */
public class TaxonomyAssignmentView extends AbstractWebView {

	public static final String ID = "innowake.mining.taxonomy.view.taxonomyAssignment";

	private static final String URL = "#/project-%s/taxonomy-assignments";

	private List<String> pathPatterns = Collections.emptyList();

	/**
	 * Construct a new view instance.
	 */
	public TaxonomyAssignmentView() {
		linkedWithEditor = false;
	}

	@SuppressWarnings("unused")
	@Override
	public void createPartControl(@Nullable final Composite parent) {
		super.createPartControl(parent);
		final Browser browser = getBrowserNotNull();
		browser.addProgressListener(ProgressListener.completedAdapter(event -> {
			new CallbackFunction<Void>(browser, "java__callback__onSave", /* do nothing */ () -> null);
			new CallbackFunction<Void>(browser, "java__callback__onCancel", () -> {
				closeView();
				return null;
			});
			new CallbackFunction<>(browser, "java__callback__retrieveIdentifiers", this::getPathPatterns);
		}));
	}

	/**
	 * Initialize the view with the given path patterns.
	 *
	 * @param pathPatterns the path patterns for which the view should display the assignment UI
	 */
	public void init(final List<String> pathPatterns) {
		this.pathPatterns = pathPatterns;
		getBrowserNotNull().setUrl(createUrl());
	}

	protected String createUrl() {
		final Optional<IProject> projectFromSelectedResource = SelectionUtil.getProjectFromSelectedResource();
		if ( ! projectFromSelectedResource.isPresent()) {
			throw new IllegalStateException("Could not retrieve the project from the current selection.");
		}

		try {
			project = projectFromSelectedResource.get();
			final String uiServerUrl = MiningPreferences.getApiServerUrl(assertNotNull(project))
					.orElseThrow(() -> new IllegalStateException("API server URL is not configured. Please fix mining project settings."));

			final Long projectId = MiningPreferences.getApiProject(project)
					.orElseThrow(() -> new IllegalStateException("Mining project is not configured. Please fix mining project settings.")).getProjectId();

			return StringUtils.appendIfMissing(uiServerUrl, "/") + String.format(URL, projectId);
		} catch (final IllegalStateException e) {
			Logging.error(e.getMessage(), e);
			throw new IllegalStateException();
		}
	}

	@Override
	protected String buildUrl(final String uiServerUrl, final Long projectId, final Long id, final String linkHash) {
		throw new NotImplementedException("Custom logic for building the URL, due to not being dependent on a single Module.");
	}

	@Override
	protected void refreshGraph(@Nullable final FileEditorInput input) {
		/* Do nothing */
	}

	private Object[] getPathPatterns() {
		/* The SWT browser cannot convert collections so this needs to be returned as a raw Object array */
		return pathPatterns.toArray();
	}

	private void closeView() {
		WorkbenchUtil.closeView(ID);
	}

	/**
	 * Refreshes the internal browser of this view.
	 */
	public void refresh() {
		getBrowserNotNull().refresh();
	}
}
