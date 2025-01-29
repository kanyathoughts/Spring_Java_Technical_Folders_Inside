/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.ui;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.jetty.http.HttpStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.FileEditorInput;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.datadictionary.DataDictionaryServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.annotation.ui.editor.EditorDecorationManager;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.datadictionary.view.DataDictionaryEditorView;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.model.DataFieldFormat;

/**
 * Base class for web-based editor views.
 * 
 * @param <T> the type of the entity to be edited
 */
public abstract class WebBasedEditorView<T extends MiningPojo> extends AbstractWebView implements WebBasedView<T> {

	private static final String INVALID_SELECTION = "invalid-selection";

	private static final String ON_SAVE_CALLBACK_NAME = "java__callback__onSave";
	private static final String ON_CANCEL_CALLBACK_NAME = "java__callback__onCancel";
	private static final String ON_DELETE_CALLBACK_NAME = "java__callback__onDelete";
	
	@Nullable
	T value;
	
	/**
	 * Creates a new instance.
	 */
	protected WebBasedEditorView() {
		linkedWithEditor = false;
	}
	
	protected abstract String getUpdateUrlPattern();

	protected abstract String getCreateUrlPattern();

	protected abstract String getViewId();

	@SuppressWarnings("unused")
	@Override
	public void createPartControl(@Nullable final Composite parent) {
		super.createPartControl(parent);
		final Browser browser = getBrowserNotNull();
		browser.addProgressListener(ProgressListener.completedAdapter(event -> {
			new CallbackFunction<Void>(browser, ON_SAVE_CALLBACK_NAME, () -> {
				onSave();
				return null;
			});
			new CallbackFunction<Void>(browser, ON_CANCEL_CALLBACK_NAME, () -> {
				onCancel();
				return null;
			});
			new CallbackFunction<Void>(browser, ON_DELETE_CALLBACK_NAME, () -> {
				onDelete();
				return null;
			});
		}));
	}

	
	@Override
	public void init() {
		setUrl();
	}

	@Override
	public void init(final T value) {
		this.value = value;
		init();
	}

	@Override
	public void refresh() {
		getBrowserNotNull().refresh();
	}
	
	@Override
	protected void refreshGraph(@Nullable final FileEditorInput input) {
		/* Do nothing */
	}

	@Override
	protected String buildUrl(final String uiServerUrl, final Long projectId, final Long moduleId, final String linkHash) {
		final String path;
		if (value != null) {
			final Long id = value.getId();
			path = String.format(getUpdateUrlPattern(), projectId, linkHash, id);
			/* reset the entity in case someone wants to create a new entity after editing an existing one */
			value = null;
		} else {
			final Optional<ITextSelection> selection = EditorActionUtil.getSelection();
			if ( ! selection.isPresent()) {
				Logging.error("Could not determine any editor selection when trying to show the editor.");
				return INVALID_SELECTION;
			}
			final EntityId modId = EntityId.of(moduleId);
			createAstIfNotExists(projectId, modId);
			final Optional<IFile> editorFile = EditorActionUtil.getFile();
			final Optional<IFile> file = editorFile.isPresent() ? editorFile : Optional.empty();
			final ITextSelection textSelection = selection.get();
			long offset = textSelection.getOffset();
			long length = textSelection.getLength();
			if (file.isPresent() && DataDictionaryEditorView.ID.equals(getViewId())) {
				final Optional<DataFieldFormat> dataFieldFormat = getFormatIfSelectionIsValidBasedOnOffset(offset, modId, projectId);
					if (dataFieldFormat.isPresent()) {
						offset = dataFieldFormat.get().getLocation().getOffset();
						length = dataFieldFormat.get().getLocation().getLength();
				}
			}
			if (length <= 0) {
				Logging.error(String.format("Invalid selection length: %d", Long.valueOf(length)));
				return INVALID_SELECTION;
			}
			path = String.format(getCreateUrlPattern(), projectId, linkHash, String.valueOf(offset), String.valueOf(length));
		}
		return String.format("%s%s", StringUtils.appendIfMissing(uiServerUrl, "/"), path);
	}

	private Optional<DataFieldFormat> getFormatIfSelectionIsValidBasedOnOffset(final long offset, final EntityId moduleId, final Long projectId) {
		DataDictionaryServiceProvider dataDictionaryServiceProvider;
		try {
			dataDictionaryServiceProvider = ApiClient.dataDictionaryService(assertNotNull(project));
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		try {
			final Result<DataFieldFormat> results = dataDictionaryServiceProvider.getFormatIfSelectionIsValidBasedOnOffset()
					.setProjectId(projectId)
					.setModuleId(moduleId)
					.setOffset(offset)
					.execute();
			if (results.getStatusCode() != HttpStatus.OK_200) {
				Logging.error(results.getStatusMessage());
			}
			return results.getValue();
		} catch (final IOException e) {
			Logging.error(e.getMessage());
			return Optional.empty();
		}
	}

	private void createAstIfNotExists(final Long projectId, final EntityId moduleId) {
		try {
			final ModuleServiceProvider moduleServiceProvider = ApiClient.moduleService(assertNotNull(project));
			final Optional<String> statusBody = moduleServiceProvider.hasAstNodes()
					.setProjectId(projectId)
					.setModuleId(moduleId)
					.execute()
					.getStatusBody();
			if (statusBody.isPresent() && statusBody.get().contains("false")) {
					moduleServiceProvider.storeAstNodes()
					.setProjectId(projectId)
					.setModuleId(moduleId)
					.execute();
			}
		} catch (final Exception e) {
			throw new IllegalStateException("Could not fetch or store AstNodes", e);
		}
	}

	protected void onSave() {
		updateDecorationsOnActiveEditor();
	}

	protected void onDelete() {
		updateDecorationsOnActiveEditor();
		closeView();
	}

	protected void onCancel() {
		closeView();
	}

	protected ConnectionInfo getConnectionInfo(final String uiServerUrl, final String accessToken) {
		return new ConnectionInfo(System.getProperty("test.url", uiServerUrl), accessToken);
	}

	private void handleInvalidSelection() {
		Logging.error("Invalid selection. Please select some text in an editor.");
		MessageDialog.openError(WorkbenchUtil.getActiveShell(), "Invalid selection", "Please select some text in an editor.");
		closeView();
	}

	private void setUrl() {
		final String url = createUrl(getEditorInput());
		if (INVALID_SELECTION.equals(url)) {
			handleInvalidSelection();
			return;
		}
		getBrowserNotNull().setUrl(url);
	}

	private void closeView() {
		WorkbenchUtil.closeView(getViewId());
	}

	private void updateDecorationsOnActiveEditor() {
		EditorDecorationManager.INSTANCE.reinstall(WorkbenchUtil.getActiveEditor());
	}
}
