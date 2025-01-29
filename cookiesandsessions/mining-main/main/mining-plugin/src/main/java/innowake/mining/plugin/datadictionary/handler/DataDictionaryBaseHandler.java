/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.handler;

import static innowake.mining.plugin.base.ui.SelectionUtil.getTokenSelection;

import java.util.Optional;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.datadictionary.FindAllDataDictionaryEntries;
import innowake.mining.client.service.module.FindModuleByPath;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.ViewManager;
import innowake.mining.plugin.annotation.ui.editor.EditorDecorationManager;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.plugin.datadictionary.view.DataDictionarySearchView;
import innowake.mining.plugin.fieldtracer.ParsedProgramFactory;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.fieldtracing.OffsetModel;
import innowake.ndt.fieldtracing.ParsedProgram;
import innowake.ndt.fieldtracing.model.FieldDefinitionFactory;

/**
 * Base implementation for handlers which have to deal with data dictionary entries.
 * <p>
 * Sub-classes have access to some pre-populated members, like e.g.:
 * <ul>
 * <li>module
 * <li>project
 * <li>data field of the selection
 * <li>data dictionary entry of the selection
 * </ul>
 * <p>
 * Before the sub-classes are called there are some validations done, e.g. around the current selection.
 */
public abstract class DataDictionaryBaseHandler extends AbstractHandler implements ViewManager {

	private static final String DATA_DICTIONARY_SEARCH_VIEW_ID = "innowake.mining.datadictionary.view.datadictionarySearch";
	private static final String VIEW_RETRIEVAL_ERROR_MESSAGE = "An error occurred while retrieving the Data Dictionary Search View.";

	protected static final String INVALID_SELECTION_ERROR_TITLE = "Invalid selection";

	protected Optional<ModulePojo> module = Optional.empty();
	protected Optional<IProject> project = Optional.empty();
	protected Optional<DataDictionaryPojo> existingDataDictionaryEntry = Optional.empty();
	protected Optional<FieldDefinition<?>> dataField = Optional.empty();
	protected Optional<DataDictionarySearchView> ddeSearchView = Optional.empty();
	protected Optional<ITextSelection> dataFieldSelection = Optional.empty();

	@Nullable
	protected IStructuredSelection selection;
	@Nullable
	protected IWorkbenchWindow activeWorkbenchWindow;

	private Optional<IFile> file = Optional.empty();


	@Nullable
	@Override
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final Shell shell = HandlerUtil.getActiveShell(event);
		activeWorkbenchWindow = HandlerUtil.getActiveWorkbenchWindow(event);
		selection = SelectionUtil.getResourceSelection().orElse(null);
		project = selection != null ? SelectionUtil.getProject(selection) : Optional.empty();
		file = selection != null ? SelectionUtil.getFile(selection) : Optional.empty();

		if ( ! file.isPresent()) {
			MessageDialog.openError(shell, INVALID_SELECTION_ERROR_TITLE, "Could not determine the file of the active editor.");
			return null;
		}

		if (doGenericFieldtracing(shell)) {
			existingDataDictionaryEntry = entryForSelection(dataFieldSelection);
			if (handle(Assert.assertNotNull(event))) {
				EditorDecorationManager.INSTANCE.reinstall(WorkbenchUtil.getActiveEditor());
			}
		}
		return null;
	}

	@Override
	public void refreshView() {
		if ( ! ddeSearchView.isPresent() && activeWorkbenchWindow != null) {
			final RunnableFuture<IViewPart> runnable = new FutureTask<>(() -> activeWorkbenchWindow.getActivePage().findView(DATA_DICTIONARY_SEARCH_VIEW_ID));
			WorkbenchUtil.asyncExec(Display.getCurrent(), runnable);

			IViewPart viewPart = null;
			try {
				viewPart = runnable.get();
			} catch (final java.util.concurrent.ExecutionException e) {
				MiningPlugin.getDefaultNonNull().getPluginLog().error(VIEW_RETRIEVAL_ERROR_MESSAGE, e);
			} catch (final InterruptedException e) {
				MiningPlugin.getDefaultNonNull().getPluginLog().error(VIEW_RETRIEVAL_ERROR_MESSAGE, e);
				Thread.currentThread().interrupt();
			}

			if (viewPart instanceof DataDictionarySearchView) {
				ddeSearchView = Optional.of((DataDictionarySearchView) viewPart);
			}
		}

		ddeSearchView.ifPresent(view -> WorkbenchUtil.asyncExec(Display.getCurrent(), view::refreshViewIfLinkedWithEditor));
	}

	/**
	 * Sub-classes should handle the given event.
	 * <p>
	 * Sub-classes have access to the project, module and existing data dictionary entry.
	 *
	 * @param event the execution event to handle
	 * @return {@code true} if no errors occurred, {@code false} otherwise
	 * @throws ExecutionException  If the active workbench window variable is not found
	 */
	protected abstract boolean handle(ExecutionEvent event) throws ExecutionException;

	private Optional<DataDictionaryPojo> entryForSelection(final Optional<ITextSelection> textSelection) {
		final Optional<DataDictionaryPojo[]> entries = MiningServiceExecutor.create(this::findDataDictionaryEntries).executeWithDefaultErrorHandling();
		if (entries.isPresent() && textSelection.isPresent()) {
			final ModuleLocation currentLocation = new ModuleLocation(textSelection.get().getOffset(), textSelection.get().getLength());
			for (final DataDictionaryPojo entry : entries.get()) {
				final Optional<ModuleLocation> existingModuleLocation = entry.getLocation();
				if ( ! existingModuleLocation.isPresent()) {
					continue;
				}
				if (currentLocation.isWithin(existingModuleLocation.get()) || currentLocation.overlapsWith(existingModuleLocation.get())) {
					return Optional.of(entry);
				}
			}
		}
		return Optional.empty();
	}

	private FindModuleByPath findModule() throws CoreException, StorageException {
		final IFile presentFile = file.orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID,
				"Could not determine the file of the active editor.")));
		final IProject presentProject = project.orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID,
				"No project present for " + presentFile.getFullPath().toPortableString())));
		return ApiClient.moduleService(presentProject)
				.findModuleByPath()
				.setPath(ResourceUtil.getProjectRelativePath(presentFile));
	}

	private FindAllDataDictionaryEntries findDataDictionaryEntries() throws CoreException, StorageException {
		module = MiningServiceExecutor.create(this::findModule).executeWithDefaultErrorHandling();
		return ApiClient.dataDictionaryService(project.get())
				.findAllDataDictionaryEntries()
				.setModuleId(module.get().identity());
	}

	@SuppressWarnings("unchecked")
	private boolean doGenericFieldtracing(final Shell shell) {
		final Optional<ITextSelection> rawSelection = EditorActionUtil.getFile().equals(file) ? EditorActionUtil.getSelection() : Optional.empty();
		if ( ! rawSelection.isPresent()) {
			MessageDialog.openError(shell, INVALID_SELECTION_ERROR_TITLE, "Please select a data field declaration in an editor.");
			return false;
		}

		final ParsedProgram<?, ? extends AstModel, ? extends AstNode, ? extends AstNode, ? extends AstNode, ? extends OffsetModel<?>> parsedProgram =
				ParsedProgramFactory.create(file.get());
		if ( ! parsedProgram.isRootObject()) {
			MessageDialog.openInformation(shell, INVALID_SELECTION_ERROR_TITLE, "Data fields not declared in the root file is currently not supported.");
			return false;
		}
		final Optional<ITextSelection> tokenSelection = getTokenSelection(parsedProgram.getProgramObject(), rawSelection.get());
		if ( ! tokenSelection.isPresent()) {
			return false;
		}

		try {
			final int offset = tokenSelection.get().getOffset();
			final int length = tokenSelection.get().getLength();
			final String text = tokenSelection.get().getText();

			/* use null as DataDictionaryEntry is currently not supported for includees */
			final int assembledOffset = parsedProgram.getOffsetModel().getOffsetAssembled(offset, null);

			dataField = (Optional<FieldDefinition<?>>) parsedProgram.findFieldDefinition(assembledOffset, length, text);
		} catch (final IllegalStateException e) {
			/* Assuming this occurred due to partial text selection which is not allowed */
			MiningPlugin.getDefaultNonNull().getPluginLog().error(e.getLocalizedMessage(), e);
			return false;
		}

		if ( ! dataField.isPresent()) {
			final String message = "Could not determine a data field for the selection";
			MiningPlugin.getDefaultNonNull().getPluginLog().error(message);
			MessageDialog.openError(shell, INVALID_SELECTION_ERROR_TITLE, message + "\nPlease select a data field declaration.");
			return false;
		}

		final AstNode astNode = (AstNode) dataField.get();
		final innowake.ndt.fieldtracing.model.FieldDefinition<?> fieldDefinition = FieldDefinitionFactory.create(parsedProgram, astNode);
		final EclipseCompatibleTextSelection selectionInUnassembledSourceFile = new EclipseCompatibleTextSelection(fieldDefinition.getSelectionInUnassembledSourceFile());
		dataFieldSelection = Optional.of(selectionInUnassembledSourceFile);

		return true;
	}
}
