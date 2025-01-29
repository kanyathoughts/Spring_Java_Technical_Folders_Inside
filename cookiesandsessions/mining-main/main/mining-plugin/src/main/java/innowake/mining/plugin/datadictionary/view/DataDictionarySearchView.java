/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.view;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.plugin.client.ApiClient.moduleService;

import java.util.Optional;
import java.util.function.Consumer;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditor;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.EditorPartListener;
import innowake.mining.plugin.base.ui.LastSelectedProjectListener;
import innowake.mining.plugin.base.ui.LinkWithEditorAction;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.ModuleLocation;


/**
 * View for searching for Data Dictionary Entries on the currently active project.
 */
public class DataDictionarySearchView extends ViewPart {

	private final LastSelectedProjectListener projectSelectionListener = new LastSelectedProjectListener();
	private boolean linkedWithEditor;
	private final EditorPartListener editorListener;

	@Nullable
	private DataDictionaryTableViewer tableViewer;
	@Nullable
	private IFile lastFileOpened;
	
	public DataDictionarySearchView() {
		editorListener = new EditorPartListener();
		editorListener.addEditorChangeConsumer(f -> refreshViewIfLinkedWithEditor(f.getFile()));
	}
	
	@SuppressWarnings("unused") /* because of labels for layout */
	@Override
	public void createPartControl(@Nullable final Composite parent) {
		final Composite container = new Composite(parent, SWT.NONE);
		
		final GridLayout gridLayout = new GridLayout(1, false);
		container.setLayout(gridLayout);
		
		final Group grpSearchExpressions = new Group(container, SWT.NONE);
		grpSearchExpressions.setLayout(new GridLayout(3, false));
		final GridData gridSearchExpressions = new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1);
		gridSearchExpressions.widthHint = 563;
		grpSearchExpressions.setLayoutData(gridSearchExpressions);
		grpSearchExpressions.setText("Search Expressions");
		
		final Label lblNewLabel = new Label(grpSearchExpressions, SWT.NONE);
		lblNewLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblNewLabel.setText("Element Name:");
		
		final Text elementNameText = new Text(grpSearchExpressions, SWT.BORDER);
		elementNameText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(grpSearchExpressions, SWT.NONE);
		
		final Label lblDescription = new Label(grpSearchExpressions, SWT.NONE);
		lblDescription.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		lblDescription.setText("Description");
		
		final Text descriptionText = new Text(grpSearchExpressions, SWT.BORDER);
		descriptionText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		final Button btnSearch = new Button(grpSearchExpressions, SWT.NONE);
		btnSearch.setText("Search");
		
		tableViewer = new DataDictionaryTableViewer(container, SWT.BORDER | SWT.FULL_SELECTION | SWT.V_SCROLL);
		final Table table = tableViewer.getTable();
		final Menu menuArtifactTable = new Menu(table);
		table.setMenu(menuArtifactTable);
		
		assertNotNull(tableViewer).addDoubleClickListener(this::openFile);
		
		final IWorkbenchPage page = getSite().getPage();
		page.addPostSelectionListener(projectSelectionListener);
		IWorkbenchPart activePart = page.getActivePart();
		if (activePart == null) {
			activePart = page.getActiveEditor();
		}
		projectSelectionListener.selectionChanged(activePart, page.getSelection());
		
		btnSearch.addSelectionListener(new SearchListener(descriptionText, elementNameText));
		
		final IToolBarManager toolbarManager = getViewSite().getActionBars().getToolBarManager();
		toolbarManager.add(new LinkWithEditorAction(linkedWithEditor, this::linkWithEditor));
	}

	private void linkWithEditor(final Boolean state) {
		linkedWithEditor = state.booleanValue();
		final Optional<IFile> file = EditorActionUtil.getFile();
		file.ifPresent(f -> {
			refreshViewIfLinkedWithEditor(f);
			lastFileOpened = f;
		});
	}

	/**
	 * Refreshes this view when the link editor option is selected and a file was previously opened.
	 */
	public void refreshViewIfLinkedWithEditor() {
		if (lastFileOpened != null) {
			refreshViewIfLinkedWithEditor(lastFileOpened);
		}
	}

	private void refreshViewIfLinkedWithEditor(final IFile file) {
		if (linkedWithEditor) {
			final Consumer<ModulePojo> findDDEntries = module -> {
				MiningServiceExecutor
					.create(() -> ApiClient.dataDictionaryService(file.getProject())
						 .findAllDataDictionaryEntries()
						 .setModuleId(module.identity()))
					.setValidResultConsumer(assertNotNull(tableViewer)::setInput)
					.setInvalidResultConsumer(invalidResult -> {
						MessageDialog.openError(getSite().getShell(), "Error while searching in Data Dictionary", invalidResult.getStatusMessage());
						Logging.error(invalidResult.getExtendedStatusMessage());
					})
					.setExceptionConsumer(exception -> MessageDialog.openError(getSite().getShell(), "Exception while searching in Data Dictionary", exception.getLocalizedMessage()))
					.execute();
			};
			MiningServiceExecutor
				.create(() -> moduleService(assertNotNull(file.getProject()))
						.findModuleByPath()
						.setPath(ResourceUtil.getProjectRelativePath(file)))
				.setValidResultConsumer(findDDEntries)
				.setExceptionConsumer(exception -> Logging.error(exception.getMessage(), exception))
				.execute();
		}
	}
	
	@Override
	public void setFocus() {
		/* no special focus handling necessary */
	}

	@Override
	public void init(@Nullable final IViewSite site) throws PartInitException {
		super.init(site);
		editorListener.activate(assertNotNull(site).getPage());
	}
	
	@Override
	public void dispose() {
		getSite().getPage().removePostSelectionListener(projectSelectionListener);
		editorListener.deactivate(getSite().getPage());
		super.dispose();
	}

	private void openFile(@Nullable final DoubleClickEvent event) {
		final IStructuredSelection selection = (IStructuredSelection) Assert.assertNotNull(event).getSelection();
		final DataDictionaryPojo entry = Assert.assertInstanceOf(selection.getFirstElement(), DataDictionaryPojo.class);
		final Optional<IProject> optionalLastSelectedProject = projectSelectionListener.getLastSelectedProject();
		if (optionalLastSelectedProject.isPresent()) {
			final IProject lastSelectedProject = optionalLastSelectedProject.get();
			final Optional<ModulePojo> module = MiningServiceExecutor
					.create(() -> 
						ApiClient.moduleService(lastSelectedProject)
								.findModuleById()
								.setModuleId(entry.getModule()))
					.executeWithDefaultErrorHandling();
	
			if ( ! module.isPresent()) {
				MessageDialog.openError(getSite().getShell(), 
						"Could not determine source file", "The source file for the given entry could not be determined.");
				return;
			}
	
			final IFile file = lastSelectedProject.getFile(module.get().getPath().orElse(null));
			final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			try {
				final IEditorPart editor = IDE.openEditor(page, file);
				final Optional<ModuleLocation> moduleLocation = entry.getLocation();
				if ( ! moduleLocation.isPresent()) {
					return;
				}
				final Control control = editor.getAdapter(Control.class);
				if ( ! (control instanceof StyledText)) {
					return;
				}
				final StyledText text = (StyledText) control;
				final int offset = moduleLocation.get().getOffset().intValue();
				final int length = moduleLocation.get().getLength().intValue();
				text.setSelectionRange(offset, length);
				text.setCaretOffset(offset);
				if ( ! (editor instanceof ITextEditor)) {
					return;
				}
				final ITextEditor textEditor = (ITextEditor) editor;
				textEditor.selectAndReveal(offset, length);
			} catch (final PartInitException e) {
				Logging.error("Could not initialize the editor", e);
				MessageDialog.openError(getSite().getShell(), "Could not initialize the editor", "Please see logs for more information.");
			}
		} else {
			MessageDialog.openError(getSite().getShell(), 
					"Could not determine current project", "Please select a project or a file therein before searching.");
		}
	}
	
	private final class SearchListener extends SelectionAdapter {

		private final Text descriptionText;
		private final Text elementNameText;

		private SearchListener(final Text descriptionText, final Text elementNameText) {
			this.descriptionText = descriptionText;
			this.elementNameText = elementNameText;
		}

		@Override
		public void widgetSelected(@Nullable final SelectionEvent e) {
			final String elementName = elementNameText.getText();
			final String description = descriptionText.getText();
			final Shell shell = getSite().getShell();
			final Optional<IProject> optionalLastSelectedProject = projectSelectionListener.getLastSelectedProject();
			if (optionalLastSelectedProject.isPresent()) {
				final IProject lastSelectedProject = optionalLastSelectedProject.get();
				final Optional<DataDictionaryPojo[]> entries = MiningServiceExecutor
						.create(() -> 
							ApiClient.dataDictionaryService(lastSelectedProject)
									 .searchDataDictionaryEntry()
									 .setDataElementName(elementName)
									 .setDescription(description))
						.setInvalidResultConsumer(invalidResult -> {
							MessageDialog.openError(shell, "Error while searching in Data Dictionary", invalidResult.getStatusMessage());
							Logging.error(invalidResult.getExtendedStatusMessage());
						})
						.setExceptionConsumer(
								exception -> MessageDialog.openError(shell, "Exception while searching in Data Dictionary", exception.getLocalizedMessage()))
						.execute();
				entries.ifPresent(assertNotNull(tableViewer)::setInput);
			} else {
				MessageDialog.openError(shell, 
						"Could not determine current project", "Please select a project or a file therein before searching.");
				return;
			}
		}
	}

	private static class DataDictionaryTableViewer extends TableViewer {
		
		private TableViewerColumn name;
		private TableViewerColumn description;

		/**
		 * Creates a new instance.
		 * 
		 * @param parent the parent control
		 * @param style the SWT style bits
		 */
		public DataDictionaryTableViewer(final Composite parent, final int style) {
			super(parent, style);
			
			setContentProvider(ArrayContentProvider.getInstance());
			getTable().setLinesVisible(true);
			getTable().setHeaderVisible(true);
			getTable().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));		

			name = new TableViewerColumn(this, SWT.NONE);
			name.getColumn().setWidth(100);
			name.getColumn().setText("Element Name");
			name.setLabelProvider(new ColumnLabelProvider() {
				@Nullable
				@Override
				public String getText(@Nullable final Object element) {
					return element instanceof DataDictionaryPojo ? ((DataDictionaryPojo) element).getName() : null;
				}
			});

			description = new TableViewerColumn(this, SWT.NONE);
			description.getColumn().setWidth(500);
			description.getColumn().setText("Description");
			description.setLabelProvider(new ColumnLabelProvider() {
				@Nullable
				@Override
				public String getText(@Nullable final Object element) {
					return element instanceof DataDictionaryPojo ? ((DataDictionaryPojo) element).getDescription() : null;
				}
			});
		}
	}
}
