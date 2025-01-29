/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import static innowake.mining.plugin.fieldtracer.view.WorkbenchActions.getSharedImages;

import java.io.IOException;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.part.ViewPart;

import innowake.lib.core.lang.Nullable;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.fieldtracer.model.export.IFileStorage;
import innowake.ndt.fieldtracing.model.AccessMode;
import innowake.ndt.fieldtracing.model.FieldDefinition;
import innowake.ndt.fieldtracing.model.FieldUsage;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.ModelUtils;
import innowake.ndt.fieldtracing.model.Node;
import innowake.ndt.fieldtracing.model.SourceFile;
import innowake.ndt.fieldtracing.model.SourceFileOrFieldDefinition;
import innowake.ndt.fieldtracing.model.export.ExportFormat;
import innowake.ndt.fieldtracing.model.tree.Tree;
import innowake.ndt.fieldtracing.model.tree.TreeNode;

/**
 * Represent the field tracer view part ui.
 */
public final class FieldtracerView extends ViewPart implements ViewAdapter<IFile> {

	private static final String LBL_MODULE_FORMAT = "%s <a>(Open Editor)</a>";
	private static final String LBL_TRACED_FIELD_FORMAT = "%s <a>(Goto Declaration)</a>";
	private static final String LBL_LINE_FORMAT = "%d <a>(Goto Line)</a>";
	
	public static final String ID = "innowake.mining.fieldtracer";
	private final ExceptionHandler exceptionHandler;
	private boolean linkedWithEditor = true;
	private boolean inResize = false;
	
	private TableViewer tblStatements;
	private CheckboxTreeViewer treeVariable;
	private TreeViewer treeFiles;
	private TableViewer treeVarAccessRead;
	private TableViewer treeVarAccessWrite;
	private Link lnkModule; 
	private Link lnkTracedVariable;
	private Link lnkLine;
	private Link lbUsedStatment;
	private final StatementViewerComparator tblStatementComparator = new StatementViewerComparator();
	private final FilesViewerComparator treeFilesComparator = new FilesViewerComparator();
	private Optional<Model<IFile>> model = Optional.empty();
	private TreeViewerColumn tcLvl;
	private TreeViewerColumn tcVarType;
	private TreeViewerColumn tcVariable;
	private TreeViewerColumn tcFiles;
	private TableViewerColumn tcLine;
	private TableViewerColumn tcType;
	private TableViewerColumn tcField;
	private TableViewerColumn tcStatement;
	
	
	/**
	 * Create a new instance of the field tracer ui.
	 */
	public FieldtracerView() {
        super();
		exceptionHandler = new ExceptionHandler(getActiveShell());
	}
	
	@Override
	public void setModel(final Model<IFile> model) {
		this.model = Optional.of(model);
		treeFiles.setInput(model);
		
		final Tree<?> tree = ModelUtils.buildFieldDefinitionTree(model);
		treeVariable.setInput(tree.getChildren().toArray());
		setPartName(Optional.of(model.getSelectedField()));
		initialSelection();
	}
	
	@Override
	public Image getTitleImage() {
		return getSharedImages().getImage(ISharedImages.IMG_TOOL_FORWARD);
	}
	
	private void setPartName(final Optional<FieldDefinition<IFile>> node) {
		super.setPartName("Field Tracer " + node.map(FieldDefinition::getFieldName)
			.orElseGet(() -> ""));
	}
	
	/**
	 * Create contents of the view part.
	 * @param compMainContent The parent element to add the new components to.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void createPartControl(@Nullable final Composite compMainContent) {
		final SashForm sashMain = new SashForm(compMainContent, SWT.NONE);
		final Composite sashPnlMainLeft = new Composite(sashMain, SWT.NONE);
		sashPnlMainLeft.setLayout(new GridLayout(4, false));
		
		treeFiles = new TreeViewer(sashPnlMainLeft, SWT.BORDER| SWT.MULTI);
		treeFiles.getTree().setLinesVisible(true);
		treeFiles.getTree().setHeaderVisible(true);
		treeFiles.setAutoExpandLevel(AbstractTreeViewer.ALL_LEVELS);
		treeFiles.setContentProvider(new FilesContentProvider());
		treeFiles.setComparator(treeFilesComparator);
		final GridData gdTreeFiles = new GridData(SWT.LEFT, SWT.FILL, false, true, 1, 1);
		gdTreeFiles.minimumWidth = 150;
		gdTreeFiles.widthHint = 150;
		treeFiles.getTree().setLayoutData(gdTreeFiles);
		treeFiles.addDoubleClickListener(event -> openModuleInEditor( (SourceFile<IFile>) treeFiles.getStructuredSelection().getFirstElement() ));
		
		tcFiles = new TreeViewerColumn(treeFiles, SWT.NONE);
		tcFiles.getColumn().setWidth(150);
		tcFiles.getColumn().setText("Files");
		tcFiles.getColumn().addSelectionListener(getFilesTreeSelectionAdapter(tcFiles.getColumn(), 0));
		tcFiles.setLabelProvider(new FilesLabelProvider());
		tcFiles.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				treeFilesResize();
			}
		});
		
		sashPnlMainLeft.addControlListener(new ControlAdapter() {

			@Override
			public void controlResized(final ControlEvent event) {
				treeFilesResize();
			}
		});
		
		treeVariable = new CheckboxTreeViewer(sashPnlMainLeft, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL);
		final GridData gdtblVariable = new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1);
		gdtblVariable.minimumWidth = 180;
		gdtblVariable.widthHint = 180;
		treeVariable.getTree().setLayoutData(gdtblVariable);
		treeVariable.getTree().setHeaderVisible(true);
		treeVariable.getTree().setLinesVisible(true);
		treeVariable.setContentProvider(new VariableContentProvider());
		treeVariable.addDoubleClickListener( event -> {
			final Object firstElement = treeVariable.getStructuredSelection().getFirstElement();
			if (firstElement != null) {
				final TreeNode<SourceFileOrFieldDefinition<IFile>> element = (TreeNode<SourceFileOrFieldDefinition<IFile>>) firstElement;
				if (element.getData().isFieldDefinition()) {
					openLineInEditor(element.getData().getFieldDefinition());
				}
				treeVariable.getTree().forceFocus();
			}
		} );
		treeVariable.addCheckStateListener(event -> updateStatementTableFromTree());
		treeVariable.setCheckStateProvider(new ICheckStateProvider() {
			
			@Override
			public boolean isChecked(final Object element) {
				return false;
			}

			@Override
			public boolean isGrayed(final Object element) {
				return ((TreeNode<?>) element).getParent() == null;
			}
		});

		tcVariable = new TreeViewerColumn(treeVariable, SWT.NONE);
		tcVariable.getColumn().setWidth(200);
		tcVariable.getColumn().setText("Variables");
		tcVariable.setLabelProvider(new VariableLabelProvider());
		tcVariable.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				treeVariablesResize();
			}
		});

		tcVarType = new TreeViewerColumn(treeVariable, SWT.NONE);
		tcVarType.getColumn().setWidth(80);
		tcVarType.getColumn().setText("Type");
		tcVarType.setLabelProvider(new VariableLabelProvider());
		tcVarType.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				treeVariablesResize();
			}
		});
		
		tcLvl = new TreeViewerColumn(treeVariable, SWT.NONE);
		tcLvl.getColumn().setWidth(33);
		tcLvl.getColumn().setText("Lvl");
		tcLvl.setLabelProvider(new VariableLabelProvider());
		tcLvl.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				treeVariablesResize();
			}
		});
		
		final MenuManager cmmVariableTree = new MenuManager();
		cmmVariableTree.add(new Action("Deep Toggle") {
			@Override
			public void run() {
				treeVariablesDeepToggleCheckState();
			}
		});
		cmmVariableTree.add(new Action("Deep Expand") {
			@Override
			public void run() {
				treeVariablesDeepExpandCheckState();
			}
		});
		
		final Menu cmVariableTree = cmmVariableTree.createContextMenu(treeVariable.getControl());
		treeVariable.getControl().setMenu(cmVariableTree);

		sashPnlMainLeft.addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				treeFilesResize();
				treeVariablesResize();
				tblStatementsResize();
			}
		});
		
		tblStatements = new TableViewer(sashPnlMainLeft, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
		final GridData gdtblStatements = new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1);
		gdtblStatements.minimumWidth = 200;
		gdtblStatements.widthHint = 200;
		tblStatements.getTable().setLayoutData(gdtblStatements);
		tblStatements.getTable().setHeaderVisible(true);
		tblStatements.getTable().setLinesVisible(true);
		tblStatements.setContentProvider(new StatementContentProvider());
		tblStatements.addSelectionChangedListener(event -> statementSelectionChanged());
		tblStatements.setComparator(tblStatementComparator);
		
		tcLine = new TableViewerColumn(tblStatements, SWT.NONE);
		tcLine.getColumn().setWidth(40);
		tcLine.getColumn().setText("Line");
		tcLine.getColumn().addSelectionListener(getStatementTableSelectionAdapter(tcLine.getColumn(), 0));
		tcLine.setLabelProvider(new StatementLabelProvider());
		tcLine.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				tblStatementsResize();
			}
		});
		
		tcType = new TableViewerColumn(tblStatements, SWT.NONE);
		tcType.getColumn().setWidth(50);
		tcType.getColumn().setText("Type");
		tcType.getColumn().addSelectionListener(getStatementTableSelectionAdapter(tcType.getColumn(), 1));
		tcType.setLabelProvider(new StatementLabelProvider());
		tcType.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				tblStatementsResize();
			}
		});
		
		tcField = new TableViewerColumn(tblStatements, SWT.NONE);
		tcField.getColumn().setWidth(150);
		tcField.getColumn().setText("Field");
		tcField.getColumn().addSelectionListener(getStatementTableSelectionAdapter(tcField.getColumn(), 2));
		tcField.setLabelProvider(new StatementLabelProvider());
		tcField.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				 tblStatementsResize();
			}
		});
		
		tcStatement = new TableViewerColumn(tblStatements, SWT.NONE);
		tcStatement.getColumn().setWidth(120);
		tcStatement.getColumn().setText("Statement");
		tcStatement.getColumn().addSelectionListener(getStatementTableSelectionAdapter(tcStatement.getColumn(), 3));
		tcStatement.setLabelProvider(new StatementLabelProvider());
		tcStatement.getColumn().addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(final ControlEvent event) {
				tblStatementsResize();
			}
		});
		
		final Composite sashPnlMainRight = new Composite(sashMain, SWT.BORDER);
		sashPnlMainRight.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		final SashForm sFRightPnl = new SashForm(sashPnlMainRight, SWT.VERTICAL);
		
		final Group grpDetails = new Group(sFRightPnl, SWT.NONE);
		grpDetails.setText("Details");
		grpDetails.setLayout(new GridLayout(2, false));
		
		final Label lblModule = new Label(grpDetails, SWT.NONE);
		lblModule.setText("Module");
		
		lnkModule = new Link(grpDetails, SWT.NONE);
		lnkModule.setBackground(grpDetails.getBackground());
		final GridData gridDataModule = new GridData();
		gridDataModule.widthHint = 400;
		lnkModule.setLayoutData(gridDataModule);
		lnkModule.addSelectionListener( new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent event) {
				openModuleInEditor( (FieldUsage<IFile>) tblStatements.getStructuredSelection().getFirstElement() );
			}
		} );
		
		final Label lblTracedVariable = new Label(grpDetails, SWT.NONE);
		lblTracedVariable.setText("Traced variable");
		
		lnkTracedVariable = new Link(grpDetails, SWT.NONE | SWT.NO_BACKGROUND);
		lnkTracedVariable.setBackground(grpDetails.getBackground());
		final GridData gDTracVar = new GridData();
		gDTracVar.widthHint = 400;
		lnkTracedVariable.setLayoutData(gDTracVar);
		lnkTracedVariable.addSelectionListener( new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent event) {
				openDeclarationInEditor();
			}
		} );
		
		final Label lblUsedInStmt = new Label(grpDetails, SWT.NONE);
		lblUsedInStmt.setText("Used in statement");
		
		lbUsedStatment = new Link(grpDetails, SWT.NONE);
		final GridData gDUsedInStmt = new GridData();
		gDUsedInStmt.widthHint = 400;
		lbUsedStatment.setLayoutData(gDUsedInStmt);
		
		final Label lblLine = new Label(grpDetails, SWT.NONE);
		lblLine.setText("Line");
		
		lnkLine = new Link(grpDetails, SWT.NONE);
		final GridData gridDataLine = new GridData();
		gridDataLine.widthHint = 400;
		lnkLine.setLayoutData(gridDataLine);
		lnkLine.setBackground(grpDetails.getBackground());
		lnkLine.addSelectionListener( new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent event) {
				final Object node = tblStatements.getStructuredSelection().getFirstElement();
				if (node instanceof Node) {
					openLineInEditor((Node<IFile>) node);
				}
			}
		} );
		
		final Composite lbSpace = new Composite(grpDetails, SWT.NONE);
		final GridData gridDataSpacer = new GridData();
		gridDataSpacer.grabExcessHorizontalSpace = true;
		gridDataSpacer.grabExcessVerticalSpace = true;
		gridDataSpacer.horizontalAlignment = SWT.FILL;
		gridDataSpacer.verticalAlignment = SWT.FILL;
		lbSpace.setLayoutData(gridDataSpacer);
		
		final Group grpReadWrite = new Group(sFRightPnl, SWT.NONE);
		grpReadWrite.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		final SashForm sashFormVarAccess = new SashForm(grpReadWrite, SWT.NONE);
		
		final Group grpVarAccessRead = new Group(sashFormVarAccess, SWT.NONE);
		grpVarAccessRead.setText("Read Variables");
		grpVarAccessRead.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		treeVarAccessRead = new TableViewer(grpVarAccessRead, SWT.BORDER);
		treeVarAccessRead.getTable().setLinesVisible(true);
		treeVarAccessRead.getTable().setHeaderVisible(true);
		treeVarAccessRead.setContentProvider(new VariableAccessContentProvider(AccessMode.READ));
		treeVarAccessRead.addDoubleClickListener(new SubTraceAction());
		
		final TableViewerColumn colVarReadName = new TableViewerColumn(treeVarAccessRead, SWT.NONE);
		colVarReadName.getColumn().setWidth(150);
		colVarReadName.getColumn().setText("Name");
		colVarReadName.setLabelProvider(new VariableAccessLabelProvider());
		
		final Group grpVarAccessWrite = new Group(sashFormVarAccess, SWT.NONE);
		grpVarAccessWrite.setText("Write Variables");
		grpVarAccessWrite.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		treeVarAccessWrite = new TableViewer(grpVarAccessWrite, SWT.BORDER);
		treeVarAccessWrite.getTable().setLinesVisible(true);
		treeVarAccessWrite.getTable().setHeaderVisible(true);
		treeVarAccessWrite.setContentProvider(new VariableAccessContentProvider(AccessMode.WRITE));
		treeVarAccessWrite.addDoubleClickListener(new SubTraceAction());
		
		final TableViewerColumn colVarWriteName = new TableViewerColumn(treeVarAccessWrite, SWT.NONE);
		colVarWriteName.getColumn().setWidth(150);
		colVarWriteName.getColumn().setText("Name");
		colVarWriteName.setLabelProvider(new VariableAccessLabelProvider());
		
		sashFormVarAccess.setWeights(new int[] {1, 1});
		sFRightPnl.setWeights(new int[] {2, 3});
		sashMain.setWeights(new int[] {4, 2});
		
		createActions();
		initializeToolBar();
		initializeMenu();
	}
	
	/**
	 * Create the actions.
	 */
	private void createActions() {
		/* do nothing */
	}

	/**
	 * Initialize the toolbar.
	 */
	private void initializeToolBar() {
		final IToolBarManager toolbarManager = getViewSite().getActionBars().getToolBarManager();
		toolbarManager.add(new ExportModelAction());
		toolbarManager.add(new LinkWithEditorAction());
	}

	/**
	 * Initialize the menu.
	 */
	private void initializeMenu() {
		/* do nothing */
	}

	@Override
	public void setFocus() {
		/* do nothing */
	}
	
	@Override
	public void handleException(final String message, final Exception exception) {
		exceptionHandler.handle(message, exception);
	}

	@SuppressWarnings("unchecked")
	private void showDetails(final IStructuredSelection selection) {
		if (selection.isEmpty()) {
			return;
		}
		final FieldUsage<IFile> node = (FieldUsage<IFile>) selection.getFirstElement();
		setModule(node.getModule().getSourceObject());
		setReadWriteVariables(node);
		lbUsedStatment.setText(node.getStatementName());
		setTracedField(getModel().getSelectedField());
		setLine(node.getLine());
	}

	private void setModule(final IFile module) {
		if (module == null) {
			lnkModule.setText("");
			lnkModule.setToolTipText("");
		} else {
			lnkModule.setText( String.format(LBL_MODULE_FORMAT, module.getName()) );
			lnkModule.setToolTipText(module.getProjectRelativePath().toPortableString());
		}
	}
	
	private void setTracedField(final FieldDefinition<IFile> node) {
		lnkTracedVariable.setText( String.format(LBL_TRACED_FIELD_FORMAT, node.getFieldName()) );
		setPartName(Optional.of(node));
	}

	private void setLine(final Optional<Integer> line) {
		if (line.isPresent()) {
			lnkLine.setText(String.format(LBL_LINE_FORMAT, line.orElseGet(() -> Integer.valueOf(0)) ));
		} else {
			lnkLine.setText("");
		}
	}
	
	private void setReadWriteVariables(final FieldUsage<IFile> node) {
		treeVarAccessRead.setInput(node);
		treeVarAccessWrite.setInput(node);
	}

	private void statementSelectionChanged() {
		final Node<?> usage = (Node<?>) tblStatements.getStructuredSelection().getFirstElement();
		if (usage != null) {
			openLineInEditor(usage);
			showDetails(tblStatements.getStructuredSelection());
			tblStatements.getTable().forceFocus();
		}
	}
	
	private void openModuleInEditor(final Node<?> source) {
		openInEditor((IFile) source.getModule().getSourceObject(), Optional.empty());
	}
	
	private void openLineInEditor(final Node<?> cobolField) {
		openInEditor((IFile) cobolField.getModule().getSourceObject(), cobolField.getLine());
	}

	
	private void openDeclarationInEditor() {
		final FieldDefinition<IFile> fieldDef = getModel().getSelectedField();
		openInEditor(fieldDef.getModule().getSourceObject(), fieldDef.getLine());
	}
	private void openInEditor(final IFile module, final Optional<Integer> line) {
		if ( ! linkedWithEditor) {
			return;
		}
		try {
			if (line.isPresent()) {
				EditorActionUtil.openEditorAndGotoLine(module, line.get().intValue());
			} else {
				EditorActionUtil.openEditor(module);
			}
		} catch (BadLocationException e) {
			exceptionHandler.handle("Error while opening the editor.", e);
		}		
	}
	
	private Model<IFile> getModel() {
		return model.orElseThrow(() -> new IllegalStateException("The model was not set.")); 
	}
	
	/**
	 * Toolbar action to display the "Link with Editor" action.
	 */
	private final class LinkWithEditorAction extends Action implements IPropertyChangeListener {

		LinkWithEditorAction() {
			setChecked(linkedWithEditor);
			setImageDescriptor(getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_SYNCED));
			setDisabledImageDescriptor(getSharedImages().getImageDescriptor(ISharedImages.IMG_ELCL_SYNCED_DISABLED));
			setToolTipText("Enable or disable the link between the statements tree and the editor.");
			addPropertyChangeListener(this);
		}
		
		@Override
		public void propertyChange(final PropertyChangeEvent event) {
			if ("checked".equals(event.getProperty())) {
				linkedWithEditor = ! linkedWithEditor;
				setChecked(linkedWithEditor);
			}
		}
	}
	
	private final class ExportModelAction extends Action {

		ExportModelAction() {
			setImageDescriptor(getSharedImages().getImageDescriptor(ISharedImages.IMG_ETOOL_SAVEAS_EDIT));
			setToolTipText("Export the current model as CSV file.");
		}
		
		@Override
		public void run() {
			final IFileStorage<IFile> fileStorage = new IFileStorage<>();
			try {
				final ExportFormat format = ExportFormat.CSV_COMMA_V2;
				if (fileStorage.exist(format, getModel()) && 
						! MessageDialog.openConfirm(getActiveShell(), "Override", "Field trace export already exist. Override?")) {
					return;
				}
				final IFile exported = new IFileStorage<IFile>().store(format, getModel(), true);
				if (MessageDialog.openConfirm(getActiveShell(), "Fieldtrace Export", 
					String.format("Successfully exported to file %s.\n\nShould the file be opened?", exported.getLocation()))) {
					EditorActionUtil.openEditor(exported);
				}
			} catch (final IOException | CoreException e) {
				handleException("Error while export model.", e);
			}
		}
		
	}
	
	private SelectionAdapter getFilesTreeSelectionAdapter(final TreeColumn column, final int index) {
		return new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent event) {
				treeFilesComparator.setColumn(index);
				treeFiles.getTree().setSortDirection(treeFilesComparator.getDirection());
				treeFiles.getTree().setSortColumn(column);
				treeFiles.refresh();
			}
		};
	}
	
	private SelectionAdapter getStatementTableSelectionAdapter(final TableColumn column, final int index) {
		return new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent event) {
				tblStatementComparator.setColumn(index);
				tblStatements.getTable().setSortDirection(tblStatementComparator.getDirection());
				tblStatements.getTable().setSortColumn(column);
				tblStatements.refresh();
			}
		};
	}
	
	private void initialSelection() {
		treeVariable.expandAll();
		final TreeItem[] root = treeVariable.getTree().getItems();
		checkTracedField(root, getModel().getSelectedField());
		updateStatementTableFromTree();
		
		tblStatementComparator.setColumn(0);
		tblStatements.getTable().setSortDirection(tblStatementComparator.getDirection());
		tblStatements.getTable().setSortColumn(tblStatements.getTable().getColumn(0));
		tblStatements.refresh();
		
		treeFilesResize();
		treeVariablesResize();
		tblStatementsResize();
	}
	
	private boolean checkTracedField(final TreeItem[] nodes, final FieldDefinition<IFile> selectedField) {
		for (final TreeItem node : nodes) {
			final Object data = node.getData();
			if (data instanceof TreeNode) {
				final Object dataOfData = ((TreeNode<?>) data).getData();
				if (dataOfData instanceof SourceFileOrFieldDefinition && selectedField.equals(((SourceFileOrFieldDefinition<?>) dataOfData).getFieldDefinition())) {
					treeVariable.setChecked(data, true);
					treeVariable.collapseToLevel(data, 2);
					return true;
				}
			}
			final boolean checked = checkTracedField(node.getItems(), selectedField);
			if (checked) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Show the statements for the checked elements from the variable tree.
	 * Must filter out the virtual root node to avoid duplicate statement entries.
	 *
	 */
	private void updateStatementTableFromTree() {
		final Object[] array = Arrays.stream(treeVariable.getCheckedElements())
				.map(TreeNode.class::cast)
				.filter(elem -> elem.getParent() != null)
				.collect(Collectors.toList()).toArray();
		tblStatements.setInput(array);
	}
	
	private void treeVariablesDeepToggleCheckState() {
		final ITreeSelection structuredSelection = treeVariable.getStructuredSelection();
		if ( ! structuredSelection.isEmpty()) {
			final Object firstElement = structuredSelection.getFirstElement();
			try {
				treeVariable.getTree().setRedraw(false);
				final boolean checked = treeVariable.getChecked(firstElement);
				treeVariable.setSubtreeChecked(firstElement, !checked);
				updateStatementTableFromTree();
			} finally {
				treeVariable.getTree().setRedraw(true);
			}
		}
	}

	private void treeVariablesDeepExpandCheckState() {
		final ITreeSelection structuredSelection = treeVariable.getStructuredSelection();
		if ( ! structuredSelection.isEmpty()) {
			final Object firstElement = structuredSelection.getFirstElement();
			try {
				treeVariable.getTree().setRedraw(false);
				treeVariable.expandToLevel(firstElement, AbstractTreeViewer.ALL_LEVELS);
			} finally {
				treeVariable.getTree().setRedraw(true);
			}
		}
	}
	
	private void treeFilesResize() {
		try {
			if (! inResize) {
				inResize = true;
				final int fullWidth = treeFiles.getTree().getSize().x - 
						(treeFiles.getTree().getBorderWidth() + treeFiles.getTree().getVerticalBar().getSize().x + 3);
				tcFiles.getColumn().setWidth(fullWidth);
			}
		} finally {
			inResize = false;
		}
	}
	
	private void treeVariablesResize() {
		try {
			if (! inResize) {
				inResize = true;
				final int fullWidth = treeVariable.getTree().getSize().x;
				final int preservedWidth = tcLvl.getColumn().getWidth() 
						+ tcVarType.getColumn().getWidth() + 20 /* 20 pixels for checkbox column */;
				tcVariable.getColumn().setWidth(fullWidth - preservedWidth);
			}
		} finally {
			inResize = false;
		}
	}
	
	private void tblStatementsResize() {
		try {
			if (! inResize) {
				inResize = true;
				final int fullWidth = getTableWidth(tblStatements.getTable());
				final int preservedWidth = tcLine.getColumn().getWidth() 
						+ tcType.getColumn().getWidth()
						+ tcStatement.getColumn().getWidth();
				tcField.getColumn().setWidth(fullWidth - preservedWidth);
			}
		} finally {
			inResize = false;
		}
	}
	
	private int getTableWidth(final Table tbl) {
		return tbl.getSize().x - (tbl.getBorderWidth() + tbl.getVerticalBar().getSize().x + 5);
	}
	
	private Shell getActiveShell() {
		return WorkbenchActions.getActiveWindowShell().orElseThrow(() -> new IllegalStateException("No shell access."));
	}
	
}
