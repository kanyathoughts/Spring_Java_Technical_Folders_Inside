/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.part.ViewPart;

import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.manager.RemoteJobManager.StateUpdateListener;
import innowake.mining.job.plugin.view.action.ClearAction;
import innowake.mining.job.plugin.view.action.RefreshAction;
import innowake.mining.job.plugin.view.action.RemoveAction;
import innowake.mining.job.plugin.view.labelprovider.AbstractColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.DateColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.JobColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.LogsColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.ProjectColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.ResultColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.StateColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.StepColumnLabelProvider;
import innowake.mining.job.plugin.view.labelprovider.TimeColumnLabelProvider;

/**
 * This is the job view that provides an overview of all remote jobs that are currently known
 * to this workspace instance.
 */
public class JobView extends ViewPart {
	
	private final RemoteJobManager remoteJobManager;
	
	@Nullable
	private TreeViewer treeViewer;
	@Nullable
	private IStyledLabelProvider stateLabelProvider;
	@Nullable
	private IStyledLabelProvider jobLabelProvider;
	@Nullable
	private IStyledLabelProvider stepLabelProvider;
	@Nullable
	private IStyledLabelProvider dateLabelProvider;
	@Nullable
	private IStyledLabelProvider timeLabelProvider;
	@Nullable
	private IStyledLabelProvider resultLabelProvider;
	@Nullable
	private IStyledLabelProvider projectLabelProvider;
	@Nullable
	private Action refreshAction;
	@Nullable
	private Action removeAction;
	@Nullable
	private ResultColumnClickListener resultClickListener;
	@Nullable
	private IStyledLabelProvider logsLabelProvider;
	@Nullable
	private LogsColumnClickListener logsClickListener;
	/* we initially start in offline mode until receiving valid data */
	private boolean isOffline = true;
	
	/**
	 * Constructor.
	 */
	public JobView() {
		this.remoteJobManager = MiningJobPlugin.getDefaultNonNull().getRemoteJobManager();
	}
	
	@Override
	public void createPartControl(@Nullable final Composite parent) {
		final TreeViewer viewer = treeViewer = new TreeViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		viewer.setContentProvider(new JobViewContentProvider());
		viewer.getTree().setHeaderVisible(true);
		viewer.getTree().setLinesVisible(true);
		viewer.addDoubleClickListener(event -> {
			final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
			if (viewer.getExpandedState(selection.getFirstElement())) {
				viewer.collapseToLevel(selection.getFirstElement(), AbstractTreeViewer.ALL_LEVELS);
			} else {
				viewer.expandToLevel(selection.getFirstElement(), AbstractTreeViewer.ALL_LEVELS);
			}
		});
		createLabelProviders(viewer);
		GridLayoutFactory.fillDefaults().generateLayout(parent);
		
		createToolbar();
		createContextMenu(viewer);
		
		final RemoteJobInfo[] remoteJobInfos = remoteJobManager.getJobs().values().stream()
				.flatMap(rji -> rji.stream())
				.collect(Collectors.toSet())
				.toArray(new RemoteJobInfo[] {});
		viewer.setInput(remoteJobInfos);
		
		remoteJobManager.setStateUpdateListener(new StateUpdateListener() {
			
			@Override
			public void remoteJobsUpdated() {
				refreshContent();
			}
			
			@Override
			public void transitionedConnectionMode(final boolean offline) {
				updateConnectionState(offline);
			}
		});
	}

	@Override
	public void setFocus() {
		if (treeViewer != null) {
			treeViewer.getControl().setFocus();
		}
	}
	
	@Override
	public void dispose() {
		super.dispose();
		remoteJobManager.removeStateUpdateListener();
	}
	
	private void createLabelProviders(final TreeViewer viewer) {
		this.stateLabelProvider = new StateColumnLabelProvider();
		final TreeViewerColumn stateColumn = new TreeViewerColumn(viewer, SWT.NONE);
		stateColumn.getColumn().setText("State");
		stateColumn.getColumn().setWidth(180);
		stateColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(stateLabelProvider));
		
		this.jobLabelProvider = new JobColumnLabelProvider();
		final TreeViewerColumn jobColumn = new TreeViewerColumn(viewer, SWT.NONE);
		jobColumn.getColumn().setText("Job");
		jobColumn.getColumn().setWidth(300);
		jobColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(jobLabelProvider));
		
		this.stepLabelProvider = new StepColumnLabelProvider();
		final TreeViewerColumn stepColumn = new TreeViewerColumn(viewer, SWT.NONE);
		stepColumn.getColumn().setText("Step");
		stepColumn.getColumn().setWidth(600);
		stepColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(stepLabelProvider));
		
		this.dateLabelProvider = new DateColumnLabelProvider();
		final TreeViewerColumn dateColumn = new TreeViewerColumn(viewer, SWT.NONE);
		dateColumn.getColumn().setText("Date");
		dateColumn.getColumn().setWidth(250);
		dateColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(dateLabelProvider));
		
		this.timeLabelProvider = new TimeColumnLabelProvider();
		final TreeViewerColumn timeColumn = new TreeViewerColumn(viewer, SWT.NONE);
		timeColumn.getColumn().setText("Time");
		timeColumn.getColumn().setWidth(200);
		timeColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(timeLabelProvider));
		
		this.logsLabelProvider = new LogsColumnLabelProvider();
		final TreeViewerColumn logsColumn = new TreeViewerColumn(viewer, SWT.NONE);
		logsColumn.getColumn().setText("Logs");
		logsColumn.getColumn().setWidth(100);
		logsColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(logsLabelProvider));
		/* As the TreeViewer doesn't support embedding elements like hyperlinks or buttons,
		 * we use a click listener that reacts to clicks on the whole "Logs" table cell as a workaround. */
		logsClickListener = new LogsColumnClickListener(viewer, viewer.getTree().getColumnCount() - 1);
		logsColumn.getViewer().getControl().addMouseListener(logsClickListener);
		
		this.resultLabelProvider = new ResultColumnLabelProvider();
		final TreeViewerColumn resultColumn = new TreeViewerColumn(viewer, SWT.NONE);
		resultColumn.getColumn().setText("Result");
		resultColumn.getColumn().setWidth(100);
		resultColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(resultLabelProvider));
		/* As the TreeViewer doesn't support embedding elements like hyperlinks or buttons,
		 * we use a click listener that reacts to clicks on the whole "Result" table cell as a workaround. */
		resultClickListener = new ResultColumnClickListener(viewer, viewer.getTree().getColumnCount() - 1);
		resultColumn.getViewer().getControl().addMouseListener(resultClickListener);
		
		this.projectLabelProvider = new ProjectColumnLabelProvider();
		final TreeViewerColumn projectColumn = new TreeViewerColumn(viewer, SWT.NONE);
		projectColumn.getColumn().setText("Project");
		projectColumn.getColumn().setWidth(200);
		projectColumn.setLabelProvider(new DelegatingStyledCellLabelProvider(projectLabelProvider));
	}
	
	private void createToolbar() {
		final IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();
		this.refreshAction = new RefreshAction();
		/* we initially start in offline mode until receiving valid data */
		this.refreshAction.setEnabled(false);
		toolBarManager.add(refreshAction);
		toolBarManager.add(new ClearAction(remoteJobManager));
	}
	
	private void createContextMenu(final TreeViewer viewer) {
		final MenuManager contextMenu = new MenuManager("Job View Menu");
		removeAction = new RemoveAction(remoteJobManager, viewer);
		contextMenu.add(removeAction);
		
		contextMenu.addMenuListener(new IMenuListener() {
			
			@Override
			public void menuAboutToShow(@Nullable final IMenuManager mgr) {
				if (mgr != null) {
					final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
					final Object element = selection.getFirstElement(); /* TreeViewer is single select */
					final boolean isActiveJob = element instanceof RemoteJobInfo && RemoteJobManager.isJobInActiveState(((RemoteJobInfo) element).getJobInfo());
					
					/* Disable the context menu action when in offline mode or when the job is currently active. */
					Assert.assertNotNull(removeAction).setEnabled( ! isOffline && ! isActiveJob);
				}
			}
		});
		final Menu menu = contextMenu.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
	}
	
	private void refreshContent() {
		WorkbenchUtil.getWorkbench().getDisplay().asyncExec(() -> {
			if (treeViewer != null) {
				final TreeViewer viewer = treeViewer;
				/* Save current expanded elements to re-expand them after setting the new content. */
				final Object[] expandedElements = viewer.getExpandedElements();
				
				final RemoteJobInfo[] newContent = remoteJobManager.getJobs().values().stream()
						.flatMap(rji -> rji.stream())
						.collect(Collectors.toSet())
						.toArray(new RemoteJobInfo[] {});
				viewer.setInput(newContent);
				
				/* Get all elements of the new content whose job Id match the previous expanded to re-expand them in the tree. */
				final List<Object> newExpandedElements = new ArrayList<>();
				for (final Object expandedElement : expandedElements) {
					if (expandedElement instanceof RemoteJobInfo) {
						for (final RemoteJobInfo newRemoteJobInfo : newContent) {
							if (newRemoteJobInfo.getJobInfo().getJobId().equals(((RemoteJobInfo) expandedElement).getJobInfo().getJobId())) {
								newExpandedElements.add(newRemoteJobInfo);
							}
						}
					}
				}
				viewer.setExpandedElements(newExpandedElements.toArray());
			}
		});
	}
	
	private void updateConnectionState(final boolean offline) {
		if (isOffline != offline) {
			if (refreshAction != null) {
				refreshAction.setEnabled( ! offline);
			}
			
			if (resultClickListener != null) {
				resultClickListener.setOffline(offline);
			}
			
			if (logsClickListener != null) {
				logsClickListener.setOffline(offline);
			}
			
			if (stateLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) stateLabelProvider).setOffline(offline);
			}
			if (jobLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) jobLabelProvider).setOffline(offline);
			}
			if (stepLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) stepLabelProvider).setOffline(offline);
			}
			if (dateLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) dateLabelProvider).setOffline(offline);
			}
			if (timeLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) timeLabelProvider).setOffline(offline);
			}
			if (resultLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) resultLabelProvider).setOffline(offline);
			}
			if (logsLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) logsLabelProvider).setOffline(offline);
			}
			if (projectLabelProvider instanceof AbstractColumnLabelProvider) {
				((AbstractColumnLabelProvider) projectLabelProvider).setOffline(offline);
			}
		
			WorkbenchUtil.getWorkbench().getDisplay().asyncExec(() -> {
				if (treeViewer != null) {
					treeViewer.refresh();
				}
			});
		}
		isOffline = offline;
	}
}
