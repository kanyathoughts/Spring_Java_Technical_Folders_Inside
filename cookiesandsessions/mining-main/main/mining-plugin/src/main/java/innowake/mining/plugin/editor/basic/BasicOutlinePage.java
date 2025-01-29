/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.texteditor.IElementStateListener;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.ide.ui.view.GenericTreeLabelProvider;
import innowake.ndt.ide.ui.view.GenericTreeUtil;
import innowake.ndt.ide.ui.view.ISelectableFromEditor;

/**
 * Outline page for BASIC language.
 */
public class BasicOutlinePage extends ContentOutlinePage implements ISelectableFromEditor {

	private final ITextEditor editor;
	@Nullable private IElementStateListener elementStateListener;
	@Nullable private ISelectionChangedListener selectionListener;
	@Nullable private IPropertyListener propertyListener;
	private boolean suppressEditorSelection;

	/**
	 * Constructor to initialize BASIC outline.
	 * 
	 * @param textEditor in which the file, for which outline is required, is opened.
	 */
	public BasicOutlinePage(final ITextEditor textEditor) {
		editor = textEditor;
	}

	@Override
	public void createControl(@Nullable final Composite parent) {
		super.createControl(parent);
		final TreeViewer viewer = getTreeViewer();
		viewer.setContentProvider(new BasicOutlineContentProvider(editor));
		viewer.setLabelProvider(new GenericTreeLabelProvider());
		viewer.setInput(editor.getEditorInput());
		final Runnable refreshRunnable = this::refresh;
		this.elementStateListener = new IElementStateListener() {

			@Override
			public void elementMoved(@Nullable final Object originalElement, @Nullable final Object movedElement) {
				/*Currently not overriding parent functionality.*/
			}

			@Override
			public void elementDirtyStateChanged(@Nullable final Object element, final boolean isDirty) {
				if ( ! isDirty) {
					WorkbenchUtil.syncExec(getSite(),  refreshRunnable);
				}
			}

			@Override
			public void elementDeleted(@Nullable final Object element) {
				/*Currently not overriding parent functionality.*/
			}

			@Override
			public void elementContentReplaced(@Nullable final Object element) {
				/*Currently not overriding parent functionality.*/
			}

			@Override
			public void elementContentAboutToBeReplaced(@Nullable final Object element) {
				/*Currently not overriding parent functionality.*/
			}
		};
		editor.getDocumentProvider().addElementStateListener(elementStateListener);

		this.selectionListener = event -> {
			if ( ! suppressEditorSelection) {
				GenericTreeUtil.synchronizeSelection(getTreeViewer(), editor);
			}
		};
		getTreeViewer().addSelectionChangedListener(selectionListener);

		this.propertyListener = (source, propId) -> {
			if (propId == IEditorPart.PROP_INPUT) {
				final TreeViewer treeViewer = getTreeViewer();
				treeViewer.setInput(editor.getEditorInput());
				treeViewer.expandToLevel(2);
			}
		};
		editor.addPropertyListener(propertyListener);
		refresh();
		viewer.expandToLevel(2);
	}

	@Override
	public void selectFromEditor(final int caretOffset) {
		try {
			suppressEditorSelection = true;
			GenericTreeUtil.synchronizeSelection(getTreeViewer(), caretOffset);
		} finally {
			suppressEditorSelection = false;
		}
	}

	private void refresh() {
		final TreeViewer viewer = getTreeViewer();
		viewer.refresh();
	}

	@Override
	public void dispose() {
		try {
			editor.getDocumentProvider().removeElementStateListener(elementStateListener);
		} catch (final NullPointerException e) {
			throw new IllegalStateException("Could not remove element state listener properly", e);
		}
		try {
			editor.removePropertyListener(propertyListener);
		} catch (final NullPointerException e) {
			throw new IllegalStateException("Could not remove property listener properly", e);
		}
		try {
			getTreeViewer().removeSelectionChangedListener(selectionListener);
		} catch (final NullPointerException e) {
			throw new IllegalStateException("Could not remove selection listener properly", e);
		}
		super.dispose();
	}

}
