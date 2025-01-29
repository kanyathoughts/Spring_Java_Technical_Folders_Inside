/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.function.Consumer;

import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.FileEditorInput;

import innowake.lib.core.api.lang.Nullable;

/**
 * Used to listen and notify on editor page changes.
 */
public class EditorPartListener implements IPartListener {

	@Nullable
	private Consumer<FileEditorInput> editorOpenConsumer;
	@Nullable
	private Consumer<FileEditorInput> editorChangeConsumer;
	@Nullable
	private Consumer<FileEditorInput> editorCloseConsumer;
	@Nullable
	private FileEditorInput lastEditor = null;
	
	/**
	 * Register the part listener on a given page.
	 * 
	 * @param page Instance of the page to register the listener on.
	 */
	public void activate(final IWorkbenchPage page) {
		page.addPartListener(this);
	}
	
	/**
	 * Remove the part listener from a given page.
	 * 
	 * @param page Instance of the page to register the listener on.
	 */
	public void deactivate(final IWorkbenchPage page) {
		page.removePartListener(this);
	}

	/**
	 * Function to be called if an editor opened.
	 * 
	 * @param consumer consumer function
	 */
	public void addEditorOpenConsumer(final Consumer<FileEditorInput> consumer) {
		this.editorOpenConsumer = consumer;
	}
	
	/**
	 * Function to be called if an editor activates.
	 * <p>
	 * This is only triggered when the actual editor changes.
	 * 
	 * @param consumer consumer function
	 */
	public void addEditorChangeConsumer(final Consumer<FileEditorInput> consumer) {
		this.editorChangeConsumer = consumer;
	}
	
	/**
	 * Function to be called if the editor closed.
	 * 
	 * @param consumer consumer function
	 */
	public void addEditorCloseConsumer(final Consumer<FileEditorInput> consumer) {
		this.editorCloseConsumer = consumer;
	}
	
	@Override
	public void partActivated(@Nullable final IWorkbenchPart part) {
		IEditorInput editorInput = null;
		if (part instanceof IEditorPart) {
			editorInput = ((IEditorPart) part).getEditorInput();
		}
		
		if (editorInput != lastEditor) {
			consume(editorChangeConsumer, part);
		}
	}

	@Override
	public void partBroughtToTop(@Nullable final IWorkbenchPart part) {
		/* nothing to handle, appears if there are 2 editors on a single pane, 1 not visible and getting visible, after that partActivated is always triggered */
	}
	
	@Override
	public void partOpened(@Nullable final IWorkbenchPart part) {
		consume(editorOpenConsumer, part);
	}

	@Override
	public void partClosed(@Nullable final IWorkbenchPart part) {
		consume(editorCloseConsumer, part);
	}

	@Override
	public void partDeactivated(@Nullable final IWorkbenchPart part) {
		/* nothing to handle */
	}
	
	private void consume(@Nullable final Consumer<FileEditorInput> consumer, @Nullable final IWorkbenchPart part) {
		if (part instanceof IEditorPart && consumer != null) {
			final IEditorInput editorInput = ((IEditorPart) part).getEditorInput();
			if (editorInput instanceof FileEditorInput) {
				lastEditor = (FileEditorInput) editorInput;
				consumer.accept(lastEditor);
			}
		}
	}

	/**
	 * Sets the editor input.
	 * <p>
	 * This can be used for setting an initial input.
	 *
	 * @param editorInput the editor input to set
	 */
	public void setEditorInput(final IEditorInput editorInput) {
		if (editorInput instanceof FileEditorInput) {
			lastEditor = (FileEditorInput) editorInput;
		}
	}
}
