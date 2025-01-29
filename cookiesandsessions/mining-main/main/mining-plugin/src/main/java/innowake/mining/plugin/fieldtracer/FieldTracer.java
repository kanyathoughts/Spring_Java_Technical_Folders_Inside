/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.ViewPart;

import innowake.base.eclipse.core.util.ProgressAdapter;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
import innowake.mining.plugin.datadictionary.handler.EclipseCompatibleTextSelection;
import innowake.mining.plugin.fieldtracer.model.CobolLegacyTracedModule;
import innowake.mining.plugin.fieldtracer.model.NaturalLegacyTracedModule;
import innowake.mining.plugin.fieldtracer.view.FieldtracerView;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode;
import innowake.ndt.cobolclipse.core.CobolLanguage;
import innowake.ndt.cobolclipse.core.CobolServices;
import innowake.ndt.cobolclipse.core.object.ICobolObject;
import innowake.ndt.cobolclipse.ui.view.bms.BMSView;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.fieldtracing.DocumentBasedTextSelection;
import innowake.ndt.fieldtracing.SubTraceable;
import innowake.ndt.fieldtracing.TextSelection;
import innowake.ndt.fieldtracing.cobol.relatedfields.CobolCompositeRelatedFieldsResolver;
import innowake.ndt.fieldtracing.cobol.relatedfields.CobolDefaultRelatedFieldsResolver;
import innowake.ndt.fieldtracing.cobol.relatedfields.ICobolRelatedFieldsResolver;
import innowake.ndt.fieldtracing.cobol.tracer.CobolFieldTracer;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.TracedModule;
import innowake.ndt.fieldtracing.model.TracingMode;
import innowake.ndt.fieldtracing.natural.relatedfields.INaturalRelatedFieldsResolver;
import innowake.ndt.fieldtracing.natural.relatedfields.NaturalCompositeRelatedFieldsResolver;
import innowake.ndt.fieldtracing.natural.relatedfields.NaturalDefaultRelatedFieldsResolver;
import innowake.ndt.fieldtracing.natural.tracer.NaturalFieldTracer;
import innowake.ndt.ide.core.object.IIdeLegacyObject;
import innowake.ndt.natclipse.core.INaturalProjectNature;
import innowake.ndt.natclipse.core.NatclipseCore;
import innowake.ndt.natclipse.core.NaturalLanguage;
import innowake.ndt.natclipse.core.object.INaturalObject;

/**
 * 	Executes a trace on a selected field.
 */
public class FieldTracer extends AbstractHandler implements SubTraceable {

	private String viewSubId = "";

	@Override
	public void subtrace(final TextSelection fieldSelection) {
		viewSubId = fieldSelection.getText();
		executeInternal(fieldSelection, EditorActionUtil.getFile(), EditorActionUtil.getTextViewer());
	}

	@Override
	@Nullable
	public Object execute(@Nullable final ExecutionEvent event) throws ExecutionException {
		final IWorkbenchPart activePart = WorkbenchUtil.getActivePart();
		final IWorkbenchWindow activeWindow = WorkbenchUtil.getActiveWindow();
		if (activeWindow != null && activePart instanceof BMSView) {
			final BMSView activeBMSView = (BMSView) activePart;
			final StructuredSelection bmsStructuredSelection = activeBMSView.getSelection();
			final IEditorReference editorReference = getActiveBMSViewReference();
			if (bmsStructuredSelection != null && editorReference != null) {
				final BmsDfhmdfNode selectedBMSNode = (BmsDfhmdfNode) bmsStructuredSelection.getFirstElement();
				@Nullable final String id = selectedBMSNode.getId();
				if (id == null) {
					throw new IllegalStateException("Editor selection is not available.");
				}
				final DocumentBasedTextSelection textSelection = new DocumentBasedTextSelection(new Document(id), 0, StringUtils.length(id));
				executeInternal(textSelection, getActiveFile(TracingMode.BMS), getActiveTextViewer(TracingMode.BMS));
			} else {
				throw new IllegalStateException("Editor selection is not available.");
			}
		} else {
			executeInternal(new FieldTracerCompatibleITextSelection(EditorActionUtil.getSelection().orElseThrow(() -> new IllegalStateException("Editor selection is not available."))),
					getActiveFile(TracingMode.EDITOR), getActiveTextViewer(TracingMode.EDITOR));
		}
		return null;
	}

	private Optional<IFile> getActiveFile(final TracingMode mode) {
		switch (mode) {
			case EDITOR:
				return EditorActionUtil.getFile();
			case BMS:
				return Optional.ofNullable(getActiveBMSViewReference())
						.map(editorReference -> editorReference.getEditor(false))
						.filter(Objects::nonNull)
						.map(IEditorPart::getEditorInput)
						.map(FileEditorInput.class::cast)
						.map(FileEditorInput::getFile)
						.map(Optional::of)
						.orElseGet(Optional::empty);
			default:
				throw new UnsupportedOperationException(String.format(
						"The type of the provided object for field tracing '%s' is not supported: Currently only programs, copybooks, and maps can be traced.",
						mode));
		}
	}

	private Optional<ITextViewer> getActiveTextViewer(final TracingMode mode) {
		switch (mode) {
			case EDITOR:
				return EditorActionUtil.getTextViewer();
			case BMS:
				return Optional.ofNullable(getActiveBMSViewReference())
						.map(editorReference -> editorReference.getEditor(false))
						.filter(Objects::nonNull)
						.map(editor -> editor.getAdapter(ITextOperationTarget.class))
						.map(ITextViewer.class::cast)
						.map(Optional::of)
						.orElseGet(Optional::empty);
			default:
				throw new UnsupportedOperationException(String.format(
						"The type of the provided object for field tracing '%s' is not supported: Currently only programs, copybooks, and maps can be traced.",
						mode));
		}
	}

	@Nullable
	private IEditorReference getActiveBMSViewReference() {
		final IWorkbenchPart activePart = WorkbenchUtil.getActivePart();
		final IWorkbenchWindow activeWindow = WorkbenchUtil.getActiveWindow();
		if (activeWindow == null || ! (activePart instanceof BMSView)) {
			return null;
		}
		final BMSView activeBMSView = (BMSView) activePart;
		final IEditorReference[] editorReferences = activeWindow.getActivePage().getEditorReferences();
		return Arrays.asList(editorReferences).stream()
				.filter(reference -> reference.getTitle().equals(activeBMSView.getFileName()))
				.findFirst()
				.orElse(null);
	}

	/**
	 * Shows the view.
	 *
	 * @param id the secondary id
	 */
	public void showView(@Nullable final String id) {
		if (WorkbenchUtil.isWorkbenchRunning()) {
			WorkbenchUtil.syncExec(WorkbenchUtil.getDisplaySafely(), () -> {
				final IWorkbenchPage activePage = WorkbenchUtil.getActivePage();
				try {
					if (activePage != null) {
						final FieldtracerView fieldTracerView = (FieldtracerView) activePage.showView(FieldtracerView.ID, id, IWorkbenchPage.VIEW_VISIBLE);
						if (fieldTracerView != null) {
							activePage.bringToTop(fieldTracerView);
						}
					}
				} catch (final PartInitException e) {
					throw new IllegalStateException(e);
				}
			});
		} else {
			throw new IllegalStateException("Workbench is not available.");
		}
	}

	/**
	 * Returns the {@link ViewPart}
	 *
	 * @param id the secondary id of the {@link ViewPart}
	 * @return the {@link ViewPart}
	 */
	@Nullable
	public ViewPart getView(@Nullable final String id) {
		final AtomicReference<ViewPart> ref = new AtomicReference<>();

		if (WorkbenchUtil.isWorkbenchRunning()) {
			WorkbenchUtil.syncExec(WorkbenchUtil.getDisplaySafely(), () -> {
				final IWorkbenchPage activePage = WorkbenchUtil.getActivePage();

				if (activePage == null) {
					ref.set(null);
				} else {
					final IViewReference viewReference = activePage.findViewReference(FieldtracerView.ID, id);

					if (viewReference == null) {
						ref.set(null);
					} else {
						final IViewPart view = viewReference.getView(false);
						if (view instanceof ViewPart) {
							final ViewPart expertView = (ViewPart) view;
							ref.set(expertView);
						} else {
							ref.set(null);
						}
					}
				}

			});
		} else {
			return null;
		}

		return ref.get();
	}

	private void executeInternal(final TextSelection selection, final Optional<IFile> file, final Optional<ITextViewer> textViewer) {
		final String viewId;
		/* WMIN-641: Use the same view instance for non-sub-tracing and a separate one for each sub-trace.*/
		if (viewSubId.isEmpty()) {
			viewId = null;
		} else {
			viewId = FieldtracerView.ID + viewSubId;
		}
		showView(viewId);
		final ViewPart existingView = getView(viewId);
		final FieldtracerView view;
		if (existingView == null) {
			view = new FieldtracerView();
		} else {
			if (existingView instanceof FieldtracerView) {
				view = (FieldtracerView) existingView;
			} else {
				throw new IllegalStateException("Script was changed. Please close the Field Trace view and try again.");
			}
		}
		buildAndDisplayModel(view, new EclipseCompatibleTextSelection(selection), file, textViewer);
	}

	private void buildAndDisplayModel(
			final FieldtracerView view,
			final ITextSelection selection,
			final Optional<IFile> file,
			final Optional<ITextViewer> textViewer) {

		final Job job = Job.create(String.format("Start field trace for field '%s'.", selection.getText()), monitor -> {
			try {
				monitor.beginTask(String.format("Start field trace for field '%s'.", selection.getText()), IProgressMonitor.UNKNOWN);
				final Model<IFile> model = buildTracingModel(monitor, selection, file, textViewer);
				monitor.subTask("Publish to UI");
				runOnUiSync(() -> view.setModel(model));
			} catch (final Exception e) {
				runOnUiSync(() -> view.handleException("Error details.", e));
			} finally {
				monitor.done();
			}
		});
		job.schedule();
	}

	private Model<IFile> buildTracingModel(
			final IProgressMonitor monitor,
			final ITextSelection textSelection,
			final Optional<IFile> optFile,
			final Optional<ITextViewer> optTextViewer) throws CoreException {

		final AtomicReference<Optional<ITextViewer>> textViewerOptRef = new AtomicReference<>(Optional.empty());
		final AtomicReference<Optional<IFile>> fileOptRef  = new AtomicReference<>(Optional.empty());
		runOnUiSync(() -> {
			textViewerOptRef.set(optTextViewer);
			fileOptRef.set(optFile);
		});

		final ITextViewer textViewer = textViewerOptRef.get().orElseThrow(() -> new IllegalStateException("Invalid eclipse state."));
		final IFile file = fileOptRef.get().orElseThrow(() -> new IllegalStateException("Invalid eclipse state."));
		if (textViewer.getDocument() == null) {
			throw new IllegalStateException("Invalid eclipse state.");
		}

		final IIdeLegacyObject<?> ideObject = getIdeObject(file);
		final ProgressAdapter progressAdapter = new ProgressAdapter(monitor);

		if (CobolLanguage.is(ideObject.getLanguage())) {
			final ICobolRelatedFieldsResolver<IFile> resolver =
					new CobolCompositeRelatedFieldsResolver<>(new CobolDefaultRelatedFieldsResolver<>(progressAdapter), progressAdapter);
			final TracedModule<IFile> cobolModule = new CobolLegacyTracedModule((ICobolObject) ideObject);
			final TextSelection cobolTokenSelection = new FieldTracerCompatibleITextSelection(
					SelectionUtil.getTokenSelection(cobolModule, textSelection).orElseThrow(() -> new IllegalStateException(
							String.format("Could not determine token selection for %s on %s.", textSelection, ideObject.getObjectPath()))));
			return new CobolFieldTracer<>(progressAdapter, resolver).trace(cobolModule, cobolTokenSelection);
		}
		if (NaturalLanguage.is(ideObject.getLanguage())) {
			final INaturalRelatedFieldsResolver<IFile> resolver =
					new NaturalCompositeRelatedFieldsResolver<>(new NaturalDefaultRelatedFieldsResolver<>(progressAdapter), progressAdapter);
			final TracedModule<IFile> natModule = new NaturalLegacyTracedModule((INaturalObject) ideObject);
			final TextSelection natTokenSelection = new FieldTracerCompatibleITextSelection(
					SelectionUtil.getTokenSelection(natModule, textSelection).orElseThrow(() -> new IllegalStateException(
							String.format("Could not determine token selection for %s on %s.", textSelection, ideObject.getObjectPath()))));
			return new NaturalFieldTracer<>(resolver, progressAdapter).trace(natModule, natTokenSelection);
		}
		throw new IllegalStateException(String.format("Unsupported language %s for field tracing.", ideObject.getLanguage()));
	}

	private IIdeLegacyObject<?> getIdeObject(final IFile file) throws CoreException {
		IIdeLegacyObject<?> ideObject = CobolServices.get().getWorkspaceManager().findObject(file);
		if (ideObject == null) {
			final INaturalProjectNature nature = NatclipseCore.getNaturalProjectNature(file);
			if (nature != null) {
				ideObject = nature.getObjectManager().findObject(file);
			}
		}
		if (ideObject == null) {
			throw new IllegalArgumentException(String.format("Failed to retrieve the IIdeLegacyObject for IFile '%s'. "
					+ "This might be due to a missing Cobol or Natural Nature or a missing/incorrect .cobol-path or .nat-path file.", file));
		}
		return ideObject;
	}

	/*
	 * Runs the given runnable synchronously on the UI thread.
	 *
	 * This is useful when interacting with views.
	 *
	 * @param runnable the runnable to run synchronously on the UI thread
	 * @throws IllegalStateException when the workbench is not available
	 */
	private void runOnUiSync(final Runnable runnable) {
		if (WorkbenchUtil.isWorkbenchRunning()) {
			WorkbenchUtil.syncExec(WorkbenchUtil.getDisplaySafely(), runnable);
		} else {
			throw new IllegalStateException("Workbench is not available");
		}
	}
}