/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Optional;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.codemining.ICodeMiningProvider;
import org.eclipse.jface.text.reconciler.MonoReconciler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.ISourceViewerExtension5;
import org.eclipse.swt.custom.LineBackgroundEvent;
import org.eclipse.swt.custom.LineBackgroundListener;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.editors.text.EditorsUI;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.base.MathUtil;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Decorates an editor with code mining {@linkplain LineHeader line headers} and {@link AnnotationPojo} highlighting.
 */
@SuppressWarnings("javadoc")
class AnnotationEditorDecorator implements IEditorDecorator, LineBackgroundListener {

	private static final WorkingState[] VISIBLE_ANNOTATION_STATES_IN_EDITOR = new WorkingState[] {
			WorkingState.IN_ANALYSIS,
			WorkingState.REJECTED,
			WorkingState.FOR_REVIEW,
			WorkingState.APPROVED,
			WorkingState.INVALID
	};

	private static final Color BACKGROUND_COLOR = EditorsUI.getSharedTextColors().getColor(new RGB(227, 240, 252)); 
	
	private AnnotationPojo[] annotations = new AnnotationPojo[0];
	@Nullable
	private ISourceViewerExtension5 viewer;
	@Nullable
	private StyledText text;
	@Nullable
	private MonoReconciler monoReconciler;

	private IFile file;

	AnnotationEditorDecorator(final IFile file) {
		this.file = file;
	}
	
	@Override
	public void lineGetBackground(@Nullable final LineBackgroundEvent event) {	
		int lineOffset = assertNotNull(event).lineOffset;
		if (viewer instanceof ITextViewerExtension5) {
			lineOffset = ((ITextViewerExtension5) viewer).widgetOffset2ModelOffset(lineOffset);
			if (lineOffset == -1) {
				return;
			}
		}
		final int lineLength = assertNotNull(event).lineText.length();
		final int lineEnd = lineOffset + lineLength - 1;
		
		for (final AnnotationPojo annotation : annotations) {
			final ModuleLocation location = annotation.getLocation().orElseThrow(() -> new IllegalStateException("Location in annotation must be present"));
			final int locationOffset = location.getOffset().intValue();
			final int locationLength = location.getLength().intValue();
			final int locationEnd = locationOffset + locationLength - 1;
			if (MathUtil.isBetweenOrEqual(locationOffset, lineOffset, lineEnd) ||
					MathUtil.isBetweenOrEqual(locationEnd, lineOffset, lineEnd) ||
					MathUtil.isBetweenOrEqual(lineOffset, locationOffset, locationEnd)) {
				assertNotNull(event).lineBackground = BACKGROUND_COLOR;
				break;
			}
		}
	}
	
	@Override
	public void install(final ISourceViewerExtension5 viewer) {
		this.viewer = viewer;
		
		final Job job = Job.create("Decorating Annotations", (@Nullable final IProgressMonitor monitor) -> {
		
			final Optional<AnnotationPojo[]> annotationsByPath = MiningServiceExecutor
					.create(() ->
						ApiClient.annotationService(file.getProject())
								.findAllAnnotations()
								.setModulePath(ResourceUtil.getProjectRelativePath(file))
								.setStates(VISIBLE_ANNOTATION_STATES_IN_EDITOR))
					.setExceptionConsumer(exception -> Logging.error("An error occurred when fetching annotations.", exception))
					.execute();

			if (monitor != null && monitor.isCanceled()) throw new OperationCanceledException();
			if (annotationsByPath.isPresent()) {
				annotations = annotationsByPath.get();
				Display.getDefault().asyncExec(() ->
					/* Annotation decoration happens in UI thread */
					viewer.setCodeMiningProviders(new ICodeMiningProvider[] {
							new CodeMiningProvider(annotations)
					}));
				}			
		});
		job.setPriority(Job.DECORATE);
		job.setSystem(false);
		job.setJobGroup(MiningJobGroup.INSTANCE);
		job.schedule();
		
		monoReconciler = new MonoReconciler(new ReconcilingStrategy(viewer), false);
		monoReconciler.install((ITextViewer) viewer);
		
		text = assertNotNull(((ISourceViewer) viewer).getTextWidget());
		text.addLineBackgroundListener(this);
	}
	
	@Override
	public void uninstall() {
		if (monoReconciler != null) {
			monoReconciler.uninstall();
		}
		if (viewer != null) {
			final ISourceViewerExtension5 viewer2 = viewer;
			((ISourceViewer) viewer2).invalidateTextPresentation();
			viewer2.setCodeMiningProviders(new ICodeMiningProvider[0]);
		}
		if (text != null) {
			final StyledText text2 = text;
			if ( ! text2.isDisposed()) {
				text2.removeLineBackgroundListener(this);
			}
		}
	}
}
