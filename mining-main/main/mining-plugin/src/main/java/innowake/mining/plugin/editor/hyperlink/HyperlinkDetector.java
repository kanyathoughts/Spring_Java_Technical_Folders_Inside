/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.hyperlink;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.AbstractHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.editor.model.GenericModelProvider;
import innowake.mining.plugin.editor.model.ModelResult;
import innowake.mining.plugin.editor.model.ModelResult.Status;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.ide.IdeInternalLinkTarget;
import innowake.ndt.core.parsing.ast.model.ide.IdeLinkNode;
import innowake.ndt.core.parsing.ast.model.ide.IdeLinkTarget;

/**
 * Generic Hyperlink detector.
 */
public class HyperlinkDetector extends AbstractHyperlinkDetector {

	@Override
	@Nullable
	public IHyperlink[] detectHyperlinks(@Nullable final ITextViewer textViewer, @Nullable final IRegion region, final boolean canShowMultipleHyperlinks) {
		if (textViewer != null && region != null) {
			final IDocument document = textViewer.getDocument();
			final Optional<ModelResult> modelResult = GenericModelProvider.getInstance().getModel(document);
			final int offset = region.getOffset();
			if (modelResult.isPresent() && modelResult.get().getStatus().equals(Status.OK)) {
				final AstModel model = modelResult.get().getModel();
				if (model != null) {
					final IHyperlink[] hyperlinks = createHyperlinksFromAstModel(offset, model);
					if (hyperlinks != null) {
						return hyperlinks;
					}
				}
				final IHyperlink[] externalHyperlinks = new IHyperlink[1];
				final IdeHyperlink externalHyperlink = MiningBasedExternalLinkResolver.getInstance().resolve(document, offset);
				if (externalHyperlink != null) {
					externalHyperlinks[0] = externalHyperlink;
					return externalHyperlinks;
				}
			}
		}
		return null;
	}
	@Nullable
	private IHyperlink[] createHyperlinksFromAstModel(final int offset, final AstModel model) {
		final List<AstNode> nodes = model.getNode(offset);
		final IdeLinkNode ideLinkNode = getIdeLinkNode(nodes);
		if (ideLinkNode != null) {
			return createHyperlinksFromIdeLinkNodes(ideLinkNode);
		}
		return null;
	}
	
	@Nullable
	private IHyperlink[] createHyperlinksFromIdeLinkNodes(final IdeLinkNode linkNode) {
		final Optional<IdeLinkTarget> optionalLinkTarget = linkNode.getLinkTarget();
		final Optional<AstNode> optionalLinkSource = linkNode.getLinkSource();
		if (optionalLinkTarget.isPresent() && optionalLinkSource.isPresent()) {
			final AstNode astLinkNode = optionalLinkSource.get();
			final IdeLinkTarget ideLinkTarget = optionalLinkTarget.get();
			if (ideLinkTarget instanceof IdeInternalLinkTarget) {
				final IdeInternalLinkTarget ideInternalTarget = (IdeInternalLinkTarget) ideLinkTarget;
				final AstNode targetNode = ideInternalTarget.getLinkedNode();
				return createHyperlink(astLinkNode.getStartOffset(), astLinkNode.getLength(), targetNode.getStartOffset(), targetNode.getLength());
			}
		}
		return null;
	}
	
	@Nullable
	private IdeLinkNode getIdeLinkNode(final List<AstNode> nodes) {
		return nodes.stream()
		            .map(node -> (node).getAdapter(IdeLinkNode.class))
		            .filter(Objects::nonNull)
		            .findFirst()
		            .orElse(null);
	}
	
	private IHyperlink[] createHyperlink(final int offset, final int length, final int targetOffset, final int targetLength) {
		final IHyperlink[] links = new IHyperlink[1];
		final IRegion region = new Region(offset, length);
		final IRegion targetRegion = new Region(targetOffset, targetLength);
		links[0] = new IdeInternalHyperlink(region, PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage(), targetRegion);
		return links;
	}
	
	private class IdeInternalHyperlink extends IdeHyperlink {
		
		private final IWorkbenchPage page;
		
		/**
		 * Constructor to initialize {@link IdeInternalHyperlink}.
		 * 
		 * @param region region of the hyperlink
		 * @param page active workbench page of the hyperlink
		 * @param targetRegion of the hyperlink
		 */
		IdeInternalHyperlink(final IRegion region, final IWorkbenchPage page, final IRegion targetRegion) {
			super(region, targetRegion);
			this.page = page;
		}
		
		@Override
		public void open() {
			final IEditorPart editor = page.getActiveEditor();
			final IRegion targetRegion = getTargetRegion();
			if (editor instanceof AbstractDecoratedTextEditor) {
				((AbstractDecoratedTextEditor) editor).selectAndReveal(targetRegion.getOffset(), targetRegion.getLength());
			}
			editor.setFocus();
		}
		
	}
 
}
