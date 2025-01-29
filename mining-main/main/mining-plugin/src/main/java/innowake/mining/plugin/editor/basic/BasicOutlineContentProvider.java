/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.ide.ui.view.IGenericTreeRootProvider;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicParser;

/**
 * Content provider for the Outline page for BASIC language.
 */
public class BasicOutlineContentProvider implements ITreeContentProvider, IGenericTreeRootProvider {

	private static final BasicOutlineNode[] EMPTY = new BasicOutlineNode[0];
	

	private final ITextEditor editor;
	private final BasicOutlineTreeFactory factory;
	@Nullable private BasicOutlineNode root;
	private final BasicParser<String> basicParser;
	
	/**
	 * Constructor to initialize Outline content provider.
	 * 
	 * @param editor contents' of which the outline has to be provided
	 */
	BasicOutlineContentProvider(final ITextEditor editor) {
		this.editor = editor;
		factory = new BasicOutlineTreeFactory();
		basicParser = new BasicParser<>(new BaseParserConfiguration.Builder<String>()
				.build()) ;
	}

	@Override
	public Object[] getChildren(@Nullable final Object parentElement) {
		if (parentElement instanceof BasicOutlineNode) {
			final BasicOutlineNode treeNode = (BasicOutlineNode) parentElement;
			return treeNode.getChildren();
		}
		return EMPTY;
	}

	@Override
	public Object[] getElements(@Nullable final Object inputElement) {
		final IEditorInput editorInput = this.editor.getEditorInput();
		final IDocumentProvider provider = this.editor.getDocumentProvider();
		final IDocument document = provider.getDocument(editorInput);
		final String documentContents = document.get();
		final Optional<BasicModel> optionalModel = basicParser.parse(documentContents);
		if (optionalModel.isPresent()) {
			final BasicModel model = optionalModel.get();
			root = factory.create(model);
			if (root != null) {
				final Object[] children = root.getChildren();
				final List<Object> astChildren = Arrays.asList(children);
				final List<BasicOutlineNode> sortedChildren = astChildren.stream()
						.map(BasicOutlineNode.class::cast)
						.sorted(Comparator.comparingInt(BasicOutlineNode::getOffset))
						.collect(Collectors.toList());

				return sortedChildren.toArray();
			}
		}
		return new Object[0];
	}

	@Override
	@Nullable
	public Object getParent(@Nullable final Object element) {
		if (element instanceof BasicOutlineNode) {
			final BasicOutlineNode treeNode = (BasicOutlineNode) element;
			return treeNode.getParent();
		}
		return element;
	}

	@Override
	public boolean hasChildren(@Nullable final Object element) {
		if (element instanceof BasicOutlineNode) {
			final BasicOutlineNode treeNode = (BasicOutlineNode) element;
			return treeNode.hasChildren();
		}
		return false;
	}

	@Override
	@Nullable
	public BasicOutlineNode getRoot() {
		return root;
	}

}
