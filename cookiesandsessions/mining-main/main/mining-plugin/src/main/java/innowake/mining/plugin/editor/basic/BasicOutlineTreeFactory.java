/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import java.util.List;
import java.util.Optional;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

import innowake.base.eclipse.common.ui.util.ImageDescriptorRegistry;
import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.spi.Location;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicNode;
import innowake.ndt.parsing.parser.basic.OutlineNode;

/**
 * Class traverses the AST and provides the root Outline node.
 */
public class BasicOutlineTreeFactory {
	
	private final ImageDescriptorRegistry imageRegistry;
	
	/**
	 * Constructor to initialize Tree factory.
	 */
	public BasicOutlineTreeFactory() {
		imageRegistry = new ImageDescriptorRegistry();
	}
	
	/**
	 * Traverses the AST and returns the root Outline node.
	 * 
	 * @param basicAst the model returned after parsing
	 * @return root outline node
	 */
	@Nullable
	public final BasicOutlineNode create(final BasicModel basicAst) {
		final Optional<AstNode> rootNode = basicAst.getRoot();
		if (rootNode.isPresent() && rootNode.get() instanceof BasicNode) {
			final BasicNode root = (BasicNode) rootNode.get();
			final BasicOutlineNode result = new BasicOutlineNode(root);
			traverse(result, root);
			return result;
		}
		return null;
	}

	private void traverse(final BasicOutlineNode outlineNode, final AstNode astNode) {
		final List<AstNode> children = astNode.getChildren();
		for (final AstNode astChild : children) {
			if (astChild instanceof BasicNode && astChild instanceof OutlineNode) {
				final BasicNode child = (BasicNode) astChild;
				final BasicOutlineNode outlineChild = createBasicOutlineNode(outlineNode, child);
				outlineNode.addChild(outlineChild);
				traverse(outlineChild, child);
			}
		}
	}

	private BasicOutlineNode createBasicOutlineNode(final BasicOutlineNode parent, final BasicNode node) {
		final Image image = getImage(BasicImages.OUTLINE_NODE_BASIC);
		if ( ! (node instanceof OutlineNode)) {
			throw new IllegalArgumentException("The Node is not an instance of :" + OutlineNode.class.getSimpleName());
		}
		final String label = ((OutlineNode) node).getOutlineLabel();
		final BasicOutlineNode result = new BasicOutlineNode(parent, node, label, image);
		result.setEditorLocation(new Location(node.getStartOffset(), node.getLength()));
		return result;
	}

	private Image getImage(final ImageDescriptor desc) {
		return imageRegistry.get(desc);
	}

}
