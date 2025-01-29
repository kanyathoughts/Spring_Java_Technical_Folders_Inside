/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import org.eclipse.swt.graphics.Image;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.ide.ui.view.GenericEditorTreeNode;
import innowake.ndt.parsing.parser.basic.BasicNode;

/**
 * Class representing the node in the outline page for BASIC.
 */
public class BasicOutlineNode extends GenericEditorTreeNode {

	private final BasicOutlineNode root;
	private final int offset;

	/**
	 * Constructor to initialize a basic outline node.
	 * 
	 * @param node of Basic AST model
	 */
	BasicOutlineNode(final BasicNode node) {
		this.targetObject = node;
		this.root = this;
		this.parent = null;
		if ( ! node.isRoot()) {
			offset = node.getStartOffset();
		} else {
			offset = -1;
		}
	}

	/**
	 * Constructor to initialize a basic outline node.
	 * 
	 * @param node of Basic AST model
	 * @param parent the parent outline node of the outline node to be created
	 * @param label the display label for the outline node in the outline page
	 * @param image the image to be used for the node in the outline page
	 */
	BasicOutlineNode(final BasicOutlineNode parent, final BasicNode node, final String label, final Image image) {
		super(image, label, node);
		this.targetObject = node;
		this.root = parent.getRoot();
		this.parent = parent;
		if ( ! node.isRoot()) {
			offset = node.getStartOffset();
		} else {
			offset = -1;
		}
	}

	/**
	 * @return offset for the outline node
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * Returns the tree root node.
	 *
	 * @return the root
	 */
	public BasicOutlineNode getRoot() {
		return root;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if ( ! (obj instanceof BasicOutlineNode)) {
			return false;
		}
		final BasicOutlineNode other = (BasicOutlineNode) obj;
		return offset == other.offset;
	}

	/**
	 * @return the AST node for which the {@link BasicOutlineNode} was created
	 */
	BasicNode getBasicNode() {
		return (BasicNode) targetObject;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

}
