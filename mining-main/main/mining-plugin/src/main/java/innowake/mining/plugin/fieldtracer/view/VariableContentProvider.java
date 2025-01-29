/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.ITreeContentProvider;

import innowake.ndt.fieldtracing.model.tree.TreeNode;

/**
 * Tree content provider.
 * Accept an array of {@code TreeNode}'s.
 */
final class VariableContentProvider implements ITreeContentProvider {

	@Override
	public Object[] getElements(Object inputElement) {
		return (Object[]) inputElement;
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		return (( (TreeNode<?>) parentElement).getChildren()).toArray();
	}

	@Override
	public Object getParent(Object element) {
		return ( (TreeNode<?>) element).getParent();
	}

	@Override
	public boolean hasChildren(Object element) {
		return ( (TreeNode<?>) element).hasChildren();
	}

}
