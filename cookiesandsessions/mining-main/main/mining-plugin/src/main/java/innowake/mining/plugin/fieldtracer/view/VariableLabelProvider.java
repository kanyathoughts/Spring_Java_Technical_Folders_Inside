/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ViewerCell;

import innowake.ndt.fieldtracing.model.FieldDefinition;
import innowake.ndt.fieldtracing.model.SourceFile;
import innowake.ndt.fieldtracing.model.SourceFileOrFieldDefinition;
import innowake.ndt.fieldtracing.model.tree.TreeNode;

/**
 * Variable table label provider.
 */
public class VariableLabelProvider extends CellLabelProvider {

	@SuppressWarnings("unchecked")
	@Override
	public void update(final ViewerCell cell) {
 		final TreeNode<SourceFileOrFieldDefinition<IFile>> defNode = (TreeNode<SourceFileOrFieldDefinition<IFile>>) cell.getElement();
		if (defNode.getData().isFieldDefinition()) {
			update(cell, defNode.getData().getFieldDefinition());
		}
		if (defNode.getData().isSourceFile()) {
			update(cell, defNode.getData().getSourceFile());
		}
	}
	
	private void update(final ViewerCell cell, final FieldDefinition<IFile> node) {
		switch (cell.getColumnIndex()) {
			case 0:
				cell.setText(node.getFieldName());
				break;
			case 1:
				cell.setText(node.getType().name());
				break;
			case 2:
				cell.setText(node.getLevel().map(Object::toString).orElseGet( () -> "" ));
				break;
			default:
				cell.setText("");
				break;
		}	
	}
	
	private void update(final ViewerCell cell, final SourceFile<IFile> sourceFile) {
		switch (cell.getColumnIndex()) {
			case 0:
				cell.setText(sourceFile.getDefinitions().isEmpty() ? sourceFile.getName() + " (not further traceable)" : sourceFile.getName());
				break;
			case 1:
				cell.setText("");
				break;
			default:
				cell.setText("");
				break;
		}	
	}
}
