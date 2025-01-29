/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import static innowake.mining.plugin.fieldtracer.view.WorkbenchActions.getSharedImages;

import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.ui.ISharedImages;

import innowake.ndt.fieldtracing.model.SourceFile;

/**
 * Label provider for the files tree.
 */
final class FilesLabelProvider extends CellLabelProvider {
	
	@Override
	public void update(final ViewerCell cell) {
		final SourceFile<?> srcFile = (SourceFile<?>) cell.getElement();
		cell.setText(srcFile.getName());
		
		switch (srcFile.getType()) {
			case CALLEE:
				cell.setImage(getSharedImages().getImage(ISharedImages.IMG_TOOL_FORWARD));
				break;
			case CALLER:
				cell.setImage(getSharedImages().getImage(ISharedImages.IMG_TOOL_BACK));
				break;
			case COPY:
				cell.setImage(getSharedImages().getImage(ISharedImages.IMG_OBJ_FILE));
				break;
			case ROOT:
				cell.setImage(getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
				break;
			
		}
	}
	
}
