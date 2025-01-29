/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.ITreeContentProvider;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.SourceFile;

/**
 * Tree content provider which accept a {@code V2Model} as input and
 * visualize the source files of this model as tree.
 * @see Model#getSourceFiles()
 * @see SourceFile#getChildren()  
 */
final class FilesContentProvider implements ITreeContentProvider {

	@Override
	public Object[] getElements(final Object inputElement) {
		if (inputElement instanceof Model) {
			return ((Model<?>) inputElement).getSourceFiles().toArray();
		} else if (inputElement instanceof SourceFile) {
			return ((SourceFile<?>) inputElement).getChildren().toArray();
		}
		throw new IllegalStateException("Unexpected element " + inputElement);
	}

	@Override
	public Object[] getChildren(final Object parentElement) {
		if (parentElement instanceof SourceFile) {
			return ((SourceFile<?>) parentElement).getChildren().toArray();
		}
		return new Object[0];
	}

	@Override
	public Object getParent(final Object element) {
		if (element instanceof SourceFile) {
			return ((SourceFile<?>) element).getParent().orElseGet(() -> null);
		}
		return null;
	}

	@Override
	public boolean hasChildren(final Object element) {
		if (element instanceof SourceFile) {
			return ! ((SourceFile<?>) element).getChildren().isEmpty();
		}
		return false;
	}

}
