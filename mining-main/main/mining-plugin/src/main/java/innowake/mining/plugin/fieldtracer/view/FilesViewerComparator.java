/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.Viewer;

import innowake.ndt.fieldtracing.model.SourceFile;

/**
 * Files tree viewer comparator support
 */
final class FilesViewerComparator extends BaseViewerComparator {

	@Override
	public int compare(final Viewer viewer, final Object elem1, final Object elem2) {
		final SourceFile<?> file1 = (SourceFile<?>) elem1;
		final SourceFile<?> file2 = (SourceFile<?>) elem2;
		return direction == DESCENDING ? -compare(file1, file2) : compare(file1, file2);
	}
	
	private int compare(final SourceFile<?> file1, final SourceFile<?> file2) {
		if (propertyIndex == 0) {
			return file1.getName().compareTo(file2.getName());
		}
		return 0;
	}
}
