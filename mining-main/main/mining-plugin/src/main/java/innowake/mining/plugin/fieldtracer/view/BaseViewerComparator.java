/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;

/**
 * Base class for viewer comparator subclasses.
 */
class BaseViewerComparator extends ViewerComparator {

	/**
	 * Column index affected by the sort.
	 */
	protected int propertyIndex;
	
	/**
	 * Constant for the DESCENDING sort order
	 */
	protected static final int DESCENDING = 1;
	
	/**
	 * The current sort order
	 */
	protected int direction = DESCENDING;

	protected BaseViewerComparator() {
		this.propertyIndex = 0;
		direction = DESCENDING;
	}
	

	int getDirection() {
		return direction == 1 ? SWT.DOWN : SWT.UP;
	}

	void setColumn(final int column) {
		if (column == this.propertyIndex) {
			direction = 1 - direction;
		} else {
			this.propertyIndex = column;
			direction = DESCENDING;
		}
	}
	
}
