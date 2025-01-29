/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.Viewer;
import innowake.ndt.fieldtracing.model.FieldUsage;

/**
 * Used to sort the statement table.
 */
final class StatementViewerComparator extends BaseViewerComparator {

	@Override
	public int compare(final Viewer viewer, final Object elem1, final Object elem2) {
		final FieldUsage<?> usage1 = (FieldUsage<?>) elem1;
		final FieldUsage<?> usage2 = (FieldUsage<?>) elem2;
		return direction == DESCENDING ? -compare(usage1, usage2) : compare(usage1, usage2);
	}
	
	private int compare(final FieldUsage<?> usage1, final FieldUsage<?> usage2) {
		switch (propertyIndex) {
			case 0:
				return (usage1.getLine().orElseGet(() -> Integer.valueOf(-1))).intValue() - (usage2.getLine().orElseGet(() -> Integer.valueOf(-1))).intValue();
			case 1:
				return usage1.getAccessType().name().compareTo(usage2.getAccessType().name());
			case 2:
				return usage1.getFieldName().compareTo(usage2.getFieldName());
			case 3:
				return usage1.getStatementName().compareTo(usage2.getStatementName());
			default:
				return 0;
		}
	}

}
