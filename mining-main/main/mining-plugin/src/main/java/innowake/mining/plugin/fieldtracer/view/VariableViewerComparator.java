/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.Viewer;
import innowake.ndt.fieldtracing.model.FieldDefinition;

/**
 * Used to sort the variable table.
 */
final class VariableViewerComparator extends BaseViewerComparator {

	@Override
	public int compare(final Viewer viewer, final Object elem1, final Object elem2) {
		final FieldDefinition<?> usage1 = (FieldDefinition<?>) elem1;
		final FieldDefinition<?> usage2 = (FieldDefinition<?>) elem2;
		return direction == DESCENDING ? -compare(usage1, usage2) : compare(usage1, usage2);
	}
	
	private int compare(final FieldDefinition<?> usage1, final FieldDefinition<?> usage2) {
		switch (propertyIndex) {
			case 0:
				return (usage1.getLevel().orElseGet(() -> Integer.valueOf(-1))).intValue() - (usage2.getLevel().orElseGet(() -> Integer.valueOf(-1))).intValue();
			case 1:
				return usage1.getType().name().compareTo(usage2.getType().name());
			case 2:
				return usage1.getFieldName().compareTo(usage2.getFieldName());
			default:
				return 0;
		}
	}

}
