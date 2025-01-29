/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ViewerCell;
import innowake.ndt.fieldtracing.model.FieldUsage;

/**
 * Label provider for the statement table.
 */
public class StatementLabelProvider extends CellLabelProvider {

	@Override
	public void update(final ViewerCell cell) {
		final FieldUsage<?> usage = (FieldUsage<?>) cell.getElement();
		cell.setText(getText(usage, cell.getColumnIndex()));
	}
	
	private String getText(final FieldUsage<?> usage, int colIndex) {
		switch (colIndex) {
			case 0:
				return usage.getLine().map(Object::toString).orElseGet(() -> "");
			case 1:
				return usage.getAccessType().name();
			case 2:
				return usage.getFieldName();
			case 3:
				return usage.getStatementName();
			default:
				return "";
		}
	}

}
