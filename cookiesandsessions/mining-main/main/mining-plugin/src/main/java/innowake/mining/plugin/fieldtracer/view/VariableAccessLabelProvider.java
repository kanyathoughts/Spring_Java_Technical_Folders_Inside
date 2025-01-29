/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ViewerCell;
import innowake.ndt.fieldtracing.model.FieldUsage;

/**
 * Variable tree read / write label provider
 */
public class VariableAccessLabelProvider extends CellLabelProvider {

	@Override
	public void update(final ViewerCell cell) {
		final FieldUsage<?> usage = (FieldUsage<?>) cell.getElement();
		if (usage != null) {
			cell.setText(usage.getFieldName());
		}
	}

}
