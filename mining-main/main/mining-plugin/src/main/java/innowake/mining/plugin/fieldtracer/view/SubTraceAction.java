/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import java.util.Optional;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.TableViewer;
import innowake.mining.plugin.fieldtracer.FieldTracer;
import innowake.ndt.fieldtracing.model.FieldUsage;
import innowake.ndt.fieldtracing.SubTraceable;

/**
 * Action to create initiate a new sub trace.
 * A sub trace is a new field trace based on an existing field trace with a
 * {@link FieldUsage} as input.
 * <br>The sub trace is executed as same as the user would selected the field
 * in the source code. It is opened in a new tracing window.
 * 
 */
final class SubTraceAction implements IDoubleClickListener {
	
	@Override
	public void doubleClick(final DoubleClickEvent event) {
		if ( ! (event.getSource() instanceof TableViewer)) {
			return;
		}
		
		final Optional<FieldUsage<?>> nodeOpt = getNodeFromSelection((TableViewer) event.getSource());
		if (nodeOpt.isPresent()) {
			createSubTrace(nodeOpt.get());
		}
	}
	
	private void createSubTrace(final FieldUsage<?> node) {
		final SubTraceable script = new FieldTracer();
		script.subtrace(node.getSelectionInUnassembledSourceFile());
	}
	
	private Optional<FieldUsage<?>> getNodeFromSelection(final TableViewer source) {
		return Optional.ofNullable((FieldUsage<?>) source.getStructuredSelection().getFirstElement());
	}
}
