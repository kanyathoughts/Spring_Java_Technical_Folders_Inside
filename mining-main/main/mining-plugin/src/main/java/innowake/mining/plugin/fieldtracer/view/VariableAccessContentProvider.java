/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import java.util.stream.Collectors;

import org.eclipse.jface.viewers.IStructuredContentProvider;

import innowake.ndt.fieldtracing.model.AccessMode;
import innowake.ndt.fieldtracing.model.FieldUsage;

/**
 * Content provider for the read / write variable access.
 */
public class VariableAccessContentProvider implements IStructuredContentProvider {

	private final AccessMode mode;
	
	/**
	 * Create a new content provider and determine if content for read or write should 
	 * be provided.
	 * @param mode The access mode.
	 */
	public VariableAccessContentProvider(final AccessMode mode) {
		this.mode = mode;
	}
	
	@Override
	public Object[] getElements(final Object inputElement) {
		if (inputElement instanceof FieldUsage) {
			return ((FieldUsage<?>) inputElement).getSiblings().stream()
				.filter(node -> node.getAccessType() == mode)
				.collect(Collectors.toList()).toArray();
		}
		return new Object[0];
	}

}
