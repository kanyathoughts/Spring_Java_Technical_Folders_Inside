/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;

/**
 * Adapter throwing {@link UnsupportedOperationException} as default for {@link IStructuredSelection}.
 */
public class StructuredSelectionAdapter implements IStructuredSelection {
	
	@Override
	public boolean isEmpty() {
		throw new UnsupportedOperationException();
	}

	@Override
	public Object getFirstElement() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	@SuppressWarnings("rawtypes")
	public Iterator iterator() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		throw new UnsupportedOperationException();
	}

	@Override
	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}

	@Override
	@SuppressWarnings("rawtypes")
	public List toList() {
		throw new UnsupportedOperationException();
	}

}