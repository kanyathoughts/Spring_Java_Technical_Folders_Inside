/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import org.eclipse.jface.text.TextSelection;

/**
 * Wraps a {@link TextSelection} in a structured selection.
 */
public class TextSelectionWrapper extends StructuredSelectionAdapter {

	private final TextSelection selection;

	/**
	 * Creates an instance.
	 * 
	 * @param selection the text selection to wrap.
	 */
	public TextSelectionWrapper(final TextSelection selection) {
		this.selection = selection;
	}
	
	@Override
	public boolean isEmpty() {
		return selection.isEmpty();
	}

	@Override
	public Object getFirstElement() {
		return selection;
	}
}
