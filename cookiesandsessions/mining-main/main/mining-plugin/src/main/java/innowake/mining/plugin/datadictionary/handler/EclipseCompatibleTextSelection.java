/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.handler;

import org.eclipse.jface.text.ITextSelection;

import innowake.ndt.fieldtracing.TextSelection;

/**
 * Wrapper for {@link TextSelection} to use it as wrapped {@link ITextSelection}
 */
public class EclipseCompatibleTextSelection implements ITextSelection {

	private final TextSelection textSelection;
	
	/**
	 * Constructor
	 * 
	 * @param textSelection the wrapped {@link TextSelection}
	 */
	public EclipseCompatibleTextSelection(final TextSelection textSelection) {
		this.textSelection = textSelection;
	}

	@Override
	public boolean isEmpty() {
		return textSelection.getLength() == 0;
	}

	@Override
	public int getOffset() {
		return textSelection.getOffset();
	}

	@Override
	public int getLength() {
		return textSelection.getLength();
	}

	@Override
	public int getStartLine() {
		return textSelection.getStartLine();
	}

	@Override
	public int getEndLine() {
		return textSelection.getEndLine();
	}

	@Override
	public String getText() {
		return textSelection.getText();
	}

}
