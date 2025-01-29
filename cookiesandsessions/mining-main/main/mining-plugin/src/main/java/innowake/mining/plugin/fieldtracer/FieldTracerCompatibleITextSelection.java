/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer;

import org.eclipse.jface.text.ITextSelection;

import innowake.ndt.fieldtracing.TextSelection;

/**
 * Wrapper for {@link ITextSelection} to use it as wrapped {@link TextSelection}
 */
public class FieldTracerCompatibleITextSelection implements TextSelection {

	private final ITextSelection textSelection;
	
	/**
	 * Constructor.
	 * @param textSelection wrapped {@link ITextSelection}
	 */
	public FieldTracerCompatibleITextSelection(final ITextSelection textSelection) {
		this.textSelection = textSelection;
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
