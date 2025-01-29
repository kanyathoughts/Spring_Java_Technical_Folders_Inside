/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.datadictionary.handler;

import org.eclipse.jface.text.ITextSelection;

import innowake.ndt.core.parsing.IToken;
import innowake.ndt.fieldtracing.TextSelection;


/**
 * A text selection based on an {@link IToken}.
 */
public class TokenTextSelection implements ITextSelection, TextSelection{
	
	private final IToken token;
	
	/**
	 * Creates a text selection based on the given token.
	 * 
	 * @param token the token to get the selection information from
	 */
	public TokenTextSelection(final IToken token) {
		this.token = token;
	}

	@Override
	public boolean isEmpty() {
		return token.getLength() == 0;
	}
	
	@Override
	public String getText() {
		return token.getText().toString();
	}
	
	@Override
	public int getStartLine() {
		return token.getLine();
	}
	
	@Override
	public int getOffset() {
		return token.getOffset();
	}
	
	@Override
	public int getLength() {
		return token.getLength();
	}
	
	/**
	 * Assuming a token is not spanning multiple lines, therefore the start and end line are the same.
	 * <p>
	 * {@inheritDoc}
	 */
	@Override
	public int getEndLine() {
		return token.getLine();
	}

}
