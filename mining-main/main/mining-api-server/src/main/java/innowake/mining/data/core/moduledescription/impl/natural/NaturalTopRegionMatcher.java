/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import innowake.mining.data.core.moduledescription.api.LexerMatcher;
import innowake.mining.data.core.moduledescription.impl.AbstractRegionMatcher;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.natural.NaturalRegionType;

/**
 * Matcher to extract the Natural top region between the program content start and the first default code statement.
 */
public class NaturalTopRegionMatcher extends AbstractRegionMatcher {
	
	/** The {@link NaturalTopRegionMatcher} instance. */
	public static final LexerMatcher INSTANCE = new NaturalTopRegionMatcher();
	
	@Override
	protected boolean isRegionEnd(final ILexScanner scanner) {
		return scanner.getRegionType() == NaturalRegionType.TYPE_CODE_STANDARD;
	}

}
