/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.jcl;

import java.util.regex.Pattern;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.api.LexerMatcher;
import innowake.mining.data.core.moduledescription.impl.AbstractRegionMatcher;
import innowake.ndt.core.parsing.ILexScanner;
import innowake.ndt.core.parsing.jcl.JclRegionType;

/**
 * Matcher to extract the JCL top region between the job start and the first actual EXEC step.
 */
public class JclTopRegionMatcher extends AbstractRegionMatcher {
	
	/** The {@link JclTopRegionMatcher} instance. */
	public static final LexerMatcher INSTANCE = new JclTopRegionMatcher();
	
	private static final Pattern STEP_LABEL = Pattern.compile("//\\w+");
	private static final String EXEC_TOKEN = "EXEC";
	
	@Nullable
	private CharSequence lastToken;
	
	@Override
	public String match(final ILexScanner scanner, final String content) {
		lastToken = null;
		return super.match(scanner, content);
	}
	
	@Override
	protected boolean isRegionEnd(final ILexScanner scanner) {
		if (lastToken != null && scanner.getRegionType() == JclRegionType.TYPE_STANDARD
				&& STEP_LABEL.matcher(lastToken).matches() && EXEC_TOKEN.equals(scanner.getText())) {
			return true;
		}
		lastToken = scanner.getText();
		return false;
	}

}
