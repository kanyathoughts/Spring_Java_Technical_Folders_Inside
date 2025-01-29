/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.pl1;

import static innowake.ndt.parsing.base.pattern.Pattern.is;

import innowake.ndt.parsing.base.pattern.Pattern;
import innowake.ndt.parsing.base.pattern.RelaxedPatternMatcher;

/**
 * A pattern to detect a preprocessor macro.
 * 
 * <pre>
 * %INCLUDE xyz;
 * </pre>
 */
public class Pl1PreprocessorMacroPattern extends RelaxedPatternMatcher {

	private static final Pattern PREPROCESSOR_PATTERN = Pattern.of(
			/* preprocessor macros: https://www.ibm.com/support/knowledgecenter/SSY2V3_5.2.0/com.ibm.ent.pl1.zos.doc/pg/maksamp.html */
			is("%include", "%dcl", "%use", "%if", "%then", "%else", "%do", "%end")
		
	);

	public Pl1PreprocessorMacroPattern() {
		super(PREPROCESSOR_PATTERN);
	}

}