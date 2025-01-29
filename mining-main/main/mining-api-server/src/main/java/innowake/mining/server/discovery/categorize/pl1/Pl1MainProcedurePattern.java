/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.pl1;

import static innowake.ndt.parsing.base.pattern.Pattern.endsWith;
import static innowake.ndt.parsing.base.pattern.Pattern.is;

import innowake.ndt.parsing.base.pattern.Pattern;
import innowake.ndt.parsing.base.pattern.RelaxedPatternMatcher;

/**
 * A pattern to detect a main procedure.
 * 
 * <pre>
 * PRG: proc options(main);
 *   ...
 * end PRG;
 * </pre>
 */
public class Pl1MainProcedurePattern extends RelaxedPatternMatcher {

	private static final Pattern MAIN_PROC_PATTERN = Pattern.of(
			endsWith(":"), /* label */
			is("proc", "procedure"),
			is("options"),
			is("main"),
			is(";")
	);

	public Pl1MainProcedurePattern() {
		super(MAIN_PROC_PATTERN);
	}

}