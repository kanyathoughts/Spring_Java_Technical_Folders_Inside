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
 * PRG: proc;
 *   ...
 * end PRG;
 * </pre>
 */
public class Pl1ProcedurePattern extends RelaxedPatternMatcher {

	private static final Pattern PROC_PATTERN = Pattern.of(
			endsWith(":"),
			is("proc", "procedure"),
			is("end")
	);

	public Pl1ProcedurePattern() {
		super(PROC_PATTERN);
	}

}