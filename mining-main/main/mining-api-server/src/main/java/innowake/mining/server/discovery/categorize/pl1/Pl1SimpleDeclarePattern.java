/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.pl1;

import static innowake.ndt.parsing.base.pattern.Pattern.any;
import static innowake.ndt.parsing.base.pattern.Pattern.is;

import innowake.ndt.parsing.base.pattern.Pattern;
import innowake.ndt.parsing.base.pattern.RelaxedPatternMatcher;

/**
 * A pattern to detect a variable declaration without specific data type detection.
 * 
 * <pre>
 * DCL MY-VAR ...  
 * </pre>
 */
public class Pl1SimpleDeclarePattern extends RelaxedPatternMatcher {

	/**
	 * simpleDeclarePattern.
	 */
	private static final Pattern SIMPLE_DECLARE_PATTERN = Pattern.of(
			is("dcl", "declare"),
			any(),
			is(";")
	);

	public Pl1SimpleDeclarePattern() {
		super(SIMPLE_DECLARE_PATTERN);
	}
	
}