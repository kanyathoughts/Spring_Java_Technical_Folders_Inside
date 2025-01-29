/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.pl1;

import static innowake.ndt.parsing.base.pattern.Pattern.any;
import static innowake.ndt.parsing.base.pattern.Pattern.is;

import innowake.ndt.parsing.base.pattern.Pattern;
import innowake.ndt.parsing.base.pattern.RelaxedPatternMatcher;

/**
 * A pattern to detect a variable declaration with specific data type detection.
 * 
 * <pre>
 * DCL MY-VAR CHAR (78);  
 * </pre>
 */
public class Pl1DeclarePattern extends RelaxedPatternMatcher {
	
	private static final Pattern dclPattern = Pattern.of(
			is("dcl", "declare"),
			any(),
			is(
					/* PL1 datatypes: https://www.ibm.com/support/knowledgecenter/SSQP76_8.6.0/com.ibm.odm.dserver.rules.designer.author/pl1_topics/con_bom_pl1_models_datatype.html */
					"float", "fixed", "picture", "bit", "char", "wchar", "g", "file", "area", "pointer", "offset", "handle",
					/* found in customer sources */
					"entry", "builtin", "external", "static", "pic", "ptr"
			));

	public Pl1DeclarePattern() {
		super(dclPattern);
	}
	
}