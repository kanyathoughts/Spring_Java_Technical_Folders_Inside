/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.ims;

import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_DBD;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_HDAMPARM;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_HELPTXT;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_PSB;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Enum for pattern types for various IMS-ResolveTargets.
 */
public enum ImsPatternType {
	
	/* IMS_DBD */
	DBD(IMS_DBD, Pattern.compile("DBD\\s+NAME=\\w+,\\s")),
	FIELD(IMS_DBD, Pattern.compile("FIELD\\s+NAME=")),
	DBDGEN(IMS_DBD, Pattern.compile("DBDGEN\\s")),
	FINISH(IMS_DBD, Pattern.compile("FINISH\\s")),

	/* IMS_HDAMPARM */
	H_DBD(IMS_HDAMPARM, Pattern.compile("DBD\\s+NAME=\\w+,\\s")),
	H_ACCESS(IMS_HDAMPARM, Pattern.compile("(ACCESS|RMNAME|PSNAME)=")),

	/* IMS_HELPTXT */
	ONLINE(IMS_HELPTXT, Pattern.compile("(ONLINE)?\\s+SCREEN\\s+HELP")),
	HLP_END(IMS_HELPTXT, Pattern.compile("END\\s+PAGE")),

	/* IMS_PSB */
	PCB(IMS_PSB, Pattern.compile("PCB\\s+TYPE=\\w+")),
	PSBGEN(IMS_PSB, Pattern.compile("PSBGEN\\s+LANG=\\w+")),
	PSBNAME(IMS_PSB, Pattern.compile("PSBNAME=\\w+"));

	private final ResolveTarget type;
	private final Pattern pattern;

	/**
	 * Create a new enum from type and pattern.
	 * 
	 * @param type the {@link ResolveTarget}-type
	 * @param pattern the regex pattern
	 */
	private ImsPatternType(final ResolveTarget type, final Pattern pattern) {
		this.type = type;
		this.pattern = pattern;
	}

	/**
	 * Return all patterns corresponding to a {@link ResolveTarget}-type.
	 *
	 * @param type the {@link ResolveTarget}-type
	 * @return a list of regex-patterns
	 */
	public static List<Pattern> getPatterns(final ResolveTarget type) {
		return Arrays.asList(ImsPatternType.values())
				     .stream()
				     .filter(pattern -> pattern.getType().equals(type))
				     .map(ImsPatternType::getPattern)
				     .collect(Collectors.toList());
	}

	/**
	 * @return The compiled regex pattern
	 */
	public Pattern getPattern() {
		return pattern;
	}

	/**
	 * @return the type
	 */
	public ResolveTarget getType() {
		return type;
	}
}
