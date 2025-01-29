/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.ims;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Count the occurrences of patterns associated with ResolveTarget(s).
 */
public class ImsPatternCounter {
	/**
	 * Targets for which all regex-patterns must match for a successful recognition.
	 */
	private static final ResolveTarget[] MATCH_ALL_TARGETS = { 
													 ResolveTarget.IMS_DBD, 
													 ResolveTarget.IMS_HELPTXT, 
													 ResolveTarget.IMS_PSB,
													 ResolveTarget.IMS_HDAMPARM};
	
	/**
	 * The method tries to match regex patterns.
	 * For each relevant {@link ResolveTarget} a list of regex(es) is defined
	 * in {@link ImsPatternType}.
	 *
	 * @param content the string content of a file
	 * @return a {@link ResolveTarget} or {@code NONE} if no matching could be found with the predefined regex(es).
	 */
	public ResolveTarget getFileType(final String content) {
		/* First try to match ALL regex-patterns for particular ResolveTargets. */
		for (final ResolveTarget currentTarget : MATCH_ALL_TARGETS) {
			boolean allMatched = true;
			for (final Pattern regexPattern : ImsPatternType.getPatterns(currentTarget)) {
				if (countMatches(regexPattern, content) == 0) {
					allMatched = false;
					/* At least one regex didn't match, continue with the next potential ResolveTarget. */
					break;
				}
			}
			if (allMatched) {
				return currentTarget;
			}
		}
		return ResolveTarget.NONE;
	}

	private int countMatches(final Pattern pattern, final String fileContent) {
		final Matcher matcher = pattern.matcher(fileContent);
		int matchCount = 0;
		while (matcher.find()) {
			matchCount++;
		}
		return matchCount;
	}
}
