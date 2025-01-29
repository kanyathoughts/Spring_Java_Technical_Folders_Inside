/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;


/**
 * Version handshake helper class, to check if different version formats are considered equal.
 * <p>
 * <pre>
 * This class implements the following "algorithm" for the version check:
 * - sanitize the version string by replacing all "-" with "."
 * - split the strings into component on "."
 * - compare the first three segments of both version strings (the three-component version number)
 *   - if the segments are not equal -> version mismatch
 * - check if there is a segment "SNAPSHOT" in the version
 *   - if both have "SNAPSHOT" -> version match (skip the rest of the checks)
 *   - if only one has "SNAPSHOT" -> version mismatch
 *   - if none has "SNAPSHOT" -> continue version check
 * - compare all segments pairwise
 *   - if not equal -> version mismatch
 * - if all segments are equal but one version string has more segments than the other, then ignore the excess segments 
 *   (this automatically excludes an optional rebuild number from the check)
 * </pre>
 */
public final class Versions {
	
	private final static Logger LOG = LoggerFactory.getLogger(Versions.class);
	
	private Versions() {}

	/**
	 * Checks if the given version strings are considered equal.
	 *
	 * @param versionA the first version string
	 * @param versionB the second version string
	 * @return {@code true} if the given versions are equal, {@code false} otherwise
	 */
	public static boolean equals(final String versionA, final String versionB) {
		LOG.trace(() -> String.format("Comparing '%s' and '%s'.", versionA, versionB));
		
		/* First sanitize and split the string */
		final String[] versionAComponents = split(sanitize(versionA));
		final String[] versionBComponents = split(sanitize(versionB));
		
		/* Check for minimum length */
		if ( ! validMinimumLength(versionAComponents)) {
			LOG.trace(() -> String.format("('%s' does not have the minimum amount of version components.", versionA));
			return false;
		}

		if ( ! validMinimumLength(versionBComponents)) {
			LOG.trace(() -> String.format("'%s' does not have the minimum amount of version components.", versionB));
			return false;
		}
		
		/* Check for equal major version */
		final String majorA = versionAComponents[0];
		final String majorB = versionBComponents[0];
		if ( ! majorA.equals(majorB)) {
			LOG.trace(() -> String.format("Major version mismatch '%s' and '%s'.", majorA, majorB));
			return false;
		}
		
		/* Check for equal minor version */
		final String minorA = versionAComponents[1];
		final String minorB = versionBComponents[1];
		if ( ! minorA.equals(minorB)) {
			LOG.trace(() -> String.format("Minor version mismatch '%s' and '%s'.", minorA, minorB));
			return false;
		}

		/* Check for equal hotfix version */
		final String hotfixA = versionAComponents[2];
		final String hotfixB = versionBComponents[2];
		if ( ! hotfixA.equals(hotfixB)) {
			LOG.trace(() -> String.format("Hotfix version mismatch '%s' and '%s'.", hotfixA, hotfixB));
			return false;
		}
		
		/* Check for snapshot version */
		final boolean versionAIsSnapshot = isSnapshot(versionA);
		final boolean versionBIsSnapshot = isSnapshot(versionB);
		if (versionAIsSnapshot && versionBIsSnapshot) {
			return true;
		} else if (versionAIsSnapshot && ! versionBIsSnapshot) {
			LOG.trace(() -> String.format("'%s' is a snapshot version but '%s' is not.", versionA, versionB));
			return false;
		} else if ( ! versionAIsSnapshot && versionBIsSnapshot) {
			LOG.trace(() -> String.format("'%s' is a snapshot version but '%s' is not.", versionB, versionA));
			return false;
		}
		
		/* Both are *not* snapshot versions now we need to compare the qualifiers */
		final List<String> versionAQualifiers = qualifierComponents(versionAComponents);
		final List<String> versionBQualifiers = qualifierComponents(versionBComponents);
		
		final int versionAQualifierSize = versionAQualifiers.size();
		final int versionBQualifierSize = versionBQualifiers.size();
		if (versionAQualifierSize < versionBQualifierSize) {
			return compareQualifiers(versionAQualifiers, versionBQualifiers);
		} else if (versionAQualifierSize > versionBQualifierSize) {
			return compareQualifiers(versionBQualifiers, versionAQualifiers);
		} else {
			return compareQualifiers(versionAQualifiers, versionBQualifiers);
		}
	}

	private static String sanitize(final String string) {
		return StringUtils.replaceChars(string, '-', '.');
	}
	
	private static String[] split(final String string) {
		return string.split("\\.");
	}
	
	private static boolean validMinimumLength(final String[] components) {
		return components.length >= 3;
	}
	
	private static boolean isSnapshot(final String version) {
		return version.toLowerCase().contains("snapshot");
	}
	
	private static List<String> qualifierComponents(final String[] components) {
		return IntStream.range(3, components.length)
				.mapToObj(index -> components[index])
				.collect(Collectors.toList());
	}
	
	/**
	 * Compares the qualifiers.
	 * <p>
	 * {@code shorterQualifiers} must be shorter or equals in length than {@code longerQualifiers}
	 * 
	 * @param shorterQualifiers the list of qualifiers which has less than or equals the size of the second parameters
	 * @param longerQualifiers the second list of qualifiers for comparison
	 * @return {@code true} if the qualifiers are equal, otherwise {@code false}
	 */
	private static boolean compareQualifiers(final List<String> shorterQualifiers, final List<String> longerQualifiers) {
		final int qualifiersSize = shorterQualifiers.size();
		for (int i = 0; i < qualifiersSize; i++) {
			final String currentShortQualifier = shorterQualifiers.get(i);
			final String currentLongQualifier = longerQualifiers.get(i);
			if ( ! currentShortQualifier.equals(currentLongQualifier)) {
				LOG.trace(() -> String.format("'%s' and '%s' do not match.", currentShortQualifier, currentLongQualifier));
				return false;
			}
		}
		return true;
	}
}
