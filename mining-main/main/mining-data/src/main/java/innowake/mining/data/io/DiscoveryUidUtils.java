/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

/**
 * Utilities for dealing with Discovery uids that have special meaning.
 * In particular these are
 * <ul>
 * <li> {@code 0}: the artifact is a utility
 * <li> {@code -1}: the artifact is missing (referenced by other artifacts, but not found in the codebase)
 * </ul>
 */
public class DiscoveryUidUtils {
	/**
	 * The UID of utilities.
	 */
	public static final long UTILITY_UID = 0;
	/**
	 * The UID of missing artifacts.
	 */
	public static final long MISSING_UID = -1;
	/**
	 * The UID of missing artifacts as Long.
	 */
	public static final Long MISSING_UID_LONG = Long.valueOf(DiscoveryUidUtils.MISSING_UID);

	/**
	 * Returns whether the given UID represents a utility or a missing artifact.
	 * @param uid the uid of the artifact
	 * 
	 * @return {@code true} if {@code uid} represents either a utility or missing artifact
	 */
	public static boolean isMissingOrUtility(final long uid) {
		return isMissing(uid) || isUtility(uid);
	}

	/**
	 * Returns whether the given UID represents a missing artifact.
	 * @param uid the uid of the artifact
	 * 
	 * @return {@code true} if {@code uid} represents a missing artifact
	 */
	public static boolean isMissing(final long uid) {
		return uid == MISSING_UID;
	}

	/**
	 * Returns whether the given UID represents a utility
	 * @param uid the uid of the artifact
	 * 
	 * @return {@code true} if {@code uid} represents a utility
	 */
	public static boolean isUtility(final long uid) {
		return uid == UTILITY_UID;
	}
}
