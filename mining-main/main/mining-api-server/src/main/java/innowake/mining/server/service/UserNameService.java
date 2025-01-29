/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;

import java.util.List;

/**
 * Service that can look up the human-readable names of users given user ids.
 */
public interface UserNameService {

	String SYSTEM_USER_NAME = "SYSTEM";
	String SYSTEM_USER_ID = "system_user";

	/**
	 * Contains the result of a user name lookup operation.
	 */
	public static class LookupResult {

		private final String userName;
		private final Status status;
		
		/**
		 * Status to denote the user status.
		 */
		public enum Status {
			OK,
			MISSING,
			DELETED
		}

		/**
		 * Instantiates the user name look up result.
		 * 
		 * @param username the user name
		 * @param status the status of user
		 */
		public LookupResult(final String username, final Status status) {
			this.userName = username;
			this.status = status;
		}

		/**
		 * Returns the resolved user name or empty string when the name could not be resolved.
		 * <p>
		 * Note that when the name could not be resolved, then also {@link #isMissing()} will return {@code true}.
		 *
		 * @return the resolved user name
		 */
		public String getUserName() {
			return userName;
		}

		/**
		 * Returns {@code true} if the user is missing from the database and could not be resolved.
		 *
		 * @return {@code true} if the user is missing; else {@code false}
		 */
		public boolean isMissing() {
			return status == Status.MISSING;
		}

		/**
		 * Returns {@code true} if the user existed previously or is marked as deleted.
		 *
		 * @return {@code true} if the user account is marked as deleted; {@code false} otherwise
		 */
		public boolean isDeleted() {
			return status == Status.DELETED;
		}
	}

	/**
	 * Look up a single user name given the user id.
	 *
	 * @param userId the user id
	 * @return the result of the lookup
	 */
	public LookupResult lookupUserName(String userId);

	/**
	 * Look up user names for each entry in a collection of user ids and return a list of the resolved user names.
	 * <p>
	 * The returned list has entries in order corresponding to the provided {@code userIds}.
	 *
	 * @param userIds a collection of user ids to look up
	 * @return the list of looked up user names
	 */
	public List<LookupResult> lookupUserNames(Iterable<String> userIds);

}
