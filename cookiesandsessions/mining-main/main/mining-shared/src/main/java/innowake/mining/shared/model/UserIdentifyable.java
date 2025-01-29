/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Provide the user created and last updated an entity.
 */
public interface UserIdentifyable {

	/**
	 * Gets the {@linkplain User user id} which created this entity.
	 *
	 * @return The user id.
	 */
	public String getCreatedByUserId();
	
	/**
	 * Sets the {@linkplain User user id} which created this entity.
	 *
	 * @param userId The user id.
	 */
	public void setCreatedByUserId(String userId);

	/**
	 * Gets the human-readable name of the user who created this entity.
	 *
	 * @return The user name.
	 */
	public String getCreatedByUserName();

	/**
	 * Sets the human-readable name of the user who created this entity.
	 *
	 * @param userName The user name.
	 */
	public void setCreatedByUserName(String userName);

	/**
	 * Gets the {@linkplain User user id} which latest updated this entity.
	 *
	 * @return The user id or {@code null} if the entity was not updated by a user.
	 */
	public String getUpdatedByUserId();

	/**
	 * Sets the {@linkplain User user id} which latest updated this entity.
	 *
	 * @param userId The user id.
	 */
	public void setUpdatedByUserId(String userId);

	/**
	 * Gets the human-readable name of the user who last updated this entity.
	 *
	 * @return The user name or an empty String if the entity was not updated by a user.
	 */
	public String getUpdatedByUserName();

	/**
	 * Sets the human-readable name of the user who last updated this entity.
	 *
	 * @param userName The user name.
	 */
	public void setUpdatedByUserName(String userName);

}
