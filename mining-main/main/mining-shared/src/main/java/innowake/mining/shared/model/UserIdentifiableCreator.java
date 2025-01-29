/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Optional;
import java.util.function.UnaryOperator;

/**
 * Provides the user that created an entity.
 */
public interface UserIdentifiableCreator {
	
	/**
	 * Gets the {@linkplain User user id} which created this entity.
	 *
	 * @return The user id.
	 */
	public String getCreatedByUserId();
	
	/**
	 * Gets the human-readable name of the user who created this entity.
	 *
	 * @return The user name.
	 */
	public Optional<String> getCreatedByUserName();
	
	/**
	 * Sets the human-readable name of the user who created this entity.
	 *
	 * @param userName The user name.
	 */
	public void setCreatedByUserName(String userName);
	
	public default void setCreatedByUserName(final UnaryOperator<String> userNameResolver) {
		setCreatedByUserName(userNameResolver.apply(getCreatedByUserId()));
	}
	
}
