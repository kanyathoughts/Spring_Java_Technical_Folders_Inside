/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Optional;
import java.util.function.UnaryOperator;

/**
 * Provides the user that last updated an entity.
 */
public interface UserIdentifiableUpdater {
	
	/**
	 * Gets the {@linkplain User user id} which latest updated this entity.
	 *
	 * @return The user id or {@code null} if the entity was not updated by a user.
	 */
	public Optional<String> getUpdatedByUserId();
	
	/**
	 * Gets the human-readable name of the user who last updated this entity.
	 *
	 * @return The user name or {@code null} if the entity was not updated by a user.
	 */
	public Optional<String> getUpdatedByUserName();
	
	/**
	 * Sets the human-readable name of the user who last updated this entity.
	 *
	 * @param userName The user name.
	 */
	public void setUpdatedByUserName(String userName);
	
	public default void setUpdatedByUserName(UnaryOperator<String> userNameResolver) {
		getUpdatedByUserId().ifPresent(userId -> setUpdatedByUserName(userNameResolver.apply(userId)));
	}
	
}
