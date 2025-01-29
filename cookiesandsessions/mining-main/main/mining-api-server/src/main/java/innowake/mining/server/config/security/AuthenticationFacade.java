/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import org.springframework.beans.factory.annotation.Autowire;
import org.springframework.security.core.Authentication;

/**
 * Facade for easy access to the Authentication by using {@link Autowire Autowiring}
 */
public interface AuthenticationFacade {

    /**
     * @return the {@link Authentication}
     */
    Authentication getAuthentication();

	/**
	 * @return the user ID of the currently logged-in user
	 */
	String getUserId();
}