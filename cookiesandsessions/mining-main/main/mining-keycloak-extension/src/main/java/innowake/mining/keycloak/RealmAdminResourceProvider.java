/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import org.keycloak.models.KeycloakSession;
import org.keycloak.services.resource.RealmResourceProvider;

/**
 * An implementation of {@link RealmResourceProvider} that is used inside the Keycloak environment
 * to extend the available endpoints for managing Users and their access to the application.
 */
public class RealmAdminResourceProvider implements RealmResourceProvider {

	private final KeycloakSession session;

	/**
	 * Constructor to create an instance of {@link RealmAdminResourceProvider}.
	 * This is used to extend the available Keycloak endpoints for authorization management.
	 * 
	 * @param session The {@link KeycloakSession Keycloak Session}
	 */
	public RealmAdminResourceProvider(final KeycloakSession session) {
		this.session = session;
	}

	@Override
	public Object getResource() {
		return new AdminRestResource(session);
	}

	@Override
	public void close() {
		/* No implementation needed. */
	}

}