/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak;

import org.keycloak.Config.Scope;
import org.keycloak.models.KeycloakSession;
import org.keycloak.models.KeycloakSessionFactory;
import org.keycloak.services.resource.RealmResourceProvider;
import org.keycloak.services.resource.RealmResourceProviderFactory;

/**
 * An implementation of {@link RealmAdminResourceProviderFactory} that is used
 * to initialize an instance of a {@link RealmResourceProvider}.
 */
public class RealmAdminResourceProviderFactory implements RealmResourceProviderFactory {

	private static final String ID = "admin";

	@Override
	public String getId() {
		return ID;
	}

	@Override
	public RealmAdminResourceProvider create(final KeycloakSession session) {
		return new RealmAdminResourceProvider(session);
	}

	@Override
	public void init(final Scope config) {
		/* No initialization needed. */
	}

	@Override
	public void postInit(final KeycloakSessionFactory factory) {
		/* No implementation needed. */
	}

	@Override
	public void close() {
		/* No implementation needed. */
	}

}