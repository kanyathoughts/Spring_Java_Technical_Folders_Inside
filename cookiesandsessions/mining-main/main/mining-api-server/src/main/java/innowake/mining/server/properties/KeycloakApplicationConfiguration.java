/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;

/**
 * Bean for mapping the relevant keycloak configuration values from the Spring application configuration (application.yml).
 */
@ConfigurationProperties(prefix = "keycloak")
public class KeycloakApplicationConfiguration {

	private String authServerUrl;
	private String realm;
	private String sslRequired;
	private String clientId;

	/**
	 * Creates a new bean instance.
	 * 
	 * @param authServerUrl the value of the {@code keycloak.auth-server-url} property
	 * @param realm the value of the {@code keycloak.realm} property 
	 * @param sslRequired the value of the {@code keycloak.ssl-required} property 
	 * @param clientId the value of the {@code keycloak.client-id} property
	 */
	@ConstructorBinding
	public KeycloakApplicationConfiguration(final String authServerUrl, final String realm, final String sslRequired, final String clientId) {
		this.authServerUrl = authServerUrl;
		this.realm = realm;
		this.sslRequired = sslRequired;
		this.clientId = clientId;
	}

	/**
	 * @return the value of the {@code keycloak.auth-server-url} property
	 */
	public String getAuthServerUrl() {
		return authServerUrl;
	}

	/**
	 * @return the value of the {@code keycloak.realm} property
	 */
	public String getRealm() {
		return realm;
	}

	/**
	 * @return the value of the {@code keycloak.ssl-required} property 
	 */
	public String getSslRequired() {
		return sslRequired;
	}

	/**
	 * @return the value of the {@code keycloak.clientId} property
	 */
	public String getClientId() {
		return clientId;
	}

	/**
	 * @return the value of the {@code keycloak.client-id} property
	 */
	public String getResource() {
		return clientId;
	}
}
