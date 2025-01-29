/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.authentication;

/**
 * Defines the constants used by Keycloak authentication tests.
 */
public class AuthenticationTestConstants {

	/**
	 * The URL of the API server used during testing.
	 */
	public static final String API_SERVER_URL = "http://127.0.0.1:8080";

	/**
	 * The file name of the keycloak configuration file on the API server.
	 */
	public static final String KEYCLOAK_ECLISPE_FILE_NAME = "keycloak-eclipse.json";

	/**
	 * The file name of the plain text file of the token storage.
	 */
	public static final String PLAIN_TOKEN_STORAGE_FILE_NAME = "miningTokenStorage.txt";

	/**
	 * The URL of the keycloak configuration file on the API server.
	 */
	public static final String KEYCLOAK_CONFIG_URL = API_SERVER_URL + "/" + KEYCLOAK_ECLISPE_FILE_NAME;

}
