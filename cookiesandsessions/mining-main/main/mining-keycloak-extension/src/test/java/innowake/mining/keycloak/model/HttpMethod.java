/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.keycloak.model;

/**
 * A Util class to declare the different HTTP Methods
 * that can be used to aid in testing the extended endpoints.
 */
public class HttpMethod {

	private HttpMethod() {
		/* Empty constructor as this is a Util class. */
	}

	public static final String GET = "GET";
	public static final String POST = "POST";
	public static final String PUT = "PUT";
	public static final String DELETE = "DELETE";
}
