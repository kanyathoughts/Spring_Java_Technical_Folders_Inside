/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server;

/**
 * Container class for logging constants.
 */
public class Logging {

	/**
	 * {@value #CONTROLLER} logs messages for all controllers. 
	 */
	public static final String CONTROLLER = "innowake.mining.server.controller";
	/**
	 * {@value #GRAPHQL} logs messages for GraphQl controllers and query execution. 
	 */
	public static final String GRAPHQL = "innowake.mining.server.graphql";
	/**
	 * {@value #SERVICE} logs messages for all services. 
	 */
	public static final String SERVICE = "innowake.mining.server.service";

	public static final String LICENSE = "innowake.mining.license";

	private Logging() {
	}
}
