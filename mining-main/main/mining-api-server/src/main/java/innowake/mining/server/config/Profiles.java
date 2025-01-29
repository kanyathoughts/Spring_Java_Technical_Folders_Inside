/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

/**
 * Supported Spring profiles.
 */
public class Profiles {
	
	private Profiles() {}
	
	/**
	 * Identity & Access Management Profile.
	 * <p>
	 * Delegating the authentication and authorization to Keycloak.i.e. the initial OAuth security implementation.
	 */
	public static final String AUTH = "(!legacy-auth & !no-authorization & !auth-test) | authorized-access";
	
	/**
	 * Identity & Access Management without a backend.
	 */
	public static final String AUTH_TEST = "auth-test";
	
	/**
	 * Identity & Access Management Profile.
	 * <p>
	 * Delegating the authentication and authorization to Keycloak.
	 */
	public static final String IAM = "!legacy-auth & !no-authorization";

	/**
	 * Profile for functionality that should only be active when Identity & Access Management is not active.
	 * <p>
	 * This includes for example the security configuration using OAuth2 and not Keycloak.
	 */
	public static final String NO_IAM = "legacy-auth | no-authorization";

	/**
	 * Profile for functionality that should only be active in the legacy authentication.
	 */
	public static final String LEGACY_AUTH = "legacy-auth";
	
	/**
	 * Profile for functional integration tests without any security activated.
	 * <p>
	 * This <b>must</b> only be used for integration tests.
	 */
	public static final String NO_AUTH = "no-authorization";

	/**
	 * Profile which enables performance profiling.
	 */
	public static final String PROFILING = "profiling";

	/**
	 * Profile which enables platform-integration support (communicating with other services through Kafka).
	 */
	public static final String PLATFORM_INTEGRATION = "platform-integration";
}
