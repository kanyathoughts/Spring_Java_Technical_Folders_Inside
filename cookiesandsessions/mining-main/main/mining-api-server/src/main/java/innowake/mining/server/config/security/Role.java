/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.security.access.annotation.Secured;

import innowake.mining.shared.security.RoleType;

/**
 * Security annotation for defining the user roles required.
 * <p>
 * See {@link RoleType} for the available values.
 * 
 * @see Nature
 * @see ManualSecurityWithoutProjectAssociation
 */
@Target({ ElementType.METHOD, ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Secured({ "this-value-is-totally-irrelevant-but-cannot-be-empty" })
public @interface Role {

	/**
	 * The actual {@link RoleType types} required.
	 *
	 * @return the types.
	 */
	public RoleType[] value();
	
	/**
	 * The name of the parameter which represents the ID of the client.
	 *
	 * @return "#clientId" by default, if the parameter has a different name, please change accordingly
	 */
	public String clientId() default "#clientId";

	/**
	 * Signifies that the authorization will grant access to client administrators even if the endpoint being secured
	 * does not have any client or project association.
	 *
	 * @return {@code false} by default
	 */
	public boolean onAnyClient() default false;

}
