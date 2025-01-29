/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.security.access.annotation.Secured;

import innowake.mining.server.controller.ClientController;
import innowake.mining.shared.security.NatureType;

/**
 * Security annotation for defining the nature required.
 * <p>
 * See {@link NatureType} for the available values.
 * 
 * @see Role
 * @see ManualSecurityWithoutProjectAssociation
 */
@Target({ ElementType.METHOD, ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Secured({ "this-value-is-totally-irrelevant-but-cannot-be-empty" })
public @interface Nature {

	/**
	 * The actual {@link NatureType types} required.
	 *
	 * @return the types.
	 */
	public NatureType[] value();
	
	/**
	 * The name of the parameter which represents the ID of the project.
	 *
	 * @return "#projectId" by default, if the parameter has a different name, please change accordingly
	 */
	public String projectId() default "#projectId";
	
	/**
	 * Signify that the authorization will not check for a particular project role but checks that the
	 * given nature is present on any project within a client.
	 *
	 * @return {@code false} by default, changing this to {@code true} checks that this nature is present on any project instead of 
	 *            one particular. This is for example used by 
	 *            {@link ClientController#findById(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse, Long)}
	 *            where there is no project ID applicable. 
	 */
	public boolean onAnyProject() default false;
}
