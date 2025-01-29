/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Marker annotation used in conjunction with {@link Nature @Nature} and {@link Role @Role} to signify that the
 * secured method has to filter the result collection based on the roles of the user.
 * <p>
 * Basically this disables the project-bound static authorization. The appropriate authorization logic has to be done programmatically
 * in the respective endpoint.
 * <p>
 * This is necessary for example when retrieving all clients, where the returned list must only contain
 * the clients, for which the user has access rights to.
 * <p>
 * If this annotation is not present then the {@link MiningVoter} will try to determine the client and project IDs
 * of the to be accessed resource and in cases where this is not possible, error out.
 * 
 * @see Nature
 * @see Role
 */
@Retention(RUNTIME)
@Target(METHOD)
public @interface ManualSecurityWithoutProjectAssociation {
}
