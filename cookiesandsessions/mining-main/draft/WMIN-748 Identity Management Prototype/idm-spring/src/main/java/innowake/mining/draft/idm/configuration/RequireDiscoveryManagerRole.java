/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.draft.idm.configuration;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.security.access.prepost.PreAuthorize;

@Target({ ElementType.METHOD, ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
@PreAuthorize("( hasRole('admin') or hasRole('manager_' + #projectId) ) "
		+ "and @projectNatureValidation.verifyForProject(#projectId, 'discovery')")
public @interface RequireDiscoveryManagerRole {

}
