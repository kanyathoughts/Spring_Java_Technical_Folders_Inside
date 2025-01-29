/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.aspect;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Use as a method annotation to execute the method with system user privileges. The current user authentication (if any) is replaced with the system user
 * during method execution. After the method returns, the previous authentication is restored.
 * <p>
 * <strong>Careful:</strong> The system user has admin privileges and any methods called by the annotated method will also share those privileges.
 * Think carefully before applying this annotation.
 * <p>
 * Certain database operations require an active user or even an admin user, so this annotation can be used in those places.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface WithSystemUser {

}
