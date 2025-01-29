/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.aspect;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import innowake.mining.server.service.SystemUserSecurityContext;

/**
 * Aspect that executes methods annotated with {@link WithSystemUser} with the system user
 * and restores the previous authentication after returning.
 */
@Aspect
@Component
public class WithSystemUserAspect {
	
	@Around("@annotation(innowake.mining.server.aspect.WithSystemUser)")
	public Object runWithSystemUserPrivileges(final ProceedingJoinPoint pjp) throws Throwable {
		final SecurityContext securityContext = SecurityContextHolder.getContext();
		SecurityContextHolder.setContext(SystemUserSecurityContext.get());
		try {
			return pjp.proceed();
		} finally {
			SecurityContextHolder.setContext(securityContext);
		}
	}
}
