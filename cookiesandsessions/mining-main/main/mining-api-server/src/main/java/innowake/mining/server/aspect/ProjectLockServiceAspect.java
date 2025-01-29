/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.aspect;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.locking.ProjectLockService;
import innowake.mining.server.locking.ProjectLockService.AlreadyLockedException;
import innowake.mining.server.locking.ProjectLockService.ProjectLock;
import innowake.mining.server.service.TryLock;
import innowake.mining.shared.access.EntityId;
import org.apache.commons.lang.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.servlet.HandlerMapping;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * Use a method annotation to acquire a {@link ProjectLockService} for exclusive data access. 
 */
@Aspect
@Component
public class ProjectLockServiceAspect {
	
	private static final Logger LOG = LoggerFactory.getLogger(ProjectLockServiceAspect.class);
	
	@Autowired
	private ProjectLockService projectLockService;
	
	@Pointcut("execution(* innowake.mining.server.locking.ProjectLockService+.tryLock(..))")
	public void tryLock() {
		/* pointcut declaration */
	}
	
	@Pointcut("execution(* innowake.mining.server.locking.ProjectLockService+.waitForLockAndRun(..))")
	public void waitForLockAndRun() {
		/* pointcut declaration */
	}
	
	@Around("waitForLockAndRun()") 
	public Object projectWaitForLockAndRun (final ProceedingJoinPoint pjp) throws Throwable {
		try {
			return pjp.proceed();
		} finally {
			LOG.debug(() -> String.format("%s lock applied for job execution", pjp.getSignature().getName()));
		}
	}

	/**
	 * Executes the method while applying a TryLock. This method intercepts the execution of the annotated method and applies a try lock mechanism before
	 * proceeding with the original method execution.
	 *
	 * @param pjp The ProceedingJoinPoint object representing the intercepted method execution.
	 * @return The result of the original method execution.
	 * @throws Throwable if an error occurs during the method execution.
	 */
	@Around("@annotation(innowake.mining.server.service.TryLock)")
	public Object methodTryLock(final ProceedingJoinPoint pjp) throws Throwable {
		ProjectLock projectLock = null;
		final TryLock tryLockObj;
		final String projectId = getProjectId();
		tryLockObj = ((MethodSignature) pjp.getSignature()).getMethod().getAnnotation(TryLock.class);
		try {
			projectLock = projectLockService.tryLock(EntityId.of(projectId), tryLockObj.lockCategory(), tryLockObj.reasonPhrase());
			LOG.debug(() -> String.format("Applied lock on %s method of %s on project %s", pjp.getSignature().getName(), tryLockObj.lockCategory(), projectId));
			return pjp.proceed();
		} catch (final AlreadyLockedException ex) {
			LOG.error(() -> "Lock already applied: " + ex.getMessage());
			throw ex;
		} finally {
			/* Unlocks the project lock after a method execution. This code will be executed after method annotated with @TryLock have 
			 * successfully returned. It releases the lock if it exists. */
			if (projectLock != null) {
				projectLock.unlock();
				LOG.debug(() -> String.format("Released lock on %s method of %s on project %s", pjp.getSignature().getName(), tryLockObj.lockCategory(),
						projectId));
			}
		}
	}

	@SuppressWarnings("unchecked")
	private String getProjectId() {
		final RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
		if (requestAttributes != null) {
			final HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
			final Map<String, String> pathVariables = (Map<String, String>) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
			return pathVariables.get("projectId");
		}
		return StringUtils.EMPTY;
	}
}
