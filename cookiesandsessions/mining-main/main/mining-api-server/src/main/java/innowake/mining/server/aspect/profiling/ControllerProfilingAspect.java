/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.aspect.profiling;

import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.api.profiling.ProfilingSession;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.mining.server.config.Profiles;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.util.ClassUtils;

/**
 * Aspect which enables profiling of classes annotated with {@link org.springframework.stereotype.Controller}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class ControllerProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(ControllerProfilingAspect.class);

	private static final String PROFILING_CATEGORY_PREFIX = "controller";

	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	public ControllerProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* (@org.springframework.stereotype.Controller *).*(..))")
	public void anyMethodInClassAnnotatedWithController() {
		/* pointcut declaration */
	}

	@Pointcut("execution(* (@innowake.mining.server.controller.MiningRestController *).*(..))")
	public void anyMethodInClassAnnotatedWithMiningRestController() {
		/* pointcut declaration */
	}

	@Around("anyMethodInClassAnnotatedWithController() || anyMethodInClassAnnotatedWithMiningRestController()")
	public Object profileController(final ProceedingJoinPoint pjp) throws Throwable {
		if (DefaultProfiler.getLevel(PROFILING_CATEGORY_PREFIX) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final String className = ClassUtils.getUserClass(pjp.getThis()).getSimpleName();
		final String profilingId = PROFILING_CATEGORY_PREFIX + "." + className;
		if (DefaultProfiler.getLevel(profilingId) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final String methodName = pjp.getStaticPart().getSignature().getName();
		final Profiler profiler = profilingSession.getProfiler(profilingId);
		LOG.trace(() -> "profiling " + methodName + " on " + className);
		profiler.start(methodName);
		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}
}
