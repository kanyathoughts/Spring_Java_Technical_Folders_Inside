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
import innowake.mining.server.discovery.dawn.metrics.api.persistence.DiscoveryPersistence;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

/**
 * Aspect which enables profiling of {@link DiscoveryPersistence}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class DiscoveryPersistenceProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryPersistenceProfilingAspect.class);

	private static final String PROFILING_CATEGORY = "discovery.persistence";

	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	public DiscoveryPersistenceProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* innowake.mining.server.discovery.dawn.metrics.api.persistence.DiscoveryPersistence+.*(..))")
	public void anyMethodOnDiscoveryPersistence() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnDiscoveryPersistence()")
	public Object profileDiscoveryCore(final ProceedingJoinPoint pjp) throws Throwable {
		if (DefaultProfiler.getLevel(PROFILING_CATEGORY) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final String methodName = pjp.getStaticPart().getSignature().getName();
		final Profiler profiler = profilingSession.getProfiler(PROFILING_CATEGORY);
		LOG.trace(() -> "profiling " + methodName + " on DiscoveryPersistence");
		profiler.start(methodName);
		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}
}
