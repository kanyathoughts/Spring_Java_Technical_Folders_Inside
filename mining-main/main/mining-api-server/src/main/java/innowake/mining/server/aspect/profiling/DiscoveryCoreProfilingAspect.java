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

/**
 * Aspect which enables profiling of {@link innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class DiscoveryCoreProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryCoreProfilingAspect.class);

	private static final String PROFILING_CATEGORY = "discovery.core";

	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	public DiscoveryCoreProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore+.*(..))")
	public void anyMethodOnDiscoveryCore() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnDiscoveryCore()")
	public Object profileDiscoveryCore(final ProceedingJoinPoint pjp) throws Throwable {
		if (DefaultProfiler.getLevel(PROFILING_CATEGORY) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final String methodName = pjp.getStaticPart().getSignature().getName();
		final Profiler profiler = profilingSession.getProfiler(PROFILING_CATEGORY);
		LOG.trace(() -> "profiling " + methodName + " on DiscoveryCore");
		profiler.start(methodName);
		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}
}
