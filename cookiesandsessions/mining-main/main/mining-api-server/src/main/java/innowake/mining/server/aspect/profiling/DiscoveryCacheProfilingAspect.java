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
 * Aspect which enables profiling of {@link innowake.mining.data.discovery.metrics.DiscoveryCache}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class DiscoveryCacheProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryCacheProfilingAspect.class);

	private static final String PROFILING_CATEGORY_PREFIX = "discovery.cache";

	private final ProfilingSession profilingSession;
	private final Profiler profiler;
	
	public DiscoveryCacheProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
		
		profilingSession = ProfilingFactory.getProfilingSession();
		profiler = profilingSession.getProfiler(PROFILING_CATEGORY_PREFIX);
	}

	@Pointcut("execution(* innowake.mining.data.discovery.metrics.DiscoveryCache.*(..))")
	public void anyMethodOnDiscoveryCache() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnDiscoveryCache()")
	public Object profileDiscoveryCache(final ProceedingJoinPoint pjp) throws Throwable {
		if (DefaultProfiler.getLevel(PROFILING_CATEGORY_PREFIX) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final String methodName = pjp.getStaticPart().getSignature().getName();

		/* if "trace" logging is enabled for this profiling category, collect the lock key or map key */
		if (DefaultProfiler.getLevel(PROFILING_CATEGORY_PREFIX) == DefaultProfiler.Level.WITH_NMDC_AND_MULTIPLE_SUB_IDS) {
			final Object[] args = pjp.getArgs();
			/* key is always the second argument after jobId */
			if (args.length > 1 && args[1] instanceof String) {
				final String key = (String) args[1];
				LOG.trace(() -> "profiling " + methodName + " for key " + key + " on DiscoveryCache");
				profiler.start(methodName, key);
			} else {
				profiler.start(methodName);
			}
		} else {
			profiler.start(methodName);
		}

		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}
}
