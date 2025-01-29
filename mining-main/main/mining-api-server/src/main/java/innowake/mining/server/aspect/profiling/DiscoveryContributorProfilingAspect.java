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
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourcePojo;
import org.apache.commons.lang.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.util.ClassUtils;

/**
 * Aspect which enables profiling of methods of {@link innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor}
 * and {@link innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class DiscoveryContributorProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryContributorProfilingAspect.class);

	private static final String PROFILING_CATEGORY_PREFIX = "discovery.contributor";

	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	public DiscoveryContributorProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor+.*(..))")
	public void anyMethodOnDiscoveryContributor() {
		/* pointcut declaration */
	}

	@Pointcut("execution(* innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource+.*(..))")
	public void anyMethodOnDiscoveryContributorFromSource() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnDiscoveryContributor()")
	public Object profileContributorAdvice(final ProceedingJoinPoint pjp) throws Throwable {
		return profileContributor(pjp);
	}

	@Around("anyMethodOnDiscoveryContributorFromSource()")
	public Object profileContributorFromSourceAdvice(final ProceedingJoinPoint pjp) throws Throwable {
		return profileContributor(pjp);
	}

	private Object profileContributor(final ProceedingJoinPoint pjp) throws Throwable {
		final String contributorName = ClassUtils.getUserClass(pjp.getThis()).getSimpleName();
		final String profilingId = PROFILING_CATEGORY_PREFIX + "." + contributorName;
		if (DefaultProfiler.getLevel(profilingId) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final Profiler profiler = profilingSession.getProfiler(profilingId);
		final String methodName = pjp.getStaticPart().getSignature().getName();
		final String moduleName = getModuleName(pjp);
		LOG.trace(() -> "profiling " + methodName + " on " + contributorName + " " + moduleName);
		profiler.start(methodName, moduleName);
		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}

	private String getModuleName(final ProceedingJoinPoint pjp) {
		String name = null;
		String path = null;
		for (final Object arg : pjp.getArgs()) {
			if (ModulePojo.class.isAssignableFrom(arg.getClass())) {
				name = ((ModulePojo) arg).getName();
				path = ((ModulePojo) arg).getPath().orElse(null);
				if (path == null) {
					path = ((ModulePojo) arg).getParentPath().orElse(null);
				}
			} else if (SourcePojo.class.isAssignableFrom(arg.getClass())) {
				name = ((SourcePojo) arg).getName();
				path = ((SourcePojo) arg).getPath();
			}
		}

		return StringUtils.trimToEmpty(name) + "(" + StringUtils.trimToEmpty(path) + ")";
	}
}
