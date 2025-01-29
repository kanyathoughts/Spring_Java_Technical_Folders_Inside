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
import innowake.mining.shared.entities.ModuleLightweightPojo;
import org.apache.commons.lang.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.util.ClassUtils;

/**
 * Enables profiling of classes implementing the {@link innowake.mining.server.datalineage.operations.DataLineageResolver} interface.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class DataLineageResolverProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(DataLineageResolverProfilingAspect.class);

	private static final String PROFILING_CATEGORY_PREFIX = "datalineage.tracer";

	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	public DataLineageResolverProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* innowake.mining.server.datalineage.operations.DataLineageResolver+.*(..))")
	public void anyMethodOnDataLineageResolver() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnDataLineageResolver()")
	public Object profileResolverAdvice(final ProceedingJoinPoint pjp) throws Throwable {
		return profileResolver(pjp);
	}

	private Object profileResolver(final ProceedingJoinPoint pjp) throws Throwable {
		final String resolverName = ClassUtils.getUserClass(pjp.getThis()).getSimpleName();
		final String profilingId = PROFILING_CATEGORY_PREFIX + "." + resolverName;
		if (DefaultProfiler.getLevel(profilingId) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final Profiler profiler = profilingSession.getProfiler(profilingId);
		final String methodName = pjp.getStaticPart().getSignature().getName();
		final String moduleName = getModuleName(pjp);
		LOG.trace(() -> "profiling " + methodName + " on " + resolverName + " " + moduleName);
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
			if (ModuleLightweightPojo.class.isAssignableFrom(arg.getClass())) {
				name = ((ModuleLightweightPojo) arg).getName();
				path = ((ModuleLightweightPojo) arg).getPath();
				if (path == null) {
					path = ((ModuleLightweightPojo) arg).getParentPath();
				}
			}
		}

		return StringUtils.trimToEmpty(name) + "(" + StringUtils.trimToEmpty(path) + ")";
	}
}
