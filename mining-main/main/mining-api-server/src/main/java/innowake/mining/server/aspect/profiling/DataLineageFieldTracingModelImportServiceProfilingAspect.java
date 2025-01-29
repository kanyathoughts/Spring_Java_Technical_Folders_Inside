/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.aspect.profiling;

import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.api.profiling.ProfilingSession;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.mining.server.config.Profiles;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

/**
 * Aspect which enables profiling of {@link innowake.mining.server.datalineage.fieldtracing.service.FieldTracingModelImportService}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class DataLineageFieldTracingModelImportServiceProfilingAspect {

	private static final String PROFILING_CATEGORY_PREFIX = "datalineage.fieldtracing";

	private final ProfilingSession profilingSession;
	private final Profiler profiler;

	public DataLineageFieldTracingModelImportServiceProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
		
		profilingSession = ProfilingFactory.getProfilingSession();
		profiler = profilingSession.getProfiler(PROFILING_CATEGORY_PREFIX);
	}

	@Pointcut("execution(* innowake.mining.server.datalineage.fieldtracing.service.FieldTracingModelImportService.*(..))")
	public void anyMethodOnFieldTracingModelImportService() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnFieldTracingModelImportService()")
	public Object profileFieldTracingService(final ProceedingJoinPoint pjp) throws Throwable {
		if (DefaultProfiler.getLevel(PROFILING_CATEGORY_PREFIX) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final String methodName = pjp.getStaticPart().getSignature().getName();

		profiler.start(methodName);
		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
			profilingSession.flushCurrentThread();
		}
	}
}
