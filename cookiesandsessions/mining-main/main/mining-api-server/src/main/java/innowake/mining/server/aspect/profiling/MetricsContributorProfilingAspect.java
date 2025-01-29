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
import innowake.mining.data.discovery.metrics.MetricsContributor;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.config.Profiles;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.*;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

/**
 * Aspect which enables profiling of the calculate*Metrics() methods of {@link MetricsContributor}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class MetricsContributorProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(MetricsContributorProfilingAspect.class);

	private static final String PROFILING_CATEGORY_PREFIX = "discovery.contributor";
	
	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	public MetricsContributorProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* innowake.mining.data.discovery.metrics.MetricsContributor+.calculateGenericMetrics(..))")
	public void calculateGenericMetrics() {
		/* pointcut declaration */
	}

	@Pointcut("execution(* innowake.mining.data.discovery.metrics.MetricsContributor+.calculateDependentMetrics(..))")
	public void calculateDependentMetrics() {
		/* pointcut declaration */
	}

	@Pointcut("execution(* innowake.mining.data.discovery.metrics.MetricsContributor+.calculateTransitiveMetrics(..))")
	public void calculateTransitiveMetrics() {
		/* pointcut declaration */
	}

	@Around("calculateGenericMetrics()")
	public Object profileGenericMetrics(final ProceedingJoinPoint pjp) throws Throwable {
		return profileContributor(pjp, "calculateGenericMetrics");
	}

	@Around("calculateDependentMetrics()")
	public Object profileDependentMetrics(final ProceedingJoinPoint pjp) throws Throwable {
		return profileContributor(pjp, "calculateDependentMetrics");
	}

	@Around("calculateTransitiveMetrics()")
	public Object profileTransitiveMetrics(final ProceedingJoinPoint pjp) throws Throwable {
		return profileContributor(pjp, "calculateTransitiveMetrics");
	}

	private Object profileContributor(final ProceedingJoinPoint pjp, final String methodName) throws Throwable {
		final String profilingId = PROFILING_CATEGORY_PREFIX + "." + methodName;
		if (DefaultProfiler.getLevel(profilingId) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final Profiler profiler = profilingSession.getProfiler(profilingId);
		final String contributorName = ((MetricsContributor) pjp.getThis()).getName();
		final ModelArtifact artifact = ((ModelArtifact) pjp.getArgs()[0]);
		final String artifactName = artifact.getName() + "(" + artifact.getPath().orElse("") + ")";
		LOG.trace(() -> "profiling " + methodName + " on " + contributorName + " " + artifactName);
		profiler.start(contributorName, artifactName);
		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}
}
