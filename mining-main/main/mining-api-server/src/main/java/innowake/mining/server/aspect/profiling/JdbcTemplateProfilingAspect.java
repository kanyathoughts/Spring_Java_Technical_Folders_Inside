/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.aspect.profiling;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.api.profiling.ProfilingSession;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.mining.data.access.postgres.PgDao;
import innowake.mining.server.config.Profiles;

import org.apache.logging.log4j.ThreadContext;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * Aspect which enables profiling of {@link org.springframework.jdbc.core.JdbcTemplate}.
 * <p>
 * This aspect is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Aspect
@Component
@Profile(Profiles.PROFILING)
public class JdbcTemplateProfilingAspect {

	private static final Logger LOG = LoggerFactory.getLogger(JdbcTemplateProfilingAspect.class);

	private static final String PROFILING_CATEGORY_PREFIX = "db.jdbc";

	private final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();
	private static final Pattern ridPattern = Pattern.compile("#\\d+:\\d+");

	public JdbcTemplateProfilingAspect() {
		/* this class doesn't do anything meaningful without profiling enabled, so we might as well enable it here */
		DefaultProfiler.setProfilingEnabled(true);
	}

	@Pointcut("execution(* org.springframework.jdbc.core.JdbcTemplate.*(..))")
	public void anyMethodOnJdbcTemplate() {
		/* pointcut declaration */
	}

	@Around("anyMethodOnJdbcTemplate()")
	public Object profileJdbc(final ProceedingJoinPoint pjp) throws Throwable {
		final String methodName = pjp.getStaticPart().getSignature().getName();
		final String profilingId = PROFILING_CATEGORY_PREFIX + "." + methodName;
		if (DefaultProfiler.getLevel(profilingId) == DefaultProfiler.Level.OFF) {
			return pjp.proceed();
		}

		final Profiler profiler = profilingSession.getProfiler(profilingId);

		/* if "trace" logging is enabled for this profiling category, collect the statement string */
		if (DefaultProfiler.getLevel(profilingId) == DefaultProfiler.Level.WITH_NMDC_AND_MULTIPLE_SUB_IDS) {
			final String stmt;

			/* most JdbcTemplate methods that we are interested in take the query as first argument */
			final Object[] args = pjp.getArgs();
			if (args.length > 0 && args[0] instanceof String) {
				stmt = sanitizeStatement((String) args[0]);
			} else {
				/* PgDao.QueryBuilder.buildQuery() puts the current statement in the ThreadContext, so we can retrieve it here */
				stmt = sanitizeStatement(ThreadContext.get(PgDao.CURRENT_STATEMENT_MDC_KEY));
			}
			
			if (stmt == null) {
				LOG.trace(() -> "profiling " + methodName + " with unknown statement on JdbcTemplate");
				profiler.start();
			} else {
				LOG.trace(() -> "profiling " + methodName + " with statement " + stmt + " on JdbcTemplate");
				profiler.start(stmt);
			}
		} else {
			profiler.start();
		}

		try {
			return pjp.proceed();
		} finally {
			profiler.stop();
		}
	}

	@Nullable
	public static String sanitizeStatement(@Nullable final String stmt) {
		/* We often put recordIds ("#123:456") and nested objects (JSON) as literals into the SQL strings, instead of using parameterized (?) queries.
		 * This causes each execution of the same statement with different record id (or value in a nested object) to be profiled as a separate "new" statement.
		 * This "sanitizing" replaces everything that looks like a record id with "?" and all nested objects with "{}", so the profiler always "sees" the same
		 * statement string and profiles it as one statement.
		 * 
		 * DELETE FROM #123:456 WHERE attributes = { "boss": "Egon", "driver": "Benny", "procurer": "Kjeld" } ===> DELETE FROM ? WHERE attributes = ?
		 */
		if (stmt == null) {
			return null;
		}
		
		/* remove nested objects ({...}) from statements */
		final List<Integer> cutPoints = new ArrayList<>();
		int braceCount = 0;
		for (int i = 0; i < stmt.length(); i++) {
			if (stmt.charAt(i) == '{') {
				if (braceCount == 0) {
					cutPoints.add(i + 1);
				}
				braceCount++;
			} else if (stmt.charAt(i) == '}') {
				braceCount--;
				if (braceCount == 0) {
					cutPoints.add(i);
				}
			}
		}
		final StringBuilder sanitized = new StringBuilder(stmt.length());
		int lastCut = 0;
		for (int i = 0; i < cutPoints.size() - 1; i++) {
			sanitized.append(stmt, lastCut, cutPoints.get(i));
			lastCut = cutPoints.get(++i);
		}
		if (lastCut < stmt.length()) {
			sanitized.append(stmt.substring(lastCut));
		}
		
		/* we do a lot of CREATE EDGE FROM ... TO ... with "hard-coded" rids in the query string
		 * replace everything that looks like a rid with "?" */
		return ridPattern.matcher(sanitized.toString()).replaceAll("?");
	}
}
