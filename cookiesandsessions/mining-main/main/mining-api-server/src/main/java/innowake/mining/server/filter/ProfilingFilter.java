/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.filter;

import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.mining.server.config.Profiles;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import java.io.IOException;

/**
 * Request filter which flushes profiling metrics from the request thread after the request finishes.
 * <p>
 * The filter is active only when the {@value Profiles#PROFILING} Spring profile is enabled.
 */
@Component
@Profile(Profiles.PROFILING)
public class ProfilingFilter implements Filter {

	@Override
	public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain chain) throws IOException, ServletException {
		chain.doFilter(request, response);
		ProfilingFactory.getProfilingSession().flushCurrentThread();
	}
}
