/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.server;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.plugin.deeplink.EclipseApi;
import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * This class takes care of handling CORS requests
 */
public class CorsFilter implements Filter {
	
	private static String removeTrailingSlash(final String str) {
		if (str != null && str.endsWith("/")) {
			return str.substring(0, str.length() - 1);
		}
		return str;
	}
	
	private final Set<String> allowedOriginHosts = new HashSet<>();

	@Override
	public void doFilter(final ServletRequest servletRequest, final ServletResponse servletResponse, final FilterChain chain) throws IOException, ServletException {
		final HttpServletRequest request = (HttpServletRequest) servletRequest;
		final HttpServletResponse response = (HttpServletResponse) servletResponse;

		final String remoteHost = removeTrailingSlash(request.getHeader("Origin"));
		if ( ! allowedOriginHosts.contains(remoteHost)) {
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			return;
		}
		
		response.addHeader("Access-Control-Allow-Origin", remoteHost);
		response.addHeader("Access-Control-Allow-Methods", "POST, HEAD");
		response.addHeader("Access-Control-Allow-Credentials", "true");
		response.addHeader("Access-Control-Allow-Headers", "origin, content-type, cache-control, accept, options, authorization, x-requested-with");
		
		
		if ("OPTIONS".equals(request.getMethod())) {
			response.setStatus(HttpServletResponse.SC_ACCEPTED);
			return;
		}
		
		chain.doFilter(request, servletResponse);
	}

	@Override
	public void destroy() {
		/* This method needs to be implemented, but needs no content */
	}

	@Override
	public void init(final FilterConfig arg0) throws ServletException {	
		EclipseApi.getMiningProjects(data -> true).forEach(project -> {
			final String host = MiningPreferences.getProjectStore(project).getString(MiningPreferences.KEY_API_SERVER);
			allowedOriginHosts.add(removeTrailingSlash(host));
		});
		
		final String host = MiningPreferences.getWorkbenchStore().getString(MiningPreferences.KEY_API_SERVER);
		allowedOriginHosts.add(removeTrailingSlash(host));
	}
}
