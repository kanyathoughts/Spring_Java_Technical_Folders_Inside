/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.filter;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import innowake.mining.shared.extensions.MiningWebUiExtension;

/**
 * Filter adding the Content-Security-Policy Header to {@link HttpServletResponse}.
 */
@Component
public class ContentSecurityPolicyFilter implements Filter {

	/**
	 * The Content-Security-Policy Header Key.
	 */
	private static final String CONTENT_SECURITY_POLICY = "Content-Security-Policy";
	
	/**
	 * Specifies the format of the policy to be used.
	 */
	private static final String POLICY_FORMAT =
					"default-src %1$s; " +
					"script-src %1$s 'unsafe-inline' 'wasm-unsafe-eval'; " + 
					"style-src %1$s 'unsafe-inline'; " +
					"img-src %1$s data:; " +
					"font-src %1$s data:; " +
					"worker-src %1$s 'unsafe-inline' * blob:;";
	
	/**
	 * Base URL parameter specified for production deployments.
	 */
	private static final String UI_BASE_URL = "ui.base-url";
	
	/**
	 * Origin that is used when the UI_BASE_URL is not specified (used in development environments).
	 */
	private static final String FALLBACK_ORIGIN = "'self'";
	
	/**
	 * Permitted 3rd party packages.
	 */
	private static final String[] WHITELIST = new String[] {
			"https://unpkg.com/graphiql/",
			"https://unpkg.com/react@16/", 
			"https://unpkg.com/react-dom@16/" 
			};
	
	@Autowired
	private Environment environment;
	
	@Autowired(required = false)
	private MiningWebUiExtension[] extensions;
	
	/**
	 * Adds the Content-Security-Policy header to all HTTP responses.  If the application is started with the ui.base-url parameter
	 * the value of that environment variable will be used as the origin.  Otherwise the application will fall back to 'self'.
	 */
	@Override
	public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain filterChain) throws IOException, ServletException {
		if (response instanceof HttpServletResponse) {
			final List<String> trustedDomains = new ArrayList<>();
			addPrimaryDomain(trustedDomains);
			if (extensions != null) {
				addExtensionDomains(trustedDomains);
			}
			trustedDomains.addAll(Arrays.asList(WHITELIST));
			final HttpServletResponse httpResponse = (HttpServletResponse) response;
			httpResponse.addHeader(
					CONTENT_SECURITY_POLICY,
					String.format(POLICY_FORMAT, String.join(" ", trustedDomains)));
		}
		
		filterChain.doFilter(request, response);
	}
	
	private void addPrimaryDomain(final List<String> trustedDomains) {
		trustedDomains.add(environment.containsProperty(UI_BASE_URL) ? environment.getProperty(UI_BASE_URL): FALLBACK_ORIGIN);
	}
	
	private void addExtensionDomains(final List<String> trustedDomains) throws ServletException {
		for (final MiningWebUiExtension extension : extensions) {				
			if (extension.getProperties().containsKey(MiningWebUiExtension.Property.IFRAME_SRC)) {
				final String iframeSrc = StringUtils.substringBefore(extension.getProperties().get(MiningWebUiExtension.Property.IFRAME_SRC), "?");
				final URI uri;
				try {
					uri = UriComponentsBuilder.fromUriString(iframeSrc).build().toUri();
				} catch (final Exception e) {
					throw new ServletException("An error occurred parsing the IFRAME_SRC property in Mining Web UI Extension " + extension.getName(), e);
				}
				
				if (uri.getScheme() != null && uri.getAuthority() != null) {
					trustedDomains.add(String.format("%1s://%2s", uri.getScheme(), uri.getAuthority()));
				}
			}
		}
	}
}
