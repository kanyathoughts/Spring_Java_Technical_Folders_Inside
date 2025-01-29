/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.filter;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.CacheControl;
import org.springframework.http.HttpHeaders;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.Duration;
import java.util.Arrays;

/**
 * Request filter configuration that applies "Cache-Control: max-age=31536000" header on all static artifacts.
 */
@Configuration
public class CacheControlFilterConfig {

	private static class CacheControlFilter implements Filter {
		@Override
		public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain chain) throws IOException, ServletException {
			((HttpServletResponse) response).setHeader(HttpHeaders.CACHE_CONTROL, CacheControl.maxAge(Duration.ofDays(365)).getHeaderValue());
			chain.doFilter(request, response);
		}
	}

	@Bean
	public FilterRegistrationBean<CacheControlFilter> cacheControlFilter() {
		final FilterRegistrationBean<CacheControlFilter> reg = new FilterRegistrationBean<>(new CacheControlFilter());
		reg.setUrlPatterns(Arrays.asList("*.js", "*.css", "*.ttf", "*.woff2", "*.woff", "*.svg", "*.png", "*.jpg", "*.ico"));
		return reg;
	}
}
