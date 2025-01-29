/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.config;

import org.springframework.security.web.util.matcher.NegatedRequestMatcher;
import org.springframework.security.web.util.matcher.RequestMatcher;

/**
 * For the CSRF configuration we need a RequestMatcher, which ignores the browsers, so we negate it
 */
public final class CsrfIngoreRequestMatcher {
	public static final RequestMatcher CSRF_IGNORING_REQUEST_MATCHER = new NegatedRequestMatcher(new UserAgentBasedBrowserRequestMatcher());
}
