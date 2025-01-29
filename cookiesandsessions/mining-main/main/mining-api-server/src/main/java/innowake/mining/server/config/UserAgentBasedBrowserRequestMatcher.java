/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.config;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.springframework.security.web.util.matcher.RequestMatcher;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Matches user-agent header with the major browsers
 */
public class UserAgentBasedBrowserRequestMatcher implements RequestMatcher {

	private static final Logger LOG = LoggerFactory.getLogger(UserAgentBasedBrowserRequestMatcher.class);
	private static final String USER_AGENT_HEADER = "user-agent";
	private static final Pattern USER_AGENT_PATTERN = Pattern.compile("Chrom|Edg|Firefox|Safari|Opera|Trident|MSIE");

	@Override
	public boolean matches(final HttpServletRequest request) {
		final String userAgent = request.getHeader(USER_AGENT_HEADER);
		if (StringUtils.isBlank(userAgent)) {
			return false;
		}

		final Matcher matcher = USER_AGENT_PATTERN.matcher(userAgent);
		if (matcher.find()) {
			LOG.debug(() -> "Browser-based client detected: " + userAgent);
			return true;
		} else {
			LOG.debug(() -> "Non-browser-based client detected: " + userAgent);
			return false;
		}
	}
}
