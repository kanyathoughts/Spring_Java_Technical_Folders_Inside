/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Optional;

import org.apache.http.cookie.Cookie;
import org.apache.http.impl.client.BasicCookieStore;

import innowake.lib.core.api.lang.Nullable;

/**
 * Bean to hold connection info.
 */
public class ConnectionInfo {
	
	public static final String SESSION_COOKIE_NAME = "JSESSIONID";
	public static final int OFFLINE_TOKEN_LENGTH = 44;

	private final String url;
	@Nullable
	private final String token;
	private final BasicCookieStore cookies;
	
	/**
	 * Creates a connection info to be used in the default authentication profile.
	 * 
	 * @param url the URL to the mining backend
	 * @param token the API offline token
	 */
	public ConnectionInfo(final String url, @Nullable final String token) {
		this.url = url;
		this.token = token;
		this.cookies = new BasicCookieStore();
	}
	
	/**
	 * Gets the server URL.
	 * 
	 * @return the server URL
	 */
	public String getUrl() {
		return url;
	}	

	/**
	 * Gets the access token string in a synchronized manner.
	 * 
	 * @return The access token string or null if not set.
	 */
	@Nullable
	public String getToken() {
		final String token = this.token;
		if (token != null && token.length() == OFFLINE_TOKEN_LENGTH) {
			return "offline." + token;
		}
		return token;
	}
	
	public BasicCookieStore getCookieStore() {
		return cookies;
	}
	
	public Optional<Cookie> findSessionCookie() {
		return cookies.getCookies().stream().filter(c -> c.getName().equals(SESSION_COOKIE_NAME)).findFirst();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + url.hashCode();
		if (token != null) {
			result = prime * result + assertNotNull(token).hashCode();	
		}
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final ConnectionInfo other = (ConnectionInfo) obj;
		if (token != null) {
			if ( ! token.equals(other.token)) {
				return false;
			}
		} else if (other.token != null) {
			return false;
		}
		return url.equals(other.url);
	}

	@Override
	public String toString() {
		return new StringBuilder().append("ConnectionInfo [url=").append(url).append("]").toString();
	}

}
