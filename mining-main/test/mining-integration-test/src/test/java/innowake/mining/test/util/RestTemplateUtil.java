/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test.util;

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import innowake.mining.client.ConnectionInfo;

/**
 * Utility class to aid testing using RestTemplate.
 */
public class RestTemplateUtil {

	private RestTemplateUtil() {
		/* Empty constructor since this is a Utility class. */
	}
	
	/**
	 * Method to generate the HTTP Header that contains the Authentication Token.
	 *
	 * @param info The Connection Information which contains the Authentication Token
	 * @return The HTTP Header that can be used for REST calls
	 */
	public static HttpHeaders getHttpHeaders(final ConnectionInfo info) {
		final HttpHeaders headers = new HttpHeaders();
		headers.set("Authorization", "Bearer " + info.getToken());
		headers.setContentType(MediaType.APPLICATION_JSON);
		return headers;
	}
}
