/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import java.io.IOException;
import java.util.ArrayList;

import org.apache.http.HttpHeaders;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.LoginRequest;

/**
 * Integration tests for the oAuth2 Api access.
 * <p>
 * This set of tests must be ignored when running the integration tests with the 'no-authorization'
 * Spring profile.
 */
class ApiAuthTest extends IntegrationTest {
	
	/**
	 * Test the request for a oAuth2 mining-plugin API token with the username field.
	 *
	 * @throws ClientProtocolException Service call failed.
	 * @throws IOException Service call failed.
	 */
	@Test
	void testAdditionalInfoInRequestToken() throws ClientProtocolException, IOException {

		final HttpPost tokenGet = new HttpPost(getConnectionInfo().getUrl() + "/api/v1/legacy-auth/login");
		/* Set the user and password */

		final ArrayList<NameValuePair> postParameters = new ArrayList<>();
		final LoginRequest loginReq = new LoginRequest("admin", "Worx2000");

		tokenGet.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(loginReq), ContentType.APPLICATION_JSON));

		try (@Nullable
		final CloseableHttpResponse response = HttpClients.createDefault().execute(tokenGet)) {
			Assert.assertNotNull(response);
			Assert.assertEquals(200, response.getStatusLine().getStatusCode());
			
			final String bodyContent = EntityUtils.toString(response.getEntity());
			Assert.assertTrue(bodyContent.contains("access_token"));
			Assert.assertTrue(bodyContent.contains("token_type"));
			Assert.assertTrue(bodyContent.contains("bearer"));
		}
	}
	
	/**
	 * Test call a valid service endpoint without the auth header and expect 401 (Forbiden) as result.
	 *
	 * @throws ClientProtocolException Service call failed.
	 * @throws IOException Service call failed.
	 */
	@Test
	void failNoAuth() throws ClientProtocolException, IOException {
		final HttpGet infoGet = new HttpGet(getConnectionInfo().getUrl() + "/api/v1/info");
		try (@Nullable final CloseableHttpResponse response = HttpClients.createDefault().execute(infoGet)) {
			Assert.assertNotNull(response);
			Assert.assertEquals(401, response.getStatusLine().getStatusCode());
		}
	}

	/**
	 * Test a simple successful call to a mining api service with auth header and api token.
	 *
	 * @throws ClientProtocolException Service call failed.
	 * @throws IOException Service call failed.
	 */
	@Test
	void successWithAuthHeader() throws ClientProtocolException, IOException {
		final HttpGet infoGet = new HttpGet(getConnectionInfo().getUrl() + "/api/v1/info");
		infoGet.setHeader(new BasicHeader(HttpHeaders.AUTHORIZATION, "Bearer " + getConnectionInfo().getToken()));
		try (@Nullable final CloseableHttpResponse response = HttpClients.createDefault().execute(infoGet)) {
			Assert.assertNotNull(response);
			Assert.assertEquals(200, response.getStatusLine().getStatusCode());
		}
	}
	
}
