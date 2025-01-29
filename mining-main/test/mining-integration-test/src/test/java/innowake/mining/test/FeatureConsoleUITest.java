/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import java.io.IOException;
import java.io.StringWriter;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.junit.Assert;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;

/**
 * Integration test to check if the feature console UI is accessible.
 */
class FeatureConsoleUITest extends IntegrationTest{

	/**
	 * Test a simple successful call to feature console UI
	 * 
	 * @throws IOException Service call failed.
	 * @throws ClientProtocolException Service call failed.
	 */
	@Test
	void featureConsoleTest() throws ClientProtocolException, IOException {
		final HttpGet accessFeatureConsole = new HttpGet(getConnectionInfo().getUrl() + "/feature-console/features");
		accessFeatureConsole.setHeader(new BasicHeader(HttpHeaders.AUTHORIZATION, "Bearer " + getConnectionInfo().getToken()));
		try (@Nullable final CloseableHttpResponse response = HttpClients.createDefault().execute(accessFeatureConsole)) {
			Assert.assertNotNull(response);
			Assert.assertEquals(HttpStatus.SC_OK, response.getStatusLine().getStatusCode());
			final StringWriter writer = new StringWriter();
			IOUtils.copy(response.getEntity().getContent(), writer, "UTF-8");
			Assert.assertTrue(writer.toString().contains("FF4J - Features"));
		}
	}
}
