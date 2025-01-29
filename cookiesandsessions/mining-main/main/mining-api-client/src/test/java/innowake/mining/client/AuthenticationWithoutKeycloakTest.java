/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.apache.http.HttpStatus;
import org.junit.Ignore;
import org.junit.Test;

import innowake.mining.client.service.client.ClientServiceProvider;

/**
 * Test cases for default authentication mechanism via constructor {@link ConnectionInfo#ConnectionInfo(String, String)}. 
 */
@Ignore
public class AuthenticationWithoutKeycloakTest {

	@Test
	public void accessWithBadTokenTest() throws IOException {
		final String expectedAccessToken = "5COgpjv7HdUiRQrj0LwEsyiJnhE";
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", expectedAccessToken);
		assertEquals(expectedAccessToken, connectionInfo.getToken());
		assertEquals(HttpStatus.SC_UNAUTHORIZED, new ClientServiceProvider(connectionInfo).findAllClients().execute().getStatusCode());
	}
	
	@Test
	public void accessWithGoodTokenTest() throws IOException {
		final String expectedAccessToken = "5COgpjv7HdUiRQrj0LwEsyiJnhE";
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", expectedAccessToken);
		assertEquals(expectedAccessToken, connectionInfo.getToken());
		assertEquals(HttpStatus.SC_OK, new ClientServiceProvider(connectionInfo).findAllClients().execute().getStatusCode());
	}

}
