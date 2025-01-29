/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;

import org.apache.http.HttpStatus;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;
import org.junit.ClassRule;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.service.Result;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.LoginRequest;

/**
 * Base class for integration tests.
 * <p>
 * The integration tests can be skipped by providing the system property {@code -DskipIntegrationTests=true}
 */
public class IntegrationBaseTest {
	
	protected static final String TEST_URL_SYSTEM_PROPERTY = "test.url";
	protected static final String DEFAULT_TEST_URL = "http://localhost:8080";
	protected static final String OAUTH_URL = System.getProperty(TEST_URL_SYSTEM_PROPERTY, DEFAULT_TEST_URL) + "/api/v1/legacy-auth/login";
	protected static final String OAUTH_USER = "mining-plugin-client"; /* see AuthorizationServerConfig in mining-api-server */
	protected static final String OAUTH_PASSWORD = "mining-client-secret"; /* see AuthorizationServerConfig in mining-api-server */
	protected static final String MINING_USERNAME = "admin";
	protected static final String MINING_PASSWORD = "Worx2000";
	protected static final String TEST_CLIENT_NAME = "Mining Plugin Test Client";
	protected static final String TEST_PROJECT_NAME = "Mining Plugin Test Project";

	@ClassRule
	public static IntegrationTestRule integrationTestRule = new IntegrationTestRule();
	
	@Nullable
	private static String accessToken;

	/**
	 * Creates a test client and test project if not already available.
	 */
	public IntegrationBaseTest() {
		/* The tests require at least 1 client and 1 project so we just create some test client and project.
		 * we do this only once per test run by putting it in the static initializer of this class */
		/* check if the test client exists, if not create it */
		final Optional<ClientPojo[]> clientsResult = MiningServiceExecutor
			.create(() -> MiningApiClient.clientService(getConnectionInfo()).findAllClients())
			.setInvalidResultConsumer(result -> Assert.fail(result.getExtendedStatusMessage()))
			.setExceptionConsumer(exception -> Assert.fail(exception.toString()))
			.execute();
		
		final ClientPojo[] clients = clientsResult.orElseThrow(() -> new AssertionError("Clients must exist"));
		final Optional<ClientPojo> existingClient = Arrays.stream(clients).filter(client -> TEST_CLIENT_NAME.equals(client.getName())).findFirst();

		final EntityId clientId;
		if (existingClient.isPresent()) {
			clientId = existingClient.get().identity();
		} else {
			final ClientPojoPrototype client = new ClientPojoPrototype();
			client.setName(TEST_CLIENT_NAME);
			Logging.info(String.format("Creating test client: %s", client.toString()));
			final Optional<ClientPojo> createdClient = MiningServiceExecutor
					.create(() -> MiningApiClient.clientService(getConnectionInfo()).createClient().setClient(client))
					.setInvalidResultConsumer(result -> Assert.fail(result.getExtendedStatusMessage()))
					.setExceptionConsumer(exception -> Assert.fail(exception.toString()))
					.execute();

			if ( ! createdClient.isPresent()) {
				Assert.fail("Client creation failed");
			} else {
				Logging.info("Client successfully created");
			}
			clientId = createdClient.get().identity();
		}

		/* check if the test project exists, if not create it */
		final Optional<ProjectPojo[]> projectsResult = MiningServiceExecutor
			.create(() -> MiningApiClient.projectService(getConnectionInfo()).findAllProjects())
			.setInvalidResultConsumer(result -> Assert.fail(result.getExtendedStatusMessage()))
			.setExceptionConsumer(exception -> Assert.fail(exception.toString()))
			.execute();
		
		final ProjectPojo[] projects = projectsResult.orElseThrow(() -> new AssertionError("Projects must exist"));
		final Optional<ProjectPojo> existingProject = Arrays.stream(projects).filter(project -> TEST_PROJECT_NAME.equals(project.getName())).findFirst();
		
		if ( ! existingProject.isPresent()) {
			final ProjectPojoPrototype project = new ProjectPojoPrototype();
			project.setName(TEST_PROJECT_NAME);
			project.setClient(clientId);

			Logging.info(String.format("Creating test project: %s", project.toString()));

			MiningServiceExecutor
			.create(() -> MiningApiClient.projectService(getConnectionInfo()).createProject().setProject(project))
			.setInvalidResultConsumer(result -> Assert.fail(result.getExtendedStatusMessage()))
			.setExceptionConsumer(exception -> Assert.fail(exception.toString()))
			.execute();

			Logging.info("Project successfully created");
		}
	}
	
	public static ConnectionInfo getConnectionInfo() {
		String token = accessToken;
		if (token == null) {
			/* obtain oauth token - refer to https://confluence.innowake.hq/display/KB/User+Management */
			final HttpPost oAuthRequest = new HttpPost(OAUTH_URL);
			final LoginRequest loginReq = new LoginRequest("admin", "Worx2000");

			try {
				oAuthRequest.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(loginReq), ContentType.APPLICATION_JSON));
			} catch (final Exception e) {
				throw new IllegalStateException("Unable to send login request: " + e );
			}

			try (final CloseableHttpClient client = HttpClients.createDefault(); final CloseableHttpResponse response = client.execute(oAuthRequest)) {
				if (response.getStatusLine().getStatusCode() != HttpStatus.SC_OK) {
					throw new IllegalStateException("Unable to authenticate: server responded with status code " + response.getStatusLine().getStatusCode());
				}

				final Map<String, String> authResult = PojoMapper.jsonReaderFor(Map.class).readValue(EntityUtils.toString(response.getEntity()));

				token = authResult.get("access_token");
				if (token == null) {
					throw new IllegalStateException("Unable to authenticate: server response did not include \"access_token\": " + authResult);
				}

				accessToken = token;
			} catch (final IOException e) {
				throw new IllegalStateException("Unable to send login request: ", e);
			}
		}

		return new ConnectionInfo(System.getProperty(TEST_URL_SYSTEM_PROPERTY, DEFAULT_TEST_URL), token);
	}
	
	/**
	 * Activates a given feature.
	 *
	 * @param connectionInfo the connection info for the server the feature should be enabled on
	 * @param feature the feature to enable
	 */
	public static void activateFeature(final ConnectionInfo connectionInfo, final FeatureId feature) {
		try {
			final Result<Void> result = MiningApiClient.featureService(connectionInfo)
					.toggleFeature()
					.setFeatureId(feature)
					.setState(Boolean.TRUE)
					.execute();
			
			if ( ! result.isValid()) {
				throw new IllegalStateException(String.format("Could not activate feature '%s':%n%s", 
						feature, result.getExtendedStatusMessage()));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

}