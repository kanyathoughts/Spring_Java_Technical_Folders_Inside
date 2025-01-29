/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Duration;
import java.time.Instant;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.sql.DataSource;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInfo;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.metamodel.MetamodelServiceProvider;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Base class for all integration tests.
 */
public abstract class IntegrationTest {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(IntegrationTest.class);

	private static final String ACCESS_TOKEN = "5COgpjv7HdUiRQrj0LwEsyiJnhE";
	private static final String TEST_URL_SYSTEM_PROPERTY = "test.url";
	private static final String DEFAULT_TEST_URL = "http://127.0.0.1:8080";
	private static final String TEST_DB_POSTGRES_URL_SYSTEM_PROPERTY = "test.db.postgres.url";
	private static final String DB_URL_POSTGRES = "jdbc:postgresql:mining";
	private static final String TEST_DB_POSTGRES_USER_SYSTEM_PROPERTY = "test.db.postgres.user";
	private static final String DEFAULT_POSTGRES_DB_USER = "mining";
	private static final String TEST_DB_PASSWORD_SYSTEM_PROPERTY = "test.db.password";	
	private static final String DEFAULT_TEST_DB_PASSWORD = "Worx2000";
	private static final String TEST_DATA_RESOURCE_POSTGRES = "/test-data-postgres.sql";
	protected static final String SOURCE_CODE_NOT_FOUND = "Source code missing: Can not identify Module with Id: '[uid=someUid,nid=%s]' with Name: '%s'";
	
	@Nullable
	protected static Connection connectionPostgres;
	@Nullable
	private static DataSource dataSource;
	@Nullable
	private static String statementResetPostgres;
	
	private final ConnectionInfo connectionInfo = getConnectionInfo();
	private final ProjectServiceProvider projectServiceProvider = MiningApiClient.projectService(connectionInfo);
	protected final MetamodelServiceProvider metaModelServiceProvider = MiningApiClient.metaModelService(connectionInfo);
	private final JobServiceProvider jobServiceProvider = MiningApiClient.jobService(getConnectionInfo());
	
	protected interface QueryResultConsumer {
		void accept(Statement st) throws SQLException;
	}
	
	private static Connection tryConnect(final String url, final Properties info) {
		for (int i = 0; i < 4; i++) {
			try {
				return DriverManager.getConnection(url, info);
			} catch (final Exception e) {
				LOGGER.error("[" + url + "] " + e.getMessage());
				try {
					TimeUnit.SECONDS.sleep(30);
				} catch (final InterruptedException e1) {
					Thread.currentThread().interrupt();
					throw new IllegalStateException(e1);
				}
			}
		}
		throw new IllegalStateException("Failed to establish connection to " + url);
	}
	
	private static Connection ensurePostgresConnection() throws SQLException {
		Connection connection = connectionPostgres;
		if (connection == null || connection.isClosed()) {
			final Properties info = new Properties();
			info.put("user", System.getProperty(TEST_DB_POSTGRES_USER_SYSTEM_PROPERTY, DEFAULT_POSTGRES_DB_USER));
			info.put("password", System.getProperty(TEST_DB_PASSWORD_SYSTEM_PROPERTY, DEFAULT_TEST_DB_PASSWORD));
			connection = connectionPostgres = tryConnect(System.getProperty(TEST_DB_POSTGRES_URL_SYSTEM_PROPERTY, DB_URL_POSTGRES), info);
		}
		
		dataSource = new SingleConnectionDataSource(connection, true);
		
		return connection;
	}
	
	protected static JdbcTemplate getDataSource() {
		return new JdbcTemplate(Objects.requireNonNull(dataSource, "Postgres DataSource not available"));
	}
	
	public static void prepareResetPostgres() throws SQLException {
		ensurePostgresConnection();
		try {
			statementResetPostgres = IOUtils.toString(IntegrationTest.class.getResource(TEST_DATA_RESOURCE_POSTGRES), StandardCharsets.UTF_8);
		} catch (final IOException e) {
			throw new IllegalStateException("Could not read " + TEST_DATA_RESOURCE_POSTGRES, e);
		}
	}
	
	@BeforeAll
	public static void setup() throws ClassNotFoundException, SQLException {
		Class.forName("org.postgresql.Driver");
		prepareResetPostgres();
	}
	
	@AfterAll
	public static void cleanup() {
		try {
			if (connectionPostgres != null) {
				connectionPostgres.close();
			}
		} catch (final SQLException e) {
			throw new IllegalStateException("Error closing database connections", e);
		}
	}
	
	@BeforeEach
	void init(TestInfo testInfo) {
		LOGGER.info("===== BEGIN TEST ===== " + getClass().getName() + ": " + testInfo.getDisplayName());
		resetTestData();
	}
	
	@AfterEach
	void fini(TestInfo testInfo) {
		LOGGER.info("===== END TEST ===== " + getClass().getName() + ": " + testInfo.getDisplayName());
	}
	
	protected void executeStatement(@Nullable final Connection connection, final String sql, @Nullable final QueryResultConsumer resultHandler) {
		if (connection == null) {
			throw new IllegalStateException("Connection was null");
		}
		try (Statement st = connection.createStatement()) {
			st.execute(sql);
			if (resultHandler != null) {
				resultHandler.accept(st);
			}
		} catch (SQLException e) {
			throw new IllegalStateException(e);
		}
	}
	
	public void resetTestData() {
		LOGGER.info("===== BEGIN resetTestData ===== ");
		try {
			if (statementResetPostgres != null && connectionPostgres != null) {
				try (Statement st = connectionPostgres.createStatement()) {
					st.execute(statementResetPostgres);
				} catch (SQLException e) {
					throw new IllegalStateException("Error resetting Postgres database", e);
				}
			} else {
				throw new IllegalStateException("Postgres connection and reset statement should not be null.");
			}
			assertEquals("Resetting project configuration",
					200, projectServiceProvider.resetProjectConfiguration().setProjectId(EntityId.of(1l)).execute().getStatusCode());
			refreshMetamodel();
		} catch (IOException e) {
			throw new IllegalStateException("IOException during DB reset", e);
		}
		LOGGER.info("===== END resetTestData ===== ");
	}
	
	/**
	 * Create the connection info for the expected mining-api-server instance on integration tests.
	 * See readme.txt for ports and details
	 *
	 * @return The connection info.
	 */
	protected ConnectionInfo getConnectionInfo() {
		return new ConnectionInfo(System.getProperty(TEST_URL_SYSTEM_PROPERTY, DEFAULT_TEST_URL), ACCESS_TOKEN);
	}
	
	/**
	 * Refresh the meta model constituting the data point schema for all projects.
	 * This must be called to register changes to custom property definitions on a running mining-api-server.
	 *
	 * @throws IOException In case of a communication error.
	 */
	protected void refreshMetamodel() throws IOException {
		assertEquals("Refreshing datapoint meta model",
				200, metaModelServiceProvider.refresh().execute().getStatusCode());
	}
	
	/**
	 * Refresh the meta model constituting the data point schema for a specific project.
	 * This must be called to register changes to custom property definitions on a running mining-api-server.
	 * @param projectId The ID of the project to refresh.
	 *
	 * @throws IOException In case of a communication error.
	 */
	protected void refreshMetamodel(final EntityId projectId) throws IOException {
		metaModelServiceProvider.refresh().setProjectId(projectId).execute();
	}

	protected void waitForJobCompletion(final String jobId, final int timeoutInMinutes) throws IOException, TimeoutException {
		JobInformation jobInfo = null;
		boolean jobIsDone = false;
		final Instant start = Instant.now();

		do {
			final Result<JobInformation> jobInfoResult = jobServiceProvider.getJobInfo().setJobId(jobId).execute();
			assertNotNull(jobInfoResult);
			assertTrue(jobInfoResult.getValue().isPresent());
			jobInfo = jobInfoResult.getValue().get();

			final JobStatus status = jobInfo.getStatus();
			if (status == JobStatus.RUNNING || status == JobStatus.SCHEDULED) {
				assertNull(jobInfo.getResultStatus());
				assertNull(jobInfo.getFinishTime());
			} else if (status == JobStatus.SUCCESS || status == JobStatus.FAILURE ||
					status == JobStatus.CANCELED || status == JobStatus.TIMEOUT) {
				jobIsDone = true;
			}
			if (Duration.between(start, Instant.now()).toMinutes() >= timeoutInMinutes) {
				throw new TimeoutException(String.format("Test timed out waiting for Job [%s] with status [%s].",
						jobInfo.getJobName(),
						jobInfo.getStatus().name()));
			}
		} while ( ! jobIsDone);
	}
}
