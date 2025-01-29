/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import static org.junit.Assert.assertFalse;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.annotation.PostConstruct;
import javax.sql.DataSource;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.ff4j.FF4j;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInfo;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.junit.jupiter.DisabledIf;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.testcontainers.containers.PostgreSQLContainer;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.migration.base.PostgresSchemaMigration;
import innowake.mining.server.MiningApiApplication;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.tags.IntegrationTest;

/**
 * Base class for all Spring Boot tests using OrientDB.
 * <p>
 * This class sets up an OrientDB container and prepares the test data once.
 * If you need to reset the test data after every test case use {@link DatabaseResettingTest} as a base-class.
 */
@SpringBootTest(classes=MiningApiApplication.class)
@ActiveProfiles({"legacy-auth","test"})
@ExtendWith(SpringExtension.class)
@TestInstance(Lifecycle.PER_CLASS)
@DisabledIf("#{ 'true'.equals( systemProperties[ 'innowake.integration.tests.skip' ] ) }")
@IntegrationTest
public abstract class DatabaseRelatedTest {
	
	private static final String POSTGRES_PROPERTY_DB_URL = "postgres.datasource.jdbc-url";
	private static final String ENABLE_POSTGRES = "postgres.enabled";
	
	private static final Logger LOG = LoggerFactory.getLogger(DatabaseRelatedTest.class);

	@BeforeEach
	public void printTest(TestInfo info) {
		final StringBuilder name = new StringBuilder(info.getDisplayName());
		info.getTestMethod().ifPresent((m) -> name.append(" (" + m.getName() + ")"));
		info.getTestClass().ifPresent((c) -> name.append(" [" + c.getCanonicalName() + "]"));
		LOG.info("##### TEST ##### " + name.toString());
	}

	@Autowired
	@Qualifier("postgres")
	private DataSource dbPostgres;
	
	private Optional<PostgreSQLContainer<?>> postgreSQLContainer = Optional.empty();

	/**
	 * System property to enable or disable integration tests that extend the {@link DatabaseRelatedTest} class.
	 * <p>All integration tests are skipped if the value is '{@code true}':</p>
	 * <pre>
	 * VM arguments: -Dinnowake.integration.tests.skip=true
	 * <pre>
	 */
	/* If you change or remove this system property then adjust the mining-api-server/pom.xml and @DisabledIf annotation in class
	 * DatabaseRelatedTest accordingly */
	public static final String SYSTEM_PROPERTY_SKIP_INTEGRATION_TESTS = "innowake.integration.tests.skip";

	/**
	 * System property to enable or disable the IncrementalDiscoveryJumpstartTest test class.
	 * <p>The test class is executed only if the value is '{@code true}':</p>
	 * <pre>
	 * VM arguments: -Dinnowake.integration.jumpstart.tests.run=true
	 * <pre>
	 */
	/* If you change or remove this system property then adjust the mining-api-server/pom.xml and @EnabledIf annotation in class
	 * IncrementalDiscoveryJumpstartTest accordingly */
	public static final String SYSTEM_PROPERTY_RUN_JUMPSTART_TESTS = "innowake.integration.jumpstart.tests.run";

	@Autowired
	protected CustomPropertiesService customPropertiesService;
	
	@Autowired
	protected ProjectService projectService;
	
	@Autowired
	private transient FF4j ff4j;
	
	/**
	 * The enum representing the different reset files.
	 */
	public enum ResetScriptFile {

		/**
		 * Contains table reset queries and test data for Client and Project entities
		 */
		MINIMAL("test-data-minimal"),
		/**
		 * Contains all the queries in {@link ResetScriptFile#MINIMAL} and test data for most of the other entities
		 */
		COMPLETE("test-data-complete");

		private final String scriptFile;

		ResetScriptFile(final String scriptFile) {
			this.scriptFile = scriptFile;
		}

		/**
		 * The script file name.
		 * 
		 * @return the script file name
		 */
		public String getScriptFile() {
			return scriptFile;
		}
	}

	static {
		ToStringBuilder.setDefaultStyle(ToStringStyle.MULTI_LINE_STYLE);
	}
	
	@PostConstruct
	void construct() {
		try {
			postgreSQLContainer = PostgresDbTestContainerService.ensureContainer();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to initialize test database container", e);
		}
	}
	
	protected JdbcTemplate getDataSource() {
		return new JdbcTemplate(Objects.requireNonNull(dbPostgres, "Postgres DataSource not available"));
	}
	
	/**
	 * Sets up the database with test data.
	 *
	 * @throws IOException if an I/O exception occurs
	 */
	@BeforeAll
	public void setup() throws IOException {
		ToStringBuilder.setDefaultStyle(ToStringStyle.MULTI_LINE_STYLE);
		resetTestData();
		ff4j.disable(FeatureId.PL1_EXPERIMENTAL.getId());
	}

	protected void resetTestData() throws IOException {
		/* use a database session from our existing connection pool to do the reset */
		final ResetScriptFile scriptFile = getScriptFile();
		try {
			PostgresSchemaMigration.executePgScript(dbPostgres, scriptFile.getScriptFile());
			uploadProjectsDefaultConfiguration();
		} catch (final Exception e) {
			throw new IOException("Resetting test data failed while executing " + getScriptFile() + " script.", e);
		}
		
		try {
			final List<String> additonalPgScripts = getAdditionalPgScriptFile();
			if ( ! additonalPgScripts.isEmpty()) {
				for (final String additonalPgScript : additonalPgScripts) {
					PostgresSchemaMigration.executePgScript(dbPostgres, additonalPgScript);
				}
				uploadProjectsDefaultConfiguration();
			}
		} catch (final Exception e) {
			throw new IOException("Resetting test data failed while executing ADDITIONAL script.", e);
		}
	}

	/**
	 * The {@link ResetScriptFile#getScriptFile()} to be executed by default.
	 *
	 * @return the SQL script file to be executed
	 */
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.MINIMAL;
	}

	/**
	 * Returns the additional SQL scripts to be executed.
	 *
	 * @return SQLs to be executed separated by semicolon
	 */
	protected String getAdditionalScript() {
		return StringUtils.EMPTY;
	}
	
	/**
	 * Optionally returns a list of additional SQL script files to be executed against Postgres.
	 *
	 * @return SQLs to be executed separated by semicolon
	 */
	protected List<String> getAdditionalPgScriptFile() {
		return Collections.emptyList();
	}
	
	/**
	 * Deletes all Custom Properties created for the provided Entity on the provided Project.
	 * Use this method when adding test cases that creates new Custom Properties since Custom Properties are not reset after test case runs.
	 *
	 * @param entityName the Entity name
	 * @param projectId the project ID
	 */
	protected void resetCustomProperties(final String entityName, final EntityId projectId) {
		final List<CustomPropertyMetadata> properties = customPropertiesService.findPropertyDefinitions(q -> q.withEntity(projectId, entityName));
		assertFalse(String.format("Custom Properties should exist for class %s, project %s", entityName, projectId), properties.isEmpty());
		properties.forEach(prop -> customPropertiesService.deleteProperty(projectId, entityName, prop.getName()));
	}
	
	public void uploadProjectsDefaultConfiguration() {
		projectService.getUids(q -> q.withIdAbove(Long.valueOf(0))) 
			.forEach(id -> projectService.resetConfiguration(EntityId.of(id)));
	}
	
	@DynamicPropertySource
	public static void overridePropertySource(final DynamicPropertyRegistry registry) {
		PostgresDbTestContainerService.ensureContainer().ifPresent(container -> {
			registry.add(POSTGRES_PROPERTY_DB_URL, container::getJdbcUrl);
			registry.add(ENABLE_POSTGRES, () -> "true");
		});
	}
	
	public Optional<PostgreSQLContainer<?>> getPostgreSQLContainer() {
		return this.postgreSQLContainer;
	}
	
}
