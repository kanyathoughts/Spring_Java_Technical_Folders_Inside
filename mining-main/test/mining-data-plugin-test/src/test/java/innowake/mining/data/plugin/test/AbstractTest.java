/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.plugin.test;

import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;
import org.flywaydb.commandline.Main;
import org.junit.Before;
import org.junit.jupiter.api.Disabled;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.BindMode;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.utility.MountableFile;

import com.orientechnologies.orient.core.db.ODatabase;
import com.orientechnologies.orient.core.db.OrientDB;
import com.orientechnologies.orient.core.db.OrientDBConfig;
import com.orientechnologies.orient.core.sql.executor.OResultSet;


/**
 * Unit tests requiring an OrientDB with our plugins installed should extend this class to ensure the ClassRule lifecycle.
 * <p>
 * The container will be stopped and closed after all tests are done. 
 * See https://www.testcontainers.org/test_framework_integration/manual_lifecycle_control/ for details
 */
@Disabled
@SuppressWarnings("resource")
public abstract class AbstractTest {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(AbstractTest.class);
	
	protected static final String TEST_DB_USER = "root";	
	protected static final String TEST_DB_PASSWORD = "Worx2000";
	protected static final String TEST_DB_NAME = "mining";
	protected static final String ORIENT_URL;

	private static final int EXPOSED_PORT = 2424;
	private static final int EXPOSED_ORIENT_STUDIO_PORT = 2480;
	
	private static final String M2_REPO = System.getenv("M2_REPO");
	
	private static final String PLUGIN_BUNDLE_PATH = 
			M2_REPO
			+ "/innowake/products/mining/mining-data-plugin/99.9.99-TRUNK-MINING-SNAPSHOT"
			+ "/mining-data-plugin-99.9.99-TRUNK-MINING-SNAPSHOT-bundle.jar";
	
	private static final String DEBUG_PLUGIN_BUNDLE_PATH = 
			M2_REPO
			+ "/innowake/products/mining/mining-data-debug-plugin/99.9.99-TRUNK-MINING-SNAPSHOT"
			+ "/mining-data-debug-plugin-99.9.99-TRUNK-MINING-SNAPSHOT-bundle.jar";

	/**
	 * Global OrientDB container instance for all tests.
	 */
	private static final GenericContainer<?> ORIENT_DB_CONTAINER;
	
	/**
	 * Setup and start Container. Load Schema. 
	 */
	static {
		if (M2_REPO != null) {
			if ( ! Files.isDirectory(Paths.get(M2_REPO))) {
				throw new IllegalArgumentException("M2_REPO (" + M2_REPO + ") not found");
			}
		} else {
			throw new IllegalArgumentException("Environment variable M2_REPO not defined");
		}
		
		ORIENT_DB_CONTAINER =
				new GenericContainer<>("orientdb:3.1.6")
				.withEnv("ORIENTDB_ROOT_PASSWORD", TEST_DB_PASSWORD)
				.withExposedPorts(Integer.valueOf(EXPOSED_PORT), Integer.valueOf(EXPOSED_ORIENT_STUDIO_PORT))
				.withLogConsumer(new Slf4jLogConsumer(LOGGER))
				.withCopyFileToContainer(MountableFile.forHostPath(PLUGIN_BUNDLE_PATH), "/orientdb/plugins/mining-data-plugin.jar")
				.withCopyFileToContainer(MountableFile.forHostPath(DEBUG_PLUGIN_BUNDLE_PATH), "/orientdb/plugins/mining-data-debug-plugin.jar")
				.withClasspathResourceMapping("CreateDatabase.sql", "/orientdb/CreateDatabase.sql", BindMode.READ_ONLY)
				.withClasspathResourceMapping("test-data.sql", "/orientdb/test-data.sql", BindMode.READ_ONLY);
		ORIENT_DB_CONTAINER.start();
		try {
			ORIENT_DB_CONTAINER.execInContainer("/orientdb/bin/console.sh", "/orientdb/CreateDatabase.sql");
		} catch (final UnsupportedOperationException | IOException e) {
			throw new IllegalStateException(e);
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		ORIENT_URL = 
				"remote:" 
				+ ORIENT_DB_CONTAINER.getContainerIpAddress() 
				+ ":" 
				+ ORIENT_DB_CONTAINER.getMappedPort(AbstractTest.EXPOSED_PORT);
		
		/* Use flyway commandline to load the database schema. */
		Main.main(new String[] { "-url=jdbc:orient:" + ORIENT_URL + "/" + TEST_DB_NAME, "-locations=classpath:/db/migration", "-jarDirs=", "-user=" + TEST_DB_USER,
				"-password=" + TEST_DB_PASSWORD, "migrate" });
		
		final Integer mappedOrientStudioPort = ORIENT_DB_CONTAINER.getMappedPort(EXPOSED_ORIENT_STUDIO_PORT);
		if (LOGGER.isInfoEnabled()) LOGGER.info(String.format("Orient studio is available at: http://localhost:%d/studio/index.html", mappedOrientStudioPort));
	}
	
	/**
	 * Loads test data into database. Called before every test.
	 */
	@Before
	public void resetTestdata() {
		try (final OrientDB orientDb = new OrientDB(ORIENT_URL, OrientDBConfig.defaultConfig());
			 final ODatabase<?> db = orientDb.open(TEST_DB_NAME, TEST_DB_USER, TEST_DB_PASSWORD);
			 final Scanner scanner = new Scanner(ClassLoader.getSystemResourceAsStream("test-data.sql"));) {
			while (scanner.hasNextLine()) {
				final String line = scanner.nextLine().trim();
				if (line.isEmpty() || line.startsWith("#")) continue;
				db.execute("sql", line).close();	
			}
		}
	}
	
	/**
	 * Helper function to execute SQL queries.
	 *
	 * @param query Query to execute 
	 * @return OResultSet returned by execution of query on database.
	 */
	protected OResultSet executeSQL(String query) {
		try (final OrientDB orientDb = new OrientDB(ORIENT_URL, OrientDBConfig.defaultConfig());
			 final ODatabase<?> db = orientDb.open(TEST_DB_NAME, TEST_DB_USER, TEST_DB_PASSWORD)) {
			return db.execute("sql", query);
		}
	}
	
	/**
	 * Executes the given SQL query with the given parameters.
	 *
	 * @param query the parameterized SQL query
	 * @param params the parameters for the SQL query
	 * @return the result set, which must be closed
	 */
	protected OResultSet executeSQL(final String query, final Object...params) {
		try (final OrientDB orientDb = new OrientDB(ORIENT_URL, OrientDBConfig.defaultConfig());
			 final ODatabase<?> db = orientDb.open(TEST_DB_NAME, TEST_DB_USER, TEST_DB_PASSWORD)) {
			return db.execute("sql", query, params);
		}
	}
	
	/**
	 * Returns the file content of the file with the given file name in the given project relative folder.
	 * <p>
	 * The charset is 'Cp1252'
	 *
	 * @param folder the project relative folder the file resides in
	 * @param filename the name of the file, for which the content should be retrieved
	 * @return the content of the file
	 */
	protected String getContent(final String folder, final String filename) {
		final Path path = Paths.get(System.getProperty("user.dir"), folder, filename);
		try {
			return IOUtils.toString(Files.newBufferedReader(path, Charset.forName("Cp1252")));
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException(e);
		}
	}
	
}
