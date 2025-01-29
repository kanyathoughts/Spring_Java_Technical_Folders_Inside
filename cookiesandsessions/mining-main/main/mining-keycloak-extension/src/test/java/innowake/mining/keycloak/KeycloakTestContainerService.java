/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.keycloak;

import java.io.IOException;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.Container.ExecResult;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.utility.MountableFile;

import dasniko.testcontainers.keycloak.KeycloakContainer;


/**
 * Provides methods for {@code Keycloak} {@link GenericContainer} lifecycle for unit tests.
 */
public class KeycloakTestContainerService {

	/**
	 * {@code Keycloak} {@link GenericContainer} singleton instance.
	 */
	public static final KeycloakTestContainerService INSTANCE = new KeycloakTestContainerService();

	private static final Logger LOG = LoggerFactory.getLogger(KeycloakTestContainerService.class);
	private static final String KEYCLOAK_IMAGE = "quay.io/keycloak/keycloak:22.0.2";
	
	/**
	 * We need to use the default 8080 keycloak port because the KeycloakContainer has a hardcoded wait strategy which uses the port 8080. If exposing the port 8080 causes
	 * problems we could map the internal port 8080 to any other port with some adjustments in the test setup.
	 */
	private static final int KEYCLOAK_PORT = 8080;

	/* Log message that will be waited for until container is treated as started */
	private static final String LOG_MESSAGE_REGEX = ".*(main) Running the server in development mode*";

	private static final String EXTENSION_CLASSES_FOLDER = "target/classes";
	private static final String KEYCLOAK_DEPLOYMENTS_LOCATION = "/opt/keycloak/providers/";
	private static final String EXTENSION_NAME_IN_CONTAINER = "mining-keycloak-extension.jar";

	private static final MountableFile CREATE_REALM_CREATOR_SCRIPTFILE = MountableFile.forClasspathResource("setup_realms_and_users.sh");
	private static final String CREATE_REALM_CREATOR_SCRIPTFILE_PATH_IN_CONTAINER = "/usr/tmp/setup_realms_and_users.sh";
	/* This path is used because the default user has rights to chmod here */
	private static final String CREATE_REALM_CREATOR_COPIED_PATH = "/usr/tmp/setupscript.sh";

	private GenericContainer<?> keycloakContainer;

	private String username;
	private String password;
	private String clientSecret;
	private String keycloakUrl;

	private boolean instanceAvailable = false;

	private KeycloakTestContainerService() {
		/* disallow instance creation */
	}

	/**
	 * Checks if there is already a locally running Keycloak instance and sets up a new Keycloak test container with the mining Keycloak extension if not. If a
	 * keycloak instance is already running at port {@value #KEYCLOAK_PORT}, the URL of this instance will be returned.
	 * 
	 * @param username username used for the admin user of the keycloak container
	 * @param password password used for the admin user of the keycloak container
	 * @param clientSecret secret used for the realm-creator client secret
	 *
	 * @return URL of Keycloak instance
	 */
	public String ensureContainer(final String username, final String password, final String clientSecret) {
		try {
			this.username = username;
			this.password = password;
			this.clientSecret = clientSecret;
			if ( ! instanceAvailable) {
				if (LOG.isInfoEnabled()) {
					LOG.info("Setting up keycloak instance. Using realm-creator client secret: " + clientSecret);
				}
				try {
					/* This is much faster when a keycloak instance is already running */
					keycloakUrl = setupLocallyRunningKeycloak();
				} catch (final Exception e) {
					if (LOG.isInfoEnabled()) {
						LOG.info("Using Keycloak docker container");
					}
					keycloakUrl = setupContainer();
				}
				instanceAvailable = true;
			}
			return keycloakUrl;
		} catch (final Exception e) {
			if (LOG.isErrorEnabled()) {				
				LOG.error(e.getMessage(), e);
			}
			throw e;
		}
	}

	private String setupLocallyRunningKeycloak() throws IOException {
		/* Check whether its possible to connect to locally running keycloak */
		try (final Socket socket = new Socket("localhost", KEYCLOAK_PORT)) {
			/* Exception occurring if connection not succeeds will be handled appropriately */
		}

		if (LOG.isInfoEnabled()) {
			LOG.info("Using locally available keycloak");
		}

		final String url = "http://localhost:" + KEYCLOAK_PORT;
		if (LOG.isInfoEnabled()) {
			LOG.info(String.format("Keycloak is available at: %s", url));
		}
		return url;
	}

	/* According to https://www.testcontainers.org/test_framework_integration/manual_lifecycle_control/ the Ryuk container will
	 * take care of stopping the singleton container at the end of the test suite */
	@SuppressWarnings("resource")
	private String setupContainer() {
		if (LOG.isInfoEnabled()) {
			LOG.info("setting up a docker container");
		}
		if (keycloakContainer == null) {
			if (Files.notExists(Paths.get(EXTENSION_CLASSES_FOLDER))) {
				throw new IllegalStateException(String.format("keycloak extension classes path \"%s\" doesn't exist", EXTENSION_CLASSES_FOLDER));
			}
			
			final KeycloakContainer container = new KeycloakContainer(KEYCLOAK_IMAGE)
					.withProviderClassesFrom(EXTENSION_CLASSES_FOLDER)
					.withCopyFileToContainer(CREATE_REALM_CREATOR_SCRIPTFILE, CREATE_REALM_CREATOR_SCRIPTFILE_PATH_IN_CONTAINER)
					.withAdminUsername(username)
					.withAdminPassword(password)
					.withEnv("KEYCLOAK_USER", username)
					.withEnv("KEYCLOAK_PASSWORD", password)
					.withEnv("KEYCLOAK_PORT", String.valueOf(KEYCLOAK_PORT))
					.withEnv("CLIENT_SECRET", clientSecret)
					.withLogConsumer(new Slf4jLogConsumer(LOG))
					.withExposedPorts(Integer.valueOf(KEYCLOAK_PORT))
					.waitingFor(Wait.forLogMessage(LOG_MESSAGE_REGEX, 1)
					.withStartupTimeout(Duration.ofMinutes(10)));
			keycloakContainer = container;
		}
		if (!keycloakContainer.isRunning()) {
			keycloakContainer.start();
			try {
				/*
				 * Create deployment marker file so the keycloak extension is loaded. The WildFly deployment scanner only deploys the extension if a dodeploy
				 * file exists. (check out https://docs.jboss.org/author/display/WFLY10/Application%20deployment.html for more information on marker files)
				 */
				executeCommandInContainer("sh", "-c", "touch " + KEYCLOAK_DEPLOYMENTS_LOCATION + EXTENSION_NAME_IN_CONTAINER + ".dodeploy");
				/* chmod and execute the script to create the realm-creator service-client */
				executeCommandInContainer("sh", "-c", "cp " + CREATE_REALM_CREATOR_SCRIPTFILE_PATH_IN_CONTAINER + " " + CREATE_REALM_CREATOR_COPIED_PATH);
				executeCommandInContainer("sh", "-c", "chmod +x " + CREATE_REALM_CREATOR_COPIED_PATH);
				executeCommandInContainer("sh", "-c", CREATE_REALM_CREATOR_COPIED_PATH);
				
				/* Wait until the client is actually created and has the create-realm role assigned to it */
				waitForClientRole();
				LOG.info("realm-creator client created.");
			} catch (final UnsupportedOperationException e) {
				throw new IllegalStateException("Executing create-realm-creator-script in container failed!", e);
			}
		}
		final Integer mappedKeycloakPort = keycloakContainer.getMappedPort(KEYCLOAK_PORT);
		final String url = "http://localhost:" + mappedKeycloakPort;
		if (LOG.isInfoEnabled()) {
			LOG.info(String.format("Keycloak is available at: %s", url));
		}
		return url;
	}

	/**
	 * 
	 * Executes a command in the docker container and logs its results (in debug mode). Logs an error message if command execution failed.
	 *
	 * @param command command to be executed
	 * @return stdout of the command
	 */
	private String executeCommandInContainer(final String... command) {
		final ExecResult result;
		try {
			result = keycloakContainer.execInContainer(command);
		} catch (final UnsupportedOperationException | IOException | InterruptedException e) {
			throw new IllegalStateException("Executing command in container failed!", e);
		}
		final String stdout = result.getStdout();
		if (LOG.isInfoEnabled()) {
			final int exitCode = result.getExitCode();
			LOG.info("Command execution exit code: " + exitCode);
			LOG.info("Command execution container output: " + stdout);
			if (exitCode != 0) {
				LOG.info("Executing command in container caused error! {}", result.toString());
			}
		}
		return stdout;
	}

	/**
	 * Instantiates a anonymous Runnable that executes a command in the Docker container which checks whether the realm-creator client was created and the
	 * create-realm role is assigned to it.
	 */
	private void waitForClientRole() {
		if (LOG.isInfoEnabled()) {
			LOG.info("Waiting for create-realm role to be assigned to realm-creator client...");
		}
		final FutureTask<Void> task = new FutureTask<>(() -> {
			while (true) {
				final String result = executeCommandInContainer("sh", "-c",
						" /opt/keycloak/bin/kcadm.sh get-roles --realm master --uusername service-account-realm-creator | grep create-realm");
				if (result.contains("create-realm")) {
					break;
				}
				try {
					Thread.sleep(200);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					LOG.error("Interrupted while waiting for role to be assigned to realm-creator client.", e);
					throw new IllegalStateException(e);
				}
			}
		}, null);

		Executors.newSingleThreadExecutor().execute(task);
		try {
			task.get(10, TimeUnit.SECONDS);
		} catch (final InterruptedException | ExecutionException | TimeoutException e1) {
			throw new IllegalStateException("Waiting for realm-creator client to be created failed!", e1);
		}
		if (LOG.isInfoEnabled()) {
			LOG.info("Finished wating for create-realm role to be assigned to realm-creator client...");
		}
	}
}
