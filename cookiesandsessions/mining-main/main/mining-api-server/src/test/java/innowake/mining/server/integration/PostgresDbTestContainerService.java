/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration;

import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.util.Map;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.DockerClientFactory;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.yaml.snakeyaml.Yaml;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.lang.NestedMap;

public class PostgresDbTestContainerService {

	private static final String CONTAINER_IMAGE = "postgres:14.5";

	private static final int EXPOSED_DB_PORT = Integer.getInteger("mining.test.pg.db.port", 5432).intValue();

	/* For docker: Memory limit in bytes. (https://docs.docker.com/config/containers/resource_constraints/) */
	private static final Long CONTAINER_MEMORY_BYTES = Long.getLong("POSTGRES_CONTAINER_MEMORY_BYTES", 8L * 1024 * 1024 * 1024);
	/* For docker: Total memory limit (memory + swap); -1=unlimited swap; memory-swap==memory: Prevents containers from using any swap. */
	private static final Long CONTAINER_MEMORY_SWAP_BYTES = Long.getLong("POSTGRES_CONTAINER_MEMORY_SWAP_BYTES", 8L * 1024 * 1024 * 1024);

	private static final Boolean FORCE_CONTAINER = Boolean.getBoolean("POSTGRES_FORCE_CONTAINER");

	private static final String SPRING_PROPERTY_JDBC_URL = "postgres.datasource.jdbc-url";

	private static final Logger LOG = LoggerFactory.getLogger(PostgresDbTestContainerService.class);

	@Nullable
	private static Optional<PostgreSQLContainer<?>> postgreSQLContainer;
	
	private static URI loadConnectionString() {
		final var conf = new NestedMap(new Yaml().<Map<String, Object>>load(
				PostgresDbTestContainerService.class.getClassLoader().getResourceAsStream("application.yml")));
		final var jdbcURL = conf.getSub("postgres").getSub("datasource").<String>getValue("jdbc-url");
		return URI.create(jdbcURL.startsWith("jdbc:") ? jdbcURL.substring(5) : jdbcURL);
	}
	
	@SuppressWarnings("resource")
	public static Optional<PostgreSQLContainer<?>> ensureContainer() {
		final var dbAddr = loadConnectionString();
		
		Optional<PostgreSQLContainer<?>> localPostgreSQLContainer = postgreSQLContainer;
		
		if (localPostgreSQLContainer == null) {
			if ( ! FORCE_CONTAINER && isLocalServerRunning()) {
				LOG.info("Portgres server detected at " + dbAddr.getHost() + ":" + dbAddr.getPort() + ", not starting container");
				localPostgreSQLContainer = Optional.empty();
			} else {
				LOG.info("Preparing to start Postgres container");
				localPostgreSQLContainer = Optional.of(new PostgreSQLContainer<>(CONTAINER_IMAGE)
						.withDatabaseName("mining")
						.withUsername("mining")
						.withPassword("Worx2000")
						.withExposedPorts(Integer.valueOf(EXPOSED_DB_PORT))
						.withCommand("postgres", "-c", "max_connections=200")
						.withLogConsumer(new Slf4jLogConsumer(LOG))
						.withCreateContainerCmdModifier(cmd -> {
							cmd.getHostConfig()
									/* Memory limit in bytes. (https://docs.docker.com/config/containers/resource_constraints/) */
									.withMemory(CONTAINER_MEMORY_BYTES)
									/* Total memory limit (memory + swap); -1=unlimited swap; memory-swap==memory: Prevents containers from using any swap. */
									.withMemorySwap(CONTAINER_MEMORY_SWAP_BYTES);
						}));
			}
		}
		
		localPostgreSQLContainer.ifPresent(container -> {
			if ( ! container.isRunning()) {
				container.start();

				final Integer mappedDbPort = container.getMappedPort(EXPOSED_DB_PORT);
				final String dockerHostIpAddress = DockerClientFactory.instance().dockerHostIpAddress();
				if (LOG.isInfoEnabled()) {
					LOG.info(String.format("Postgres database is available at: http://%s:%d", dockerHostIpAddress, mappedDbPort));
				}
				System.setProperty(SPRING_PROPERTY_JDBC_URL, "jdbc:postgresql://" + dockerHostIpAddress + ":" + mappedDbPort + "/mining");
			}
		});
		
		postgreSQLContainer = localPostgreSQLContainer;
		return localPostgreSQLContainer;
	}
	
	private static boolean isLocalServerRunning() {
		try {
			final Socket socket = new Socket();
			socket.connect(new InetSocketAddress("localhost", EXPOSED_DB_PORT), 100);
			socket.close();
		} catch (final Exception e) {
			return false;
		}
		return true;
	}

}
