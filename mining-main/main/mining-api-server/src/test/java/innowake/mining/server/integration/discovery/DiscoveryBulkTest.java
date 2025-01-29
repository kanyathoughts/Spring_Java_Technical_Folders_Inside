/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.tags.DiscoveryTest;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.util.FileSystemUtils;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * Executes BaseDiscoveryTest in bulk by automatically identifying the individual code repositories.
 * The {@code folder} method needs to be overriden.
 */
@WithMockUser
@DiscoveryTest
public abstract class DiscoveryBulkTest extends DatabaseRelatedTest {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryBulkTest.class);

	@Autowired
	private transient GenericConfigProperties genericConfigProperties;

	@Autowired
	protected AutowireCapableBeanFactory beanFactory;

	/**
	 * Override this method to specify the base folder from which individual code base needs to be identified.
	 * For e.g., The directory "iristests" contain all the test cases related to IrisTest.
	 *
	 * @return Base directory.
	 */
	protected abstract String folder();

	/**
	 * If a certain test case that are in the base directory needs to be excluded, include the string here.
	 *
	 * @return Test cases that required to be ignored.
	 */
	protected Set<String> excludeTests() {
		return Collections.emptySet();
	}

	/**
	 * Execute tests that are only present in the collection. No other tests are executed.
	 * During development it is often necessary to only execute a single or a set of tests. Override this method to include only the tests that are required to
	 * run.
	 *
	 * @return Test cases that should only execute.
	 */
	protected List<String> includeThisTestsOnly() {
		return Collections.emptyList();
	}

	protected Set<String> skipDiscoveryFeatureValidation() {
		return Collections.emptySet();
	}

	protected List<String> defaultTests() throws IOException {
		if ( ! includeThisTestsOnly().isEmpty()) {
			return includeThisTestsOnly();
		}
		try (final Stream<Path> walk = Files.walk(BaseDiscoveryTest.SOURCE_FOLDER.resolve(folder()), 1)) {
			return walk.filter(Files::isDirectory)
					.map(Path::toString)
					.map(FilenameUtils::getName)
					.filter(testFolder -> ! testFolder.equals(folder()))
					.filter(testFolder -> ! excludeTests().contains(testFolder))
					.sorted()
					.collect(Collectors.toCollection(LinkedList::new));
		}
	}

	@BeforeAll
	public void before() {
		/* Clean up in case previous test execution stopped before after() got executed */
		handleSourceExport(exportDir -> {
			try {
				FileSystemUtils.deleteRecursively(exportDir);
			} catch (final IOException e) {
				e.printStackTrace();
				fail(e.getMessage());
			}
		});
	}

	@AfterAll
	public void after() {
		/* Test that the created job export directories got deleted */
		handleSourceExport(exportDir -> {
			final CountDownLatch latch = new CountDownLatch(1);
			final ScheduledThreadPoolExecutor scheduler = new ScheduledThreadPoolExecutor(1);
			final AtomicInteger counter = new AtomicInteger(0);
			/* test every 10 s if source export directory is empty */
			scheduler.scheduleAtFixedRate(() -> {
				LOG.info("Waiting for the folder '" + exportDir.toAbsolutePath() + "' to clear : " + (counter.incrementAndGet() * 10) + " seconds");
				try (final DirectoryStream<Path> stream = Files.newDirectoryStream(exportDir)) {
					if ( ! stream.iterator().hasNext()) {
						latch.countDown();
					}
				} catch (final IOException e) {
					fail("Error while getting entries of export directory", e);
				}
			}, 0, 10, TimeUnit.SECONDS);

			try {
				/* wait until background deletion job in SourceExportService must have deleted all exported sources */
				assertTrue(latch.await(60, TimeUnit.SECONDS), "Export directory '" + exportDir + "' must be empty");
				scheduler.shutdown();
			} catch (final InterruptedException e) {
				fail("Error while waiting for source export cleanup", e);
			}
		});
	}

	/**
	 * Dynamically identifies the test case based on the provided base directory.
	 *
	 * @return the identified dynamic tests.
	 */
	@TestFactory
	public Collection<DynamicTest> test() {
		try {
			return defaultTests().stream().map(testFolder -> {
				final BaseDiscoveryTest test = createTestInstance(testFolder, skipDiscoveryFeatureValidation().contains(testFolder));
				beanFactory.autowireBean(test);
				return dynamicTest(testFolder, test);
			}).collect(Collectors.toList());
		} catch(final IOException e) {
			fail("Fail to identify the dynamic tests. Exception occured : " + e.getMessage());
			return Collections.emptyList();
		}
	}


	protected BaseDiscoveryTest createTestInstance(final String testFolder, final boolean skipDiscoveryFeatureValidation) {
		return new BaseDiscoveryTest(skipDiscoveryFeatureValidation) {

			@Override
			protected String getTestFolder() {
				return Paths.get(folder()).resolve(testFolder).toString();
			}
		};
	}

	private void handleSourceExport(final Consumer<Path> operation) {
		final Path exportDir = Paths.get(genericConfigProperties.getTemporaryFolder());
		if (Files.isDirectory(exportDir)) {
			operation.accept(exportDir);
		}
	}
}
