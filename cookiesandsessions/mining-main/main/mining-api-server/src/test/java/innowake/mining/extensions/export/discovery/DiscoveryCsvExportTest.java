/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.discovery;

import innowake.mining.data.io.CsvExportHelper;
import innowake.mining.data.io.DiscoveryCsvExportService;
import innowake.mining.data.io.DiscoveryExportOptions;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.integration.discovery.DiscoveryBulkTest;
import innowake.mining.shared.access.EntityId;
import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * This tests the CSV export.
 */
public class DiscoveryCsvExportTest extends DiscoveryBulkTest {

	private static final String DUMP_FILE = "discovery.csv.dump";

	@Override
	protected String folder() {
		return "csv-export";
	}

	@Override
	@TestFactory
	public Collection<DynamicTest> test() {
		try {
			return defaultTests().stream().map(testFolder -> {
				final BaseDiscoveryTest test = new BaseDiscoveryTest() {

					@Autowired
					private DiscoveryCsvExportService csvExportService;

					/**
					 * This method calls and performs:
					 * 1.) Creates a new Project for each run.
					 * 2.) Uploads Source code & Configurations.
					 * 3.) Perform Discover Code
					 * 4.) Perform Discover Metrics
					 * 5.) Exports the snapshot into CSV.
					 * 6.) Asserts with the expected file.
					 */
					@Override
					public void execute() throws EncryptedDocumentException, InvalidFormatException, IOException {
						sourceService.resetCaches();
						final Path expectedFile = getExpectedFile(getExpectedFileName());
						final EntityId projectId = createProject().identity();
						uploadResources(projectId);
						submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
						submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));

						final String actualCsv;
						try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
							csvExportService.exportCsv(projectId, out, new DiscoveryExportOptions());
							actualCsv = out.toString(getCharset());
						}
						if (isWriteExpected()) {
							writeExpected(expectedFile, actualCsv);
							return;
						}
						DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), actualCsv);
					}

					@Override
					protected String getTestFolder() {
						return Paths.get(folder()).resolve(testFolder).toString();
					}

					@Override
					protected Charset getCharset() {
						return CsvExportHelper.WORKSPACE_CHARSET;
					}

					@Override
					protected String getExpectedFileName() {
						return DUMP_FILE;
					}
				};
				beanFactory.autowireBean(test);
				return dynamicTest(testFolder, test);
			}).collect(Collectors.toList());
		} catch (final IOException e) {
			fail("Fail to identify the dynamic tests. Exception occured : " + e.getMessage());
			return Collections.emptyList();
		}
	}
}
