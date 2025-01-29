/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static innowake.mining.client.MiningApiClient.moduleService;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.io.IoServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.shared.io.ExportFormatDescription;

/**
 * Tests for the IO REST services.
 */
class IOServiceTest extends IntegrationTest {

	private final static Long ONE = Long.valueOf(1);
	private final IoServiceProvider ioServiceProvider = MiningApiClient.ioService(getConnectionInfo());
	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	
	private static final String EXPORT_EXTENSION = "app-config-txt";
	
	/**
	 * Remove all modules, import the test-excel and check if we get the correct number of modules after the import.
	 * The test-excel contains dependency-target with ambiguous names but different types/targets. Without the fix
	 * for WMIN-799 this number will be (erroneously) 6.
	 */
	@Test
	void testExcelImport() {
		try (final InputStream in = Files.newInputStream(Paths.get("./src/test/resources/discovery_wmin-769.xlsx"))) {
			moduleServiceProvider.deleteAllModules().setProjectId(ONE).execute();
			ioServiceProvider.importExcel().setInputStream(in).setInputStreamId("WMIN-769").setProjectId(ONE).execute();
			final int moduleCount = moduleService(getConnectionInfo())
					.findAllModules()
					.setProjectId(ONE)
					.execute()
					.getValue()
					.get().length;
			assertEquals(7, moduleCount);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	/**
	 * Validates Application Configuration file export functionality is working as expected.
	 */
	@Test
	void testExportToFormatWithExtensionToggleEnabled() {		
		try {			
			final Result<Tuple2<String, byte[]>> result = ioServiceProvider.exportToFormat()
					.setProjectId(ONE)
					.setExportFormat(EXPORT_EXTENSION)
					.execute();
			assertEquals(200, result.getStatusCode());
			assertTrue(result.getValue().isPresent());
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	/**
	 * Validates get export format service is working as expected.
	 */
	@Test
	void testGetExportFormatsWithExtensionToggleDisabled() {		
		try {		
			final Result<List<ExportFormatDescription>> result= ioServiceProvider.getExportFormats().setProjectId(ONE).execute();
			assertTrue(result.getValue().isPresent());
			assertEquals(200, result.getStatusCode());
			final List<ExportFormatDescription> exportFormatDescriptions = result.getValue().get();
			assertNotNull(exportFormatDescriptions);
			assertTrue(exportFormatDescriptions.stream()
					.anyMatch(exportFormat -> exportFormat.id.equals(EXPORT_EXTENSION)));
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
}
