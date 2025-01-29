/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.discovery.DiscoveryServiceProvider;
import innowake.mining.shared.discovery.config.ConfigResources;

/**
 * Integration unit tests for the Discovery service.
 */
class DiscoveryServiceTest extends IntegrationTest {
	private final DiscoveryServiceProvider discoveryServiceProvider = MiningApiClient.discoveryService(getConnectionInfo());
	private static final String SEARCH_ORDER = "discovery-search-order.xml";
	
	private static final Long ONE = Long.valueOf(1);
	
	@Test
	void testDownloadConfiguration() throws IOException {
		final Result<byte[]> byteArr = discoveryServiceProvider.downloadConfiguration().setProjectId(ONE).execute();
		assertNotNull(byteArr);

		ZipInputStream zis = getZipInputStream(byteArr);
		assertNotNull(zis);
		ZipEntry zipEntry = zis.getNextEntry();
		final Set<String> zipEntries = new HashSet<>();
		while (zipEntry != null) {
			final String zipEntryName = zipEntry.getName();
			zipEntries.add(zipEntryName);
			zipEntry = zis.getNextEntry();
		}
		final List<String> configurations = Arrays.stream(ConfigResources.values())
												.map(ConfigResources::getResourceName)
												.collect(Collectors.toList());
		configurations.add(SEARCH_ORDER);
		assertTrue(configurations.containsAll(zipEntries));
	}

	@Nullable 
	private ZipInputStream getZipInputStream(final Result<byte[]> byteArr) {
		final Optional<byte[]> optionalByteArr = byteArr.getValue();
		if (optionalByteArr.isPresent()) {
			final InputStream targetStream = new ByteArrayInputStream(optionalByteArr.get());
			return new ZipInputStream(targetStream);
		}
		return null;
	}
}
