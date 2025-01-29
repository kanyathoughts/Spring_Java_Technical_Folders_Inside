/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io.discovery.config;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.time.StopWatch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.io.SecuredZipInputStream;
import static innowake.mining.shared.access.ProjectService.PLACEHOLDER_XML_CONFIG_KEY;

/**
 * Service for importing discovery configurations and {@link SearchOrder}s 
 * from a zipped {@link InputStream} into Mining server.
 */
@Service
public class DiscoveryConfigurationImportService {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);
	private static final String SEARCH_ORDER_CONFIG_FILE = "discovery-search-order.xml";
	
	@Autowired
	private ProjectService projectService;

	@Autowired
	private SourceService sourceService;

	/**
	 * Imports Configurations from a zipped {@link InputStream} into the given project ID.
	 *
	 * @param projectId the ID of the project.
	 * @param streamId the identification of the {@link InputStream}.
	 * @param inputStream the zipped {@link InputStream} configurations to import.
	 * @throws Exception In case of any error in the {@link SearchOrder} import.
	 */
	public void importConfigurations(final EntityId projectId, final String streamId, final InputStream inputStream) throws Exception {
		final StopWatch watch = new StopWatch();
		if (LOG.isInfoEnabled()) {
			watch.start();
			LOG.info(String.format("Importing configurations '%s' into project %s", streamId, projectId));
		}
		final Map<String, String> configContent = new HashMap<>();
		try (final ZipInputStream zipIn = new SecuredZipInputStream(inputStream, StandardCharsets.UTF_8)) {
			ZipEntry entry;
			while ((entry = zipIn.getNextEntry()) != null) {
				final ByteArrayOutputStream out = new ByteArrayOutputStream();
				IOUtils.copy(zipIn, out);
				
				final String entryName = FilenameUtils.getName(entry.getName());
				final String content = new String(out.toByteArray(), StandardCharsets.UTF_8);
				configContent.put(entryName, content);
			}
		}
		
		importConfigurations(projectId, configContent);
		
		if (LOG.isInfoEnabled()) {
			watch.stop();
			LOG.info(String.format("Overall import of '%s' took %s (H:mm:ss.SSS)", streamId, watch.toString()));
		}
		
	}

	/**
	 * Imports configurations from the Map into the given project ID.
	 *
	 * @param projectId the ID of the project.
	 * @param contents the map of configurations to import.
	 * @throws Exception In case of any error in the {@link SearchOrder} import.
	 */
	public void importConfigurations(final EntityId projectId, final Map<String, String> contents) throws Exception {
		final List<SearchOrder> searchOrders = new ArrayList<>();
		for (final Entry<String, String> entry : contents.entrySet()) {
			if (entry.getKey().equals(SEARCH_ORDER_CONFIG_FILE)) {
				searchOrders.addAll(SearchOrders.loadConfig(entry.getValue()));
			} else {
				Arrays.stream(ConfigResources.values())
					.map(ConfigResources::getResourceName)
					.filter(entry.getKey()::equalsIgnoreCase)
					.findAny()
					.ifPresent(entryFile -> projectService.putConfig(projectId, entry.getKey(), Collections.singletonMap(PLACEHOLDER_XML_CONFIG_KEY, entry.getValue())));
			}
		}
		final var projectPojo = projectService.get(projectId);
		if ( ! searchOrders.isEmpty() && ! searchOrders.equals(projectPojo.getSearchOrders())) {
			sourceService.removeAllReferences(projectId);
		}
		projectService.update(p -> p.withId(projectId).setSearchOrders(searchOrders));
	}

}
