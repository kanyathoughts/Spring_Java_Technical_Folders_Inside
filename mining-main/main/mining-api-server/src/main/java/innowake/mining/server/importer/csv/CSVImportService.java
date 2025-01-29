/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.importer.csv;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.Instant;

import org.apache.commons.lang.time.StopWatch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.io.CsvImporter;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Service for importing a Discovery CSV {@link InputStream} into Mining server.
 */
@Service
public class CSVImportService {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ProjectService projectService;

	/**
	 * Imports a Discovery CSV {@link InputStream} into the given project ID.
	 *
	 * @param projectId the ID of the project
	 * @param fileId the identification of the CSV file
	 * @param inputStream the input stream of the CSV file
	 * @throws IOException in case of an error while accessing or parsing the {@link InputStream}
	 */
	public void importCsv(final EntityId projectId, final String fileId, final InputStream inputStream) throws IOException {
		final StopWatch totalWatch = new StopWatch();
		totalWatch.start();
		LOG.info(() -> String.format("Importing Discovery CSV into project %s", projectId.toString()));
		
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
			/* Update the metricsDate on Project before beginning the import */
			final Instant metricsDate = Instant.now();
			projectService.update(p -> p.withId(projectId).setMetricsDate(metricsDate));
			new CsvImporter(projectId, moduleService, new ModuleParameters(metricsDate)).importCsv(fileId, reader);
		}
		
		totalWatch.stop();
		LOG.info(() -> String.format("Overall import took %s (H:mm:ss.SSS)", totalWatch.toString()));
	}
}