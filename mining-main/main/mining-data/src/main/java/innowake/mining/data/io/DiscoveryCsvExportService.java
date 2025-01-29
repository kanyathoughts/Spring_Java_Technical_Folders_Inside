/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import org.apache.commons.lang.time.StopWatch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Service for exporting a Discovery CSV file as byte array from Mining server.
 */
@Service
public class DiscoveryCsvExportService {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

	@Autowired
	private ProjectService projectService;
	@Autowired
	private CsvModulesGenerator modulesGenerator;
	@Autowired
	private CsvStatementsGenerator statementsGenerator;
	@Autowired
	private CsvSqlGenerator sqlGenerator;
	@Autowired
	private CsvDependenciesGenerator dependenciesGenerator;
	@Autowired
	private CsvErrorsGenerator errorsGenerator;
	@Autowired
	private CsvUndiscoveredGenerator undiscoveredGenerator;
	@Autowired
	private CsvDeadCodeGenerator deadCodeGenerator;
	@Autowired
	private CsvPricingSummaryGenerator pricingSummaryGenerator;
	@Autowired
	private CsvTypeSummaryGenerator typeSummaryGenerator;
	@Autowired
	private GenericConfiguration genericConfiguration;
	private static final String LOG_COUNT_STATISTICS = "Exporting {} overall {}";

	/**
	 * Exports a Discovery CSV file for the given project ID.
	 *
	 * @param projectId the ID of the project
	 * @param out the output stream where the CSV is written to
	 * @param options additional options for customizing the export
	 * @throws IOException in case of an error while creating the data
	 */
	public void exportCsv(final EntityId projectId, final OutputStream out, final DiscoveryExportOptions options) throws IOException {
		final var watch = new StopWatch();
		watch.start();
		LOG.info("Exporting project {} into discovery CSV", projectId);

		final Config config;
		try {
			config = Config.loadConfig(projectService, projectId);
		} catch (final DiscoveryException e) {
			throw new IOException(e);
		}
		final var csvGenerator = new CsvGenerator(out);
		final var discoveryExportSortThreshold = genericConfiguration.getDiscoveryExportSortThreshold();

		final var moduleCount = modulesGenerator.getModuleCount(projectId);
		LOG.info(LOG_COUNT_STATISTICS, "Modules", Long.valueOf(moduleCount));
		final var modulesMapping = modulesGenerator.createCsv(projectId, options, csvGenerator, moduleCount <= discoveryExportSortThreshold);

		if (config.isCollectStatements()) {
			final long statementsCount = statementsGenerator.getStatementCount(projectId);
			LOG.info(LOG_COUNT_STATISTICS, "Statements", Long.valueOf(statementsCount));
			statementsGenerator.createCsv(projectId, modulesMapping, options, csvGenerator, statementsCount <= discoveryExportSortThreshold);
		}

		final var sqlCount = sqlGenerator.getSQLCount(projectId);
		LOG.info(LOG_COUNT_STATISTICS, "SQL Statements", Long.valueOf(sqlCount));
		sqlGenerator.createCsv(projectId, modulesMapping, options, csvGenerator, sqlCount <= discoveryExportSortThreshold);

		final var dependenciesCount = dependenciesGenerator.getDependenciesCount(projectId);
		LOG.info(LOG_COUNT_STATISTICS, "Dependencies", Long.valueOf(dependenciesCount));
		dependenciesGenerator.createCsv(projectId, modulesMapping, options, csvGenerator, dependenciesCount <= discoveryExportSortThreshold);

		final var errorsCount = errorsGenerator.getErrorsCount(projectId);
		LOG.info(LOG_COUNT_STATISTICS, "Errors", Long.valueOf(errorsCount));
		errorsGenerator.createCsv(projectId, modulesMapping, options, csvGenerator, errorsCount <= discoveryExportSortThreshold);

		final var undiscoveredCount = undiscoveredGenerator.getUndiscoveredCount(projectId).longValue();
		LOG.info(LOG_COUNT_STATISTICS, "Undiscovered modules", Long.valueOf(undiscoveredCount));
		undiscoveredGenerator.createCsv(projectId, modulesMapping, options, csvGenerator, undiscoveredCount <= discoveryExportSortThreshold);

		final var deadCodeCount = deadCodeGenerator.getDeadCodeCount(projectId);
		LOG.info(LOG_COUNT_STATISTICS, "Deadcode", Long.valueOf(deadCodeCount));
		deadCodeGenerator.createCsv(projectId, modulesMapping, options, csvGenerator, deadCodeCount <= discoveryExportSortThreshold);

		watch.stop();
		LOG.info("Overall export of project {} took {} (H:mm:ss.SSS)", projectId, watch.toString());
	}

	/**
	 * Exports a Effort Summary CSV file for the given project ID.
	 *
	 * @param projectId the ID of the project
	 * @param out the output stream where the CSV is written to
	 * @throws IOException in case of an error while creating the data
	 */
	public void exportEffortSummaryCsv(final EntityId projectId, final OutputStream out) throws IOException {
		final var watch = new StopWatch();
		watch.start();
		LOG.info("Exporting project {} into effort summary CSV", projectId);

		final var csvGenerator = new CsvGenerator(out);
		typeSummaryGenerator.createCsv(projectId, csvGenerator);
		pricingSummaryGenerator.createCsv(projectId, csvGenerator);

		watch.stop();
		LOG.info("Overall export of effort summary CSV for project {} took {} (H:mm:ss.SSS)", projectId, watch.toString());
	}
}