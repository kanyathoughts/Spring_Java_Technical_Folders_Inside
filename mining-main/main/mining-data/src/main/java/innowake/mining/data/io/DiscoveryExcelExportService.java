/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
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
import org.apache.poi.util.DefaultTempFileCreationStrategy;
import org.apache.poi.util.TempFile;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.OutputStream;

import static innowake.mining.shared.io.ExcelProperties.DISCOVERY_VERSION;

/**
 * Service for exporting a Discovery Excel workbook as byte array from Mining server.
 */
@Service
public class DiscoveryExcelExportService {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

	@Autowired
	private ProjectService projectService;
	@Autowired
	private ExcelModulesGenerator modulesGenerator;
	@Autowired
	private ExcelStatementsGenerator statementsGenerator;
	@Autowired
	private ExcelSqlGenerator sqlGenerator;
	@Autowired
	private ExcelDependenciesGenerator dependenciesGenerator;
	@Autowired
	private ExcelErrorsGenerator errorsGenerator;
	@Autowired
	private ExcelUndiscoveredGenerator undiscoveredGenerator;
	@Autowired
	private ExcelDeadCodeGenerator deadCodeGenerator;
	@Autowired
	private ExcelTypeSummaryGenerator typeSummaryGenerator;
	@Autowired
	private ExcelPricingSummaryGenerator pricingSummaryGenerator;
	@Autowired
	private GenericConfiguration genericConfiguration;
	private static final String LOG_COUNT_STATISTICS = "Exporting {} overall {}";
	
	/**
	 * Exports a Discovery Excel workbook from the given project ID.
	 *
	 * @param projectId the ID of the project
	 * @param out the output stream where the Excel workbook is written to
	 * @param options additional options for customizing the export
	 * @throws IOException in case of an error while creating the data
	 */
	public void exportExcel(final EntityId projectId, final OutputStream out, final DiscoveryExportOptions options) throws IOException {
		final var watch = new StopWatch();
		watch.start();
		LOG.info("Exporting project {} into Excel", projectId);

		final Config config;
		try {
			config = Config.loadConfig(projectService, projectId);
		} catch (final DiscoveryException e) {
			throw new IOException(e);
		}

		/* change temporary file directory by implementing custom TempFileCreationStrategy */
		TempFile.setTempFileCreationStrategy(new DefaultTempFileCreationStrategy());

		try (final var workbook = new SXSSFWorkbook()) {
			try {
				/* set properties */
				final var properties = workbook.getXSSFWorkbook().getProperties();
				properties.getCustomProperties().addProperty(DISCOVERY_VERSION.name(), DISCOVERY_VERSION.getValue());

				final var discoveryExportSortThreshold = genericConfiguration.getDiscoveryExportSortThreshold();

				final var moduleCount = modulesGenerator.getModuleCount(projectId);
				LOG.info(LOG_COUNT_STATISTICS, "Modules", Long.valueOf(moduleCount));
				final var moduleMapping = modulesGenerator.createSheet(workbook, projectId, options,  moduleCount <= discoveryExportSortThreshold);
				
				if (config.isCollectStatements()) {
					final var statementsCount = statementsGenerator.getStatementCount(projectId);
					LOG.info(LOG_COUNT_STATISTICS, "Statements", Long.valueOf(statementsCount));
					statementsGenerator.createSheet(workbook, projectId, moduleMapping, options, statementsCount <= discoveryExportSortThreshold);
				}
				
				final var sqlCount = sqlGenerator.getSQLCount(projectId);
				LOG.info(LOG_COUNT_STATISTICS, "SQL Statements", Long.valueOf(sqlCount));
				sqlGenerator.createSheet(workbook, projectId, moduleMapping, options, sqlCount <= discoveryExportSortThreshold);
				
				final var dependenciesCount = dependenciesGenerator.getDependenciesCount(projectId);
				LOG.info(LOG_COUNT_STATISTICS, "Dependencies", Long.valueOf(dependenciesCount));
				dependenciesGenerator.createSheet(workbook, projectId, moduleMapping, options, dependenciesCount <= discoveryExportSortThreshold);				
				
				final var errorsCount = errorsGenerator.getErrorsCount(projectId);
				LOG.info(LOG_COUNT_STATISTICS, "Errors", Long.valueOf(errorsCount));
				errorsGenerator.createSheet(workbook, projectId, moduleMapping, options, errorsCount <= discoveryExportSortThreshold);
				
				final var undiscoveredCount = undiscoveredGenerator.getUndiscoveredCount(projectId).longValue();
				LOG.info(LOG_COUNT_STATISTICS, "Undiscovered modules", Long.valueOf(undiscoveredCount));
				undiscoveredGenerator.createSheet(workbook, projectId, moduleMapping, options, undiscoveredCount <= discoveryExportSortThreshold);
				
				final var deadCodeCount = deadCodeGenerator.getDeadCodeCount(projectId);
				LOG.info(LOG_COUNT_STATISTICS, "Deadcode", Long.valueOf(deadCodeCount));
				deadCodeGenerator.createSheet(workbook, projectId, moduleMapping, options, deadCodeCount <= discoveryExportSortThreshold);

				workbook.write(out);
			} finally {
				workbook.dispose();
			}
		}

		watch.stop();
		LOG.info("Overall export of project {} took {} (H:mm:ss.SSS)", projectId, watch.toString());
	}

	/**
	 * Exports a Effort Summary Excel workbook for the given project ID.
	 *
	 * @param projectId The given Project id.
	 * @param out the output stream where the Excel workbook is written
	 */
	public void exportEffortSummaryExcel(final EntityId projectId, final OutputStream out) {
		final var watch = new StopWatch();
		watch.start();
		LOG.info("Exporting project {} into Excel", projectId);

		/* change temporary file directory by implementing custom TempFileCreationStrategy */
		TempFile.setTempFileCreationStrategy(new DefaultTempFileCreationStrategy());

		try (final var workbook = new SXSSFWorkbook()) {
			try {
				/* set properties */
				final var properties = workbook.getXSSFWorkbook().getProperties();
				properties.getCustomProperties().addProperty(DISCOVERY_VERSION.name(), DISCOVERY_VERSION.getValue());

				typeSummaryGenerator.createSheet(workbook, projectId);
				pricingSummaryGenerator.createSheet(workbook, projectId);

				workbook.write(out);
			} finally {
				workbook.dispose();
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}

		watch.stop();
		LOG.info("Overall export of project {} took {} (H:mm:ss.SSS)", projectId, watch.toString());
	}

}