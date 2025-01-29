/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;

import org.apache.commons.lang.time.StopWatch;
import org.apache.commons.lang3.StringUtils;

import com.opencsv.CSVReader;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Importer for Discover Metrics in CSV format.
 */
public class CsvImporter {

	private static final String CATEGORY_COMMENT_PREFIX = "#";
	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

	private final EntityId projectId;
	private final ModuleService moduleService;
	private final ModuleParameters moduleParameters;

	public CsvImporter(final EntityId projectId, final ModuleService moduleService, final ModuleParameters moduleParameters) {
		this.projectId = projectId;
		this.moduleParameters = moduleParameters;
		this.moduleService = moduleService;
	}

	/**
	 * Imports the discovery CSV file from the given {@code reader}.
	 *
	 * @param fileId id of the file that is imported
	 * @param reader the reader for reading from the file
	 * @throws IOException if reading or processing of CSV rows failed
	 */
	public void importCsv(final String fileId, final Reader reader) throws IOException {
		final StopWatch importWatch = new StopWatch();
		importWatch.start();

		final RowCallbackResolver rowCallbackResolver = new RowCallbackResolver(projectId, fileId, moduleService, moduleParameters);

		String currentCategory = "";
		int currentRow = 0;
		try (final CSVReader csvReader = new CSVReader(reader)) {
			String[] row;
			RowCallback rowCallback = null;
			while ((row = csvReader.readNext()) != null) {
				if (row.length == 1) {
					final String nextCategory = getNextCategory(row[0].trim(), currentCategory);
					if (StringUtils.isNotEmpty(nextCategory)) {
						if (rowCallback != null) {
							rowCallbackResolver.done(currentCategory);
							rowCallback.sheetComplete();
						}

						importWatch.stop();
						if (LOG.isDebugEnabled()) LOG.debug(String.format("Importing %s sheet took %s (H:mm:ss.SSS); next sheet: %s", currentCategory, importWatch.toString(), nextCategory));
						importWatch.reset();
						importWatch.start();

						rowCallback = rowCallbackResolver.resolve(nextCategory);
						currentCategory = nextCategory;
						currentRow = 0;

						setRowHeaders(csvReader, rowCallback, currentCategory);
						continue;
					}
				}

				if (StringUtils.isNoneBlank(currentCategory)) {
					assertNotNull(rowCallback).rowComplete(currentRow, row);
					currentRow++;
				} else {
					if (LOG.isWarnEnabled()) LOG.warn(String.format("Ignoring line from unknown sheet '%s': %s", currentCategory, Arrays.toString(row)));
				}
			}
		} catch (final IllegalArgumentException e) {
			throw new IOException(e);
		}

		/* post-import */
		moduleService.updateSourceMetricsLinesOfDeadCode(projectId);
		importWatch.stop();
		if (LOG.isDebugEnabled()) LOG.debug(String.format("Importing %s sheet took %s (H:mm:ss.SSS);", currentCategory, importWatch.toString()));
	}

	private static void setRowHeaders(final CSVReader csvReader, final RowCallback rowCallback, final String category) throws IOException {
		/* Set the column headers of the next sheet if available */
		final String[] headers = csvReader.readNext();
		if (headers == null) {
			throw new IOException(String.format("Category: %s must be followed immediately by the row headers without empty lines. ", category));
		}

		rowCallback.setHeaders(headers);
	}

	private static String getNextCategory(final String categoryComment, final String currentCategory) throws IOException {
		if (categoryComment.startsWith(CATEGORY_COMMENT_PREFIX)) {
			final String nextCategory = categoryComment.substring(1).trim();
			if (currentCategory.equals("") && ! nextCategory.equals(WorkbookDefinition.SHEET_MODULES)) {
				/* importModules() returns the modules-to-uid mapping, which is required to interpret the subsequent sheets */
				throw new IOException("File must contain Modules sheet as first entry");
			}
			return nextCategory;
		}

		return "";
	}

}
