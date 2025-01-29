/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.commons.lang.time.StopWatch;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.eventusermodel.ReadOnlySharedStringsTable;
import org.apache.poi.xssf.eventusermodel.XSSFReader;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler.SheetContentsHandler;
import org.apache.poi.xssf.usermodel.XSSFComment;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Service for importing a Discovery Excel {@link InputStream} into Mining server.
 */
@Service
public class ExcelImportService {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);
	
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ProjectService projectService;
	
	/**
	 * Imports a Discovery Excel {@link InputStream} into the given project ID.
	 *
	 * @param projectId the ID of the project
	 * @param fileId the identification of the Excel file
	 * @param inputStream the input stream of the Excel file
	 * @throws IOException in case of an error while accessing or parsing the {@link InputStream}
	 */
	public void importExcel(final EntityId projectId, final String fileId, final InputStream inputStream) throws IOException {
		final var watch = new StopWatch();
		watch.start();
		LOG.info("Importing Excel '{}' into project {}", fileId, projectId);
		
		/* Update the metricsDate on Project before beginning the import */
		final var metricsDate = Instant.now();
		projectService.update(p -> p.withId(projectId).setMetricsDate(metricsDate));
		
		final var moduleParameters = new ModuleParameters(metricsDate);
		final var rowCallbackResolver = new RowCallbackResolver(projectId, fileId, moduleService, moduleParameters);
		
		try (final var opcPackage = OPCPackage.open(inputStream)) {
			final var sharedStringsTable = new ReadOnlySharedStringsTable(opcPackage);
			final var xssfReader = new XSSFReader(opcPackage);
			final var stylesTable = xssfReader.getStylesTable();
			final var sheetIterator = (XSSFReader.SheetIterator) xssfReader.getSheetsData();
			
			while (sheetIterator.hasNext()) {
				try (final InputStream stream = sheetIterator.next()) {
					final var sheetWatch = new StopWatch();
					sheetWatch.start();
					final var sheetNameWithoutIndex = StringUtils.substringBefore(sheetIterator.getSheetName(), SheetGenerator.SHEET_NAME_INDEX_SEPARATOR);
					LOG.info(() -> String.format("Importing sheet '%s' of '%s'", sheetIterator.getSheetName(), fileId));
					final var formatter = new DataFormatter();
					final var rowCallback = rowCallbackResolver.resolve(sheetNameWithoutIndex);
					final var contentsHandler = new GenericSheetContentsHandler(rowCallback);
					final SAXParserFactory parserFactory = SAXParserFactory.newDefaultInstance();
					parserFactory.setNamespaceAware(true);
					final SAXParser parser = parserFactory.newSAXParser();
					final XMLReader sheetParser = parser.getXMLReader();
					sheetParser.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
					sheetParser.setContentHandler(new XSSFSheetXMLHandler(stylesTable, null, sharedStringsTable, contentsHandler, formatter, false));
					sheetParser.parse(new InputSource(stream));
		
					rowCallbackResolver.done(sheetNameWithoutIndex);
					sheetWatch.stop();
					LOG.info(() -> String.format("Importing sheet '%s' of '%s' took %s (H:mm:ss.SSS)", sheetIterator.getSheetName(), fileId, sheetWatch.toString()));
				}
			}
		} catch (final Exception e) {
			throw new IOException(e);
		}
		/* post-import process */
		moduleService.updateSourceMetricsLinesOfDeadCode(projectId);
		watch.stop();
		LOG.info(() -> String.format("Overall import of '%s' took %s (H:mm:ss.SSS)", fileId, watch.toString()));
	}

	/*
	 * Handles missing columns as done in https://svn.apache.org/repos/asf/poi/trunk/src/examples/src/org/apache/poi/xssf/eventusermodel/XLSX2CSV.java
	 */
	private class GenericSheetContentsHandler implements SheetContentsHandler {

		private final RowCallback rowCallback;
		private final List<String> rowValues = new ArrayList<>();
		
		private int currentRow = 0;
		private int currentColumn = -1;

		private GenericSheetContentsHandler(final RowCallback rowCallback) {
			this.rowCallback = rowCallback;
		}

		@Override
		public void startRow(final int rowNum) {
			rowValues.clear();
			currentColumn = -1;
		}

		@Override
		public void endRow(final int rowNum) {
			if ( ! rowValues.isEmpty()) {
				if (currentRow == 0) {
					rowCallback.setHeaders(rowValues.toArray(new String[0]));
				} else {
					rowCallback.rowComplete(currentRow, rowValues.toArray(new String[0]));
				}
			}
			currentRow++;
		}

		@Override
		public void cell(@Nullable final String cellReference, @Nullable final String formattedValue, @Nullable final XSSFComment comment) {
			/* gracefully handle missing cellReferences here in a similar way as XSSFCell does */
			final String actualCellReference = cellReference != null ? cellReference : new CellAddress(currentRow, currentColumn).formatAsString();
			
			/* check for missing cells */
			final var actualColumn = new CellReference(actualCellReference).getCol();
			final var missedColumns = actualColumn - currentColumn - 1;
			for (var i = 0; i < missedColumns; i++) {
				rowValues.add(null);
			}
			currentColumn = actualColumn;
			
			rowValues.add(formattedValue);
		}
		
		@Override
		public void endSheet() {
			rowCallback.sheetComplete();
		}
	}
}