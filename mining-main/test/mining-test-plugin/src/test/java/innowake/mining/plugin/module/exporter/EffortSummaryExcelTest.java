/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.exporter;

import static innowake.mining.shared.io.ExcelProperties.DISCOVERY_VERSION;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.eventusermodel.ReadOnlySharedStringsTable;
import org.apache.poi.xssf.eventusermodel.XSSFReader;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler.SheetContentsHandler;
import org.apache.poi.xssf.extractor.XSSFEventBasedExcelExtractor;
import org.apache.poi.xssf.model.StylesTable;
import org.apache.poi.xssf.usermodel.XSSFComment;
import org.apache.xmlbeans.XmlException;
import org.junit.Ignore;
import org.junit.Test;
import org.openxmlformats.schemas.officeDocument.x2006.customProperties.CTProperty;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.IntegrationBaseTest;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.shared.io.ExcelProperties;

/**
 * Tests {@link EffortSummaryExcelTest} regarding the successful export of Excel files.
 */
public class EffortSummaryExcelTest extends IntegrationBaseTest {

	private static final Long PROJECT_ID = Long.valueOf(1);
	
	/**
	 * Imports and exports all modules from
	 * {@code src/test/resources/innowake/mining/plugin/module/exporter/effort-summary_1_2020-10-19_14-23-27.xlsx}
	 * and compares original with exported Excel file.
	 */	
	@Test
	@Ignore
	public void testAllSheets() {
		doTest("src/test/resources/innowake/mining/plugin/module/exporter/effort-summary_1_2020-10-19_14-23-27.xlsx");
	}

	private void doTest(final String path) {
		try {
			final byte[] bytesExported = exportServerside();
			final Workbook workbookOriginal = parseExcel(path);
			final Workbook workbookExported = parseExcel(bytesExported);
			assertWorkbook(workbookOriginal, workbookExported);
		} catch (final ValidationException | IOException | SAXException | OpenXML4JException | XmlException | ParserConfigurationException e) {
			throw new IllegalStateException(e);
		}
	}

	private void assertWorkbook(final Workbook expected, final Workbook actual) {
		assertTrue(actual.properties.containsKey(DISCOVERY_VERSION));
		assertEquals(DISCOVERY_VERSION.getValue(), actual.properties.get(DISCOVERY_VERSION));

		final Iterator<Sheet> sheetIteratorExpected = expected.sheets.iterator();
		final Iterator<Sheet> sheetIteratorActual = actual.sheets.iterator();

		while (sheetIteratorExpected.hasNext()) {
			assertTrue(sheetIteratorActual.hasNext());
			final Sheet sheetExpected = sheetIteratorExpected.next();
			final Sheet sheetActual = sheetIteratorActual.next();
			final List<Row> rows = sheetActual.rows;

			assertEquals(sheetExpected.name, sheetActual.name);

			/*
			 * Exported sheets may contain more modules than imported since the import can
			 * create custom utility modules. The order of both sheets is different.
			 */

			final ListIterator<Row> rowIteratorExpected = sheetExpected.rows.listIterator();
			while (rowIteratorExpected.hasNext()) {
				final Row rowExpected = rowIteratorExpected.next();
				assertTrue("Expected row " + rowExpected + " in sheet " + sheetActual,
						rows.remove(rowExpected));
			}
		}
		 
		assertFalse(sheetIteratorActual.hasNext());
	}

	private byte[] exportServerside() throws ValidationException, IOException {
		final Result<Tuple2<String, byte[]>> result = MiningApiClient.ioService(getConnectionInfo())
				.exportEffortSummaryExcel()
				.setProjectId(PROJECT_ID)
				.execute();
		
		if ( ! result.isValid()) {
			throw new ValidationException("Export Error", result.getExtendedStatusMessage());
		}
		
		return result.getValue().get().b;
	}
	
	private String makeAbsolute(final String path) {
		return Paths.get(System.getProperty("user.dir"), path).toString();
	}

	private Workbook parseExcel(final byte[] bytes) throws IOException, SAXException, OpenXML4JException, XmlException, ParserConfigurationException {
		final Workbook workbook = new Workbook();
		try (final InputStream stream = new ByteArrayInputStream(bytes)) {
			try (final OPCPackage opcPackage = OPCPackage.open(stream)) {
				doParse(opcPackage, workbook);
			}
		}
		return workbook;
	}

	/*
	 * Using POI with a path is less memory consuming that using an InputStream as
	 * done in parseExcel(byte[]).
	 */
	private Workbook parseExcel(final String path) throws IOException, SAXException, OpenXML4JException, XmlException, ParserConfigurationException {
		final Workbook workbook = new Workbook();
		final String absolutePath = makeAbsolute(path);
		try (final OPCPackage opcPackage = OPCPackage.open(absolutePath)) {
			doParse(opcPackage, workbook);
		}
		return workbook;
	}

	private void doParse(final OPCPackage opcPackage, final Workbook workbook)
			throws IOException, SAXException, OpenXML4JException, XmlException, ParserConfigurationException {
		final ReadOnlySharedStringsTable sharedStringsTable = new ReadOnlySharedStringsTable(opcPackage);
		final XSSFReader xssfReader = new XSSFReader(opcPackage);
		final StylesTable stylesTable = xssfReader.getStylesTable();
		final XSSFReader.SheetIterator sheetIterator = (XSSFReader.SheetIterator) xssfReader.getSheetsData();

		while (sheetIterator.hasNext()) {
			try (final InputStream stream = sheetIterator.next()) {
				final String sheetName = sheetIterator.getSheetName();
				final DataFormatter formatter = new DataFormatter();
				final Sheet sheet = new Sheet(sheetName);
				workbook.sheets.add(sheet);
				final FilteringSheetContentsHandler contentsHandler = new FilteringSheetContentsHandler(sheet);
				final SAXParserFactory parserFactory = SAXParserFactory.newDefaultInstance();
				parserFactory.setNamespaceAware(true);
				final SAXParser parser = parserFactory.newSAXParser();
				final XMLReader sheetParser = parser.getXMLReader();
				sheetParser.setContentHandler(new XSSFSheetXMLHandler(stylesTable, null, sharedStringsTable,
						contentsHandler, formatter, false));
				sheetParser.parse(new InputSource(stream));
			}
		}

		/* must not close excelExtractor here since the package is closed by caller */
		@SuppressWarnings("resource")
		final XSSFEventBasedExcelExtractor excelExtractor = new XSSFEventBasedExcelExtractor(opcPackage);
		final CTProperty property = excelExtractor.getCustomProperties().getProperty(DISCOVERY_VERSION.name());
		if (property != null) {
			workbook.properties.put(DISCOVERY_VERSION, property.getLpwstr());
		}
	}
	
	/*
	 * Filters Uid since it is not the same in exported Excel. Handles missing
	 * columns as done in
	 * https://svn.apache.org/repos/asf/poi/trunk/src/examples/src/org/apache/poi/
	 * xssf/eventusermodel/XLSX2CSV.java
	 */
	private class FilteringSheetContentsHandler implements SheetContentsHandler {

		private static final String UID = "uid";

		private final Sheet sheet;
		private final List<String> rowValues = new ArrayList<>();
		private final Set<Integer> filteredColumns = new HashSet<>();

		private int currentRow = 0;
		private int currentColumn = -1;

		private FilteringSheetContentsHandler(final Sheet sheet) {
			this.sheet = sheet;
		}

		@Override
		public void startRow(final int rowNum) {
			rowValues.clear();
			currentColumn = -1;
		}

		@Override
		public void endRow(final int rowNum) {
			/* do not callback for header row */
			if (currentRow > 0) {
				sheet.addRow(rowValues);
			}
			currentRow++;
		}

		@Override
		public void cell(@Nullable final String cellReference, @Nullable final String formattedValue,
				@Nullable final XSSFComment comment) {
			/*
			 * gracefully handle missing cellReferences here in a similar way as XSSFCell
			 * does
			 */
			final String actualCellReference = cellReference != null ? cellReference
					: new CellAddress(currentRow, currentColumn).formatAsString();

			/* check for missing cells */
			int actualColumn = new CellReference(actualCellReference).getCol();
			int missedColumns = actualColumn - currentColumn - 1;
			for (int i = 0; i < missedColumns; i++) {
				rowValues.add(null);
			}
			currentColumn = actualColumn;

			if (filterCell(formattedValue)) {
				filteredColumns.add(Integer.valueOf(currentColumn));
			} else {
				rowValues.add(formattedValue);
			}
		}

		private boolean filterCell(@Nullable final String value) {
			return filteredColumns.contains(Integer.valueOf(currentColumn))
					|| (currentRow == 0 && value != null && value.toLowerCase().contains(UID));
		}
	}

	private class Workbook {

		private final List<Sheet> sheets = new ArrayList<>();
		private final Map<ExcelProperties, String> properties = new EnumMap<>(ExcelProperties.class);
	}

	private class Sheet {

		private final String name;

		private final List<Row> rows = new ArrayList<>();

		private Sheet(final String name) {
			this.name = name;
		}

		private void addRow(final List<String> rowValues) {
			rows.add(new Row(rowValues.toArray(new String[rowValues.size()])));
		}

		@Override
		public String toString() {
			return name;
		}
	}

	private class Row {

		private final String[] values;

		private Row(final String[] values) {
			this.values = values;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + Arrays.hashCode(values);
			return result;
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			return Arrays.equals(values, ((Row) obj).values);
		}

		@Override
		public String toString() {
			return Arrays.toString(values);
		}
	}
}
