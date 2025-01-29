/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import static innowake.mining.shared.io.ExcelProperties.DISCOVERY_VERSION;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.InvalidOperationException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.xssf.eventusermodel.ReadOnlySharedStringsTable;
import org.apache.poi.xssf.eventusermodel.XSSFReader;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler.SheetContentsHandler;
import org.apache.poi.xssf.extractor.XSSFEventBasedExcelExtractor;
import org.apache.poi.xssf.model.StylesTable;
import org.apache.poi.xssf.usermodel.XSSFComment;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.xmlbeans.XmlException;
import org.openxmlformats.schemas.officeDocument.x2006.customProperties.CTProperty;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.io.WorkbookDefinition.Sheet;
import innowake.mining.shared.io.WorkbookDefinition.SheetColumn;

/**
 * Validates the Discovery Excel file.
 */
public class DiscoveryExcelValidator {

	private static final String INVALID_EXCEL_FILE = "Invalid Excel file";
	private static final String PLEASE_MAKE_SURE_MESSAGE = "Please make sure that this is a valid Discovery Excel file.";
	private static final String MODULES_SHEET_NAME = "Modules";
	
	private DiscoveryExcelValidator() {}
	
	/**
	 * Validates the Discovery Excel file.
	 *
	 * @param filePath the path of the Excel file
	 * @return returns list of non empty module paths
	 * @throws ValidationException if validation fails
	 */
	public static List<String> validateAndReturnModulePaths(final String filePath) throws ValidationException {
		validateCustomDiscoveryProperty(filePath);
		validateSheets(filePath);
		return getModulePathsList(filePath);
	}
	
	private static void validateCustomDiscoveryProperty(final String filePath) throws ValidationException {
		try (final XSSFEventBasedExcelExtractor excelExtractor = new XSSFEventBasedExcelExtractor(filePath)) {
			final CTProperty property = excelExtractor.getCustomProperties().getProperty(DISCOVERY_VERSION.name());
			
			if (property == null) {
				final String message = String.format("The Excel file is missing the custom property '%s'. "
						+ "Please make sure to use an up-to-date version of the Discovery Script.", DISCOVERY_VERSION.name());
				throw new ValidationException("Missing property", message);
			}
			
			final String propertyValue = property.getLpwstr();
			if ( ! propertyValue.equals(DISCOVERY_VERSION.getValue())) {
				final String message = String.format("The version '%s' of the Discovery Excel file is not supported. "
						+ "Please contact support at innowakesupport@deloitte.com.", propertyValue);
				throw new ValidationException("Unsupported Discovery Excel version", message);
			}
			
		} catch (final XmlException | OpenXML4JException | IOException e) {
			throw new ValidationException("Parse Error", "Error while parsing file", e);
		}
	}

	private static void validateSheets(final String filePath) throws ValidationException {
		try (final OPCPackage opcPackage = OPCPackage.open(filePath, PackageAccess.READ)) {
			final WorkbookValidator workbookValidator = new WorkbookValidator();
			final ReadOnlySharedStringsTable sharedStringsTable = new ReadOnlySharedStringsTable(opcPackage);
			final XSSFReader xssfReader = new XSSFReader(opcPackage);
			final StylesTable stylesTable = xssfReader.getStylesTable();
			final XSSFReader.SheetIterator sheetIterator = (XSSFReader.SheetIterator) xssfReader.getSheetsData();
			
			while (sheetIterator.hasNext()) {
				validate(workbookValidator, sharedStringsTable, stylesTable, sheetIterator);
			}
			
			workbookValidator.validate();
		} catch (final SAXException | IOException | InvalidOperationException | OpenXML4JException | ParserConfigurationException e) {
			throw new ValidationException("Parse Error", "Error while parsing file", e);
		}
	}

	private static void validate(final WorkbookValidator workbookValidator, final ReadOnlySharedStringsTable sharedStringsTable, final StylesTable stylesTable,
			final XSSFReader.SheetIterator sheetIterator) throws ValidationException, SAXException, IOException, ParserConfigurationException {
		try (final InputStream stream = sheetIterator.next()) {
			final String sheetName = sheetIterator.getSheetName();
			workbookValidator.sheetProcessed(sheetName);
			new SheetValidator(sheetName, stylesTable, sharedStringsTable, stream).validate();
		} catch (final SkipSheetException e) {
			Logging.warn(String.format("Validation of sheet failed: %s", sheetIterator.getSheetName()), e);
		}
	}
	
	private static List<String> getModulePathsList(final String filePath) {
		final List<String> modulePaths = new ArrayList<>();
		try (final XSSFWorkbook workBook = new XSSFWorkbook(new File(filePath))) {
			final XSSFSheet sheet = Assert.assertNotNull(workBook.getSheet(MODULES_SHEET_NAME));
			final int firstRowNum = sheet.getFirstRowNum();
			final XSSFRow firstRow = sheet.getRow(firstRowNum);

			final Map<String, Integer> columnIndexByName = new HashMap<>();
			for (final Cell cell : firstRow) {
				columnIndexByName.put(Cells.stringValue(cell), Integer.valueOf(cell.getColumnIndex()));
			}

			if (columnIndexByName.containsKey(DiscoveryExcelModulesColumn.PATH.toString())) {
				final Integer modulePathIndex = columnIndexByName.get(DiscoveryExcelModulesColumn.PATH.toString());
				XSSFRow row;
				for (int rowIndex = firstRowNum + 1; rowIndex <= sheet.getLastRowNum(); rowIndex++) {
					row = sheet.getRow(rowIndex);
					if (row != null) {
						final Cell cell = row.getCell(modulePathIndex);
						if (cell != null && cell.getStringCellValue().trim().length() > 0) {
							modulePaths.add(cell.getStringCellValue());
						}
					}
				}
			}
		} catch (final InvalidFormatException | IOException e) {
			throw new IllegalStateException(e);
		}
		return modulePaths;
	}
	
	private static class WorkbookValidator {
		
		private final List<String> processedSheetNames = new ArrayList<>();
		
		private void sheetProcessed(final String sheetName) {
			processedSheetNames.add(sheetName);
		}
		
		private void validate() throws ValidationException {
			for (final Sheet sheet : WorkbookDefinition.SHEETS.values()) {
				if (sheet.isMandatory() && ! processedSheetNames.contains(sheet.getSheetName())) {
					throw new ValidationException(INVALID_EXCEL_FILE, 
							String.format("Expected sheet name '%s'. " + PLEASE_MAKE_SURE_MESSAGE, sheet.getSheetName()));
				}
			}
		}
	}
	
	private static class SheetValidator {
		
		private final String sheetName;
		private final StylesTable stylesTable;
		private final ReadOnlySharedStringsTable sharedStringsTable;
		private final InputStream stream;

		private SheetValidator(
				final String sheetName, 
				final StylesTable stylesTable, 
				final ReadOnlySharedStringsTable sharedStringsTable, 
				final InputStream stream) {
			this.sheetName = sheetName;
			this.stylesTable = stylesTable;
			this.sharedStringsTable = sharedStringsTable;
			this.stream = stream;
		}

		private void validate() throws ValidationException, SAXException, IOException, ParserConfigurationException {
			if ( ! WorkbookDefinition.SHEETS.containsKey(sheetName)) {
				throw new ValidationException(INVALID_EXCEL_FILE, String.format("Unexpected sheet name '%s'. " + PLEASE_MAKE_SURE_MESSAGE, sheetName));
			}
			
			final DataFormatter formatter = new DataFormatter();
			final FirstRowSheetValidator contentsHandler = new FirstRowSheetValidator(WorkbookDefinition.SHEETS.get(sheetName));
			final SAXParserFactory parserFactory = SAXParserFactory.newInstance();
			parserFactory.setNamespaceAware(true);
			final SAXParser parser = parserFactory.newSAXParser();
			final XMLReader sheetParser = parser.getXMLReader();
			sheetParser.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			sheetParser.setFeature("http://xml.org/sax/features/external-general-entities", false);
			sheetParser.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			sheetParser.setContentHandler(new XSSFSheetXMLHandler(stylesTable, null, sharedStringsTable, contentsHandler, formatter, false));
			
			try {
				sheetParser.parse(new InputSource(stream));
			} catch (final FirstRowValidationException e) {
				/* do not rethrow cause exception since it is only used for unchecked escaping from parsing */
				throw new ValidationException(e.getTitle(), e.getMessage());
			}
		}
	}

	private static class FirstRowSheetValidator implements SheetContentsHandler {

		private final Sheet expectedSheet;
		private final Set<SheetColumn> mandatoryColumns;

		public FirstRowSheetValidator(final Sheet sheet) {
			expectedSheet = sheet;

			mandatoryColumns = new HashSet<>(sheet.getSheetColumns().length);
			for (final SheetColumn sheetColumn : sheet.getSheetColumns()) {
				if (sheetColumn.mandatory) {
					mandatoryColumns.add(sheetColumn);
				}
			}
		}

		@Override
		public void startRow(final int rowNum) {
			/* nothing to do */
		}

		@Override
		public void endRow(final int rowNum) {
			/* throw error if any of the mandatory columns is still present */
			if ( ! mandatoryColumns.isEmpty()) {
				throw new FirstRowValidationException(INVALID_EXCEL_FILE, String.format("Sheet %s is missing the mandatory columns: %s. " 
						+ PLEASE_MAKE_SURE_MESSAGE, 
						expectedSheet.getSheetName(), mandatoryColumns.stream().map(sh -> sh.name).collect(Collectors.joining(","))));
				
			}
			
			throw new SkipSheetException();
		}

		@Override
		public void cell(@Nullable final String cellReference, @Nullable final String formattedValue, @Nullable final XSSFComment comment) {
			SheetColumn match = null;
			
			final Iterator<SheetColumn> it = mandatoryColumns.iterator();
			while (it.hasNext() && match == null) {
				final SheetColumn mandatoryColumn = it.next();
				if (mandatoryColumn.name.equals(formattedValue)) {
					match = mandatoryColumn;
					break;
				}

				for (final String oldName : mandatoryColumn.oldNames) {
					if (oldName.equals(formattedValue)) {
						match = mandatoryColumn;
					}
				}
			}
			
			if (match != null) {
				mandatoryColumns.remove(match);
			}
		}
	}
	
	private static class FirstRowValidationException extends RuntimeException {

		private final String title;
		
		private FirstRowValidationException(final String title, final String message) {
			super(message);
			this.title = title;
		}
		
		private String getTitle() {
			return title;
		}
	}
	
	private static class SkipSheetException extends RuntimeException {
	}
}
