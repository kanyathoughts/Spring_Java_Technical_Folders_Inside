/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;

import org.apache.poi.xssf.streaming.SXSSFCell;
import org.apache.poi.xssf.streaming.SXSSFRow;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;

import innowake.mining.shared.io.WorkbookDefinition.Sheet;

/**
 * Creates a sheet of a Discovery Excel workbook.
 */
class SheetGenerator extends AbstractGenerator {

	/* https://support.office.com/en-au/article/Excel-specifications-and-limits-ca36e2dc-1f09-4620-b726-67c00b05040f */
	private static final int CELL_CONTENT_MAX_LENGTH = 32767;
	/* Separator used in newly created sheets upon exceeding MAX_ROWS_PER_SHEET */
	public static final String SHEET_NAME_INDEX_SEPARATOR = "__";

	private final SXSSFWorkbook workbook;
	private final Sheet sheetDefinition;
	private SXSSFSheet sheet;
	
	private int rowIndex = 0;
	private int sheetIndex = 0;
	private int maxRowsPerSheet;

	/**
	 * Creates an instance.
	 *
	 * @param workbook the workbook
	 * @param sheetDefinition the sheet
	 * @param maxRowsPerSheet 
	 */
	SheetGenerator(final SXSSFWorkbook workbook, final Sheet sheetDefinition, final int maxRowsPerSheet) {
		this.workbook = workbook;
		this.sheetDefinition = sheetDefinition;
		this.maxRowsPerSheet = maxRowsPerSheet;
		this.sheet = createSheet();
	}

	@Override
	void createRow(final Object... values) throws IOException {
		/* Create a new sheet once the maximum number of rows (MAX_ROWS_PER_SHEET) is reached */
		if (rowIndex >= maxRowsPerSheet) {
			createSheet();
		}

		final SXSSFRow row = sheet.createRow(++rowIndex);
		for (int cellIndex = 0; cellIndex < values.length; cellIndex++) {
			final Object value = values[cellIndex];
			final SXSSFCell cell = row.createCell(cellIndex);
			if (value instanceof String) {
				cell.setCellValue(format((String) value));
			} else if (value instanceof Integer) {
				cell.setCellValue(((Integer) value).intValue());
			} else if (value instanceof Long) {
				cell.setCellValue(((Long) value).longValue());
			} else if (value instanceof Double) {
				cell.setCellValue(normalizeDouble((Double) value).doubleValue());
			} else if (value instanceof Float) {
				cell.setCellValue(normalizeFloat((Float) value).doubleValue());
			} else if (value instanceof Boolean b) {
				cell.setCellValue(b.toString());
			} else if (value == null) {
				cell.setCellValue(normalizeNull());
			} else {
				throw new IOException(String.format("Unsupported value type '%s' in column %d in sheet '%s'",
						value.getClass().getName(), Integer.valueOf(cellIndex), sheet.getSheetName()));
			}
		}
	}

	private static String format(final String value) {
		return value.length() >= CELL_CONTENT_MAX_LENGTH ? value.substring(0, CELL_CONTENT_MAX_LENGTH - 5) + " ..." : value;
	}
	
	private SXSSFSheet createSheet() {
		final String sheetName = sheetIndex == 0 ? sheetDefinition.getSheetName() : sheetDefinition.getSheetName() + SHEET_NAME_INDEX_SEPARATOR + sheetIndex;
		sheet = workbook.createSheet(sheetName);
		rowIndex = 0;
		++sheetIndex;
		/* Add header row to the newly created sheet*/
		final SXSSFRow headerRow = sheet.createRow(rowIndex);
		final String[] columnNames = sheetDefinition.getColumnNames();
		for (int cellIndex = 0; cellIndex < columnNames.length; cellIndex++) {
			headerRow.createCell(cellIndex).setCellValue(columnNames[cellIndex]);
		}
		return sheet;
	}
}
