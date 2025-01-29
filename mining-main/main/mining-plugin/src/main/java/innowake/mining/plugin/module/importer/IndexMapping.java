/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;

/**
 * Mapping between column header and its index.
 * 
 * This is based on the string values of the first row in the sheet.
 */
public class IndexMapping {

	private Map<String, Integer> columnIndexByName = new HashMap<>();

	/**
	 * Creates an index mapping for the given sheet.
	 * 
	 * @param sheet the sheet for which the mapping should be created
	 */
	public IndexMapping(final XSSFSheet sheet) {
		final int firstRowNum = sheet.getFirstRowNum();
		final XSSFRow firstRow = sheet.getRow(firstRowNum);
		for (final Cell cell : firstRow) {
			columnIndexByName.put(Cells.stringValue(cell), Integer.valueOf(cell.getColumnIndex()));
		}
	}
	
	/**
	 * Returns the column index of the given column name.
	 *
	 * @param columnName the name of the column
	 * @return the index if the given column name exists
	 */
	public Optional<Integer> get(final String columnName) {
		return Optional.ofNullable(columnIndexByName.get(columnName));
	}

}
