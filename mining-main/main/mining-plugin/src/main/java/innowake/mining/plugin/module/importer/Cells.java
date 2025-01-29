/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import org.apache.poi.ss.usermodel.Cell;

/**
 * Convenience methods for POT Cells.
 */
public final class Cells {
	
	private Cells() {}

	/**
	 * Returns the string value of the given cell.
	 * 
	 * For numeric cells we only return the integral part.
	 * 
	 * @param cell the cell the value is to be retrieved
	 * @return the string value of the cell
	 */
	public static String stringValue(final Cell cell) {
		switch (cell.getCellType()) {
			case STRING:
				return cell.getStringCellValue();
			case NUMERIC:
				/* In this use case we are only interested in the integral part */
				final long numericCellValue = (long) cell.getNumericCellValue();
				return String.valueOf(numericCellValue);
			default:
				final String message = String.format("Cell type %s not supported", cell.getCellType());
				throw new IllegalStateException(message);
		}
	}
}
