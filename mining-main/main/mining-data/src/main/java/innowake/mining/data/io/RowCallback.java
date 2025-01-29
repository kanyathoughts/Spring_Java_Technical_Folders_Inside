/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

/**
 * Callback for importing rows of an Excel sheet.
 */
public interface RowCallback {

	/**
	 * Sets the column header strings of the Excel sheet.
	 * 
	 * @param headers the string headers
	 */
	void setHeaders(String[] headers);

	/**
	 * Called when a row was parsed completely.
	 *
	 * @param rowNum the row number
	 * @param row the row
	 * @throws IllegalArgumentException if a mandatory value is missing or if the amount of values in {@code row} does not match the amount of column headers
	 */
	void rowComplete(int rowNum, String[] row);
	
	/**
	 * Called when the sheet is parsed completely, i.e. after the final call to {@link #rowComplete(int, String[])}.
	 * <p>
	 * The default implementation does nothing.
	 */
	public default void sheetComplete() { /* does nothing by default */ }
}
