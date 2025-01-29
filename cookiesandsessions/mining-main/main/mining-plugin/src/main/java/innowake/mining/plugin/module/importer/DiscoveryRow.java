/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;

import innowake.mining.plugin.base.ValidationException;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Maps a row to the value of a module which are relevant for importing the Discovery Excel.
 */
public class DiscoveryRow {

	protected static final String VALIDATION_EXCEPTION_TITLE = "Incompatible format";

	private final XSSFRow row;
	
	/**
	 * Creates a new row mapping.
	 * 
	 * @param row the row to map
	 */
	public DiscoveryRow(final XSSFRow row) {
		this.row = row;
	}
	
	/**
	 * @param index the index of the language column
	 * @return the target technology
	 * @throws ValidationException if the value is not supported
	 */
	protected Technology getTechnology(final Integer index) throws ValidationException {
		final String languageName = get(index);
		try {
			return Technology.fromName(languageName);
		} catch (final IllegalArgumentException e) {
			final String message = String.format("Unsupported language '%s'", languageName);
			throw new ValidationException(VALIDATION_EXCEPTION_TITLE, message, e);
		}
	}
	
	/**
	 * @param index the index of the type column
	 * @return the target type
	 * @throws ValidationException if the value is not supported
	 */
	protected Type getType(final Integer index) throws ValidationException {
		final String typeName = get(index);
		try {
			return Type.fromName(typeName);
		} catch (final IllegalArgumentException e) {
			final String message = String.format("Unsupported type '%s'", typeName);
			throw new ValidationException(VALIDATION_EXCEPTION_TITLE, message, e);
		}
	}

	protected String get(final Integer index) {
		final XSSFCell cell = row.getCell(index.intValue());
		if (cell == null) {
			return "";
		}
		return Cells.stringValue(cell);
	}
}
