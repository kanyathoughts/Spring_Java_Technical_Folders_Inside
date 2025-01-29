/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import org.apache.poi.xssf.usermodel.XSSFRow;

import innowake.mining.plugin.base.ValidationException;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Maps a row to the value of a module which are relevant for importing the Discovery Excel.
 */
public class DisoveryModuleRow extends DiscoveryRow {

	private final Integer uidIndex;
	private final Integer nameIndex;
	private final Integer pathIndex;
	private final Integer languageIndex;
	private final Integer typeIndex;
	private final Integer linesOfCodeIndex;
	private final Integer linesOfCommentIndex;
	private final Integer complexityIndex;

	/**
	 * Creates a new row mapping.
	 * 
	 * @param row the row to map
	 * @param mapping the corresponding mapping
	 */
	public DisoveryModuleRow(final XSSFRow row, final IndexMapping mapping) {
		super(row);
		uidIndex = getIndex(mapping, DiscoveryExcelModulesColumn.UID);
		nameIndex = getIndex(mapping,  DiscoveryExcelModulesColumn.NAME);
		pathIndex = getIndex(mapping, DiscoveryExcelModulesColumn.PATH);
		languageIndex = getIndex(mapping, DiscoveryExcelModulesColumn.LANGUAGE);
		typeIndex = getIndex(mapping, DiscoveryExcelModulesColumn.TYPE);
		linesOfCodeIndex = getIndex(mapping, DiscoveryExcelModulesColumn.CODELINES);
		linesOfCommentIndex = getIndex(mapping, DiscoveryExcelModulesColumn.COMMENTLINES);
		complexityIndex = getIndex(mapping, DiscoveryExcelModulesColumn.COMPLEXITY);
	}

	/**
	 * @return the UID of the row
	 */
	public String getUid() {
		return get(uidIndex);
	}

	/**
	 * @return the name of the row
	 */
	public String getName() {
		return get(nameIndex);
	}
	
	/**
	 * @return the path of the row
	 */
	public String getPath() {
		return get(pathIndex);
	}
	
	/**
	 * @return the technology of the row
	 * @throws ValidationException if the value is not supported
	 */
	public Technology getTechnology() throws ValidationException {
		return getTechnology(languageIndex);
	}
	
	/**
	 * @return the type of the row
	 * @throws ValidationException if the value is not supported
	 */
	public Type getType() throws ValidationException {
		return getType(typeIndex);
	}
	
	/**
	 * @return the storage of the row
	 * @throws ValidationException if the value is not supported
	 */
	public Storage getStorage() throws ValidationException {
		try {
			return Storage.from(getTechnology(), getType());
		} catch (final IllegalArgumentException e) {
			final String message = String.format("Storage for technology '%s' and type '%s' not supported", getTechnology(), getType());
			throw new ValidationException(VALIDATION_EXCEPTION_TITLE, message, e);
		}
	}
	
	/**
	 * @return the number of lines of code
	 */
	public Integer getLinesOfCode() {
		return Integer.valueOf(get(linesOfCodeIndex));
	}
	
	/**
	 * @return the number of lines of comment
	 */
	public Integer getLinesOfComment() {
		return Integer.valueOf(get(linesOfCommentIndex));
	}
	
	/**
	 * @return the complexity score
	 */
	public Integer getComplexity() {
		return Integer.valueOf(get(complexityIndex));
	}

	private Integer getIndex(final IndexMapping mapping, final DiscoveryExcelModulesColumn column) {
		return mapping.get(column.toString()).orElseThrow(IllegalStateException::new);
	}
	
}
