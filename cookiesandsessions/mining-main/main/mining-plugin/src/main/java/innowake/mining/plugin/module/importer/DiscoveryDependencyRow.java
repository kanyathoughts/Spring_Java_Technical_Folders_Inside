/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import org.apache.poi.xssf.usermodel.XSSFRow;

import innowake.mining.plugin.base.ValidationException;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Maps a row to the value of the reference which are relevant for importing the Discovery Excel.
 */
public class DiscoveryDependencyRow extends DiscoveryRow {

	private final Integer uidIndex;
	private final Integer targetUidIndex;
	private final Integer targetLanguageIndex;
	private final Integer targetTypeIndex;
	private final Integer targetNameIndex;
	private final Integer attributesIndex;

	/**
	 * Creates a new row mapping for dependencies.
	 * 
	 * @param row the row to map
	 * @param mapping the corresponding mapping
	 */
	public DiscoveryDependencyRow(final XSSFRow row, final IndexMapping mapping) {
		super(row);
		uidIndex = getIndex(mapping, DiscoveryExcelDependenciesColumn.UID);
		targetUidIndex = getIndex(mapping, DiscoveryExcelDependenciesColumn.TARGET_UID);
		targetLanguageIndex = getIndex(mapping, DiscoveryExcelDependenciesColumn.TARGET_LANGUAGE);
		targetTypeIndex = getIndex(mapping, DiscoveryExcelDependenciesColumn.TARGET_TYPE);
		targetNameIndex = getIndex(mapping, DiscoveryExcelDependenciesColumn.TARGET_NAME);
		attributesIndex = getIndex(mapping, DiscoveryExcelDependenciesColumn.ATTRIBUTES);
	}
	
	/**
	 * @return the source UID of the row
	 */
	public Long getSourceUid() {
		return Long.valueOf(get(uidIndex));
	}
	
	/**
	 * @return the target UID of the row
	 */
	public Long getTargetUid() {
		return Long.valueOf(get(targetUidIndex));
	}
	
	/**
	 * @return the target technology
	 * @throws ValidationException if the value is not supported
	 */
	public Technology getTargetTechnology() throws ValidationException {
		return getTechnology(targetLanguageIndex);
	}
	
	/**
	 * @return the target type
	 * @throws ValidationException if the value is not supported
	 */
	public Type getTargetType() throws ValidationException {
		return getType(targetTypeIndex);
	}
	
	/**
	 * @return the target name
	 */
	public String getTargetName() {
		return get(targetNameIndex);
	}

	/**
	 * @return a JSON String representing the attributes
	 */
	public String getAttributes() {
		return get(attributesIndex);
	}

	private Integer getIndex(final IndexMapping mapping, final DiscoveryExcelDependenciesColumn column) {
		return mapping.get(column.toString()).orElseThrow(IllegalStateException::new);
	}

}
