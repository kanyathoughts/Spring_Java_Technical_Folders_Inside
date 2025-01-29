/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * The keys of the properties map of a {@code module_relationship}.
 */
public enum ReferenceAttributes {
	DB_ACCESS_TYPES("dbAccessTypes", "DB_ACCESS_TYPE"),
	DB_ACCESS_OPERATIONS("dbAccessOperations", "DB_ACCESS_OPERATION");
	
	
	private final String referenceAttributeExcelName;
	private final String referenceAttributeDbName;
	
	private ReferenceAttributes(final String dbName, final String excelName) {
		this.referenceAttributeDbName = dbName;
		this.referenceAttributeExcelName = excelName;
	}	
	
	/**
	 * Returns the name of the key used in the Excel for the attribute.
	 *
	 * @return the name of the key
	 */
	public String getReferenceAttributeExcelName() {
		return referenceAttributeExcelName;
	}
	
	/**
	 * Returns the DB entity name of the {@code module_relationship} subclass based on this relationship.
	 *
	 * @return the DB entity name of the {@code module_relationship} subclass
	 */
	public String getReferenceDbEntityName() {
		return referenceAttributeDbName;
	}
}
