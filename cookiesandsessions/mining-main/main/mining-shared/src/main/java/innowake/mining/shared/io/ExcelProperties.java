package innowake.mining.shared.io;

/**
 * Custom properties used in the Excel Discovery workbook.
 */
public enum ExcelProperties {
	
	/**
	 * The current (arbitrary) version of discovery. This version is written to the Excel properties and is then read when the Excel is imported into 
	 * Discovery UI. 
	 */
	DISCOVERY_VERSION("2.0");
	
	private String value;
	
	private ExcelProperties(final String value) {
		this.value = value;
	}
	
	/**
	 * @return the value for the property.
	 */
	public String getValue() {
		return value;
	}
	
}
