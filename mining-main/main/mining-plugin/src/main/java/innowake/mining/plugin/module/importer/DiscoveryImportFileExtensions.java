/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

/**
 * Valid file extensions for Discovery (Metrics) import files
 */
public enum DiscoveryImportFileExtensions {
	
	XLS("xls"),
	XLSX("xlsx"),
	CSV("csv");
	
	private final String fileExtension;
	
	private DiscoveryImportFileExtensions(final String fileExtension) {
		this.fileExtension = fileExtension;
	}

	/**
	 * Returns the file extension (without ".").
	 *
	 * @return the file extension
	 */
	public String getFileExtension() {
		return fileExtension;
	}
}
