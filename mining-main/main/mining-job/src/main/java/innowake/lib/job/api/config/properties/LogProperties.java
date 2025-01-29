/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.config.properties;

import javax.validation.constraints.NotBlank;

/**
 * Logging specific properties.
 */
public class LogProperties {
	
	@NotBlank
	private String logFolder = "logs";
	@NotBlank
	private String logFilePrefix = "job-";

	/**
	 * @return The folder where logs files are available.
	 */
	public String getLogFolder() {
		return logFolder;
	}
	
	/**
	 * Sets the folder where logs files should be written to.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param logFolder the log folder
	 */
	public void setLogFolder(final String logFolder) {
		this.logFolder = logFolder;
	}

	/**
	 * @return The file prefix name of the log files.
	 */
	public String getLogFilePrefix() {
		return logFilePrefix;
	}
	
	/**
	 * Sets the prefix that should be used for job specific log files.
	 * <p>
	 * <b>Note: </b> This should never be called by clients.
	 * 
	 * @param logFilePrefix the prefix
	 */
	public void setLogFilePrefix(final String logFilePrefix) {
		this.logFilePrefix = logFilePrefix;
	}

}
