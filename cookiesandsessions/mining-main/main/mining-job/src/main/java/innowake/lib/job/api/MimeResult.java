/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.lib.job.api;

import java.io.Serializable;

/**
 * Super class of {@link Job} result in {@link Result} to encapsulate MIME data for Rest Services
 */
public abstract class MimeResult implements Serializable {

	private final String contentType;
	private final String fileName;

	/**
	 * Constructor.
	 * 
	 * @param contentType the content type like "application/octet-stream"
	 * @param fileName the file name including extension to be presented to clients
	 */
	protected MimeResult(final String contentType, final String fileName) {
		this.contentType = contentType;
		this.fileName = fileName;
	}

	/**
	 * @return the content type
	 */
	public String getContentType() {
		return contentType;
	}

	/**
	 * @return the file name including extension
	 */
	public String getFileName() {
		return fileName;
	}
}
