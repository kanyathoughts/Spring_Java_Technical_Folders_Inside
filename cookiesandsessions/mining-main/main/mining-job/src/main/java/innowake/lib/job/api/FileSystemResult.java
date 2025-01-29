/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

/**
 * A special object to be set as the {@link Job} result in {@link Result}
 * that is returned when the Job results have been stored externally on the server's file system
 * using {@link Job#createResultFile()}.
 * 
 * @see Job#createResultFile()
 */
public class FileSystemResult extends MimeResult {
	private static final long serialVersionUID = 1L;
	private final boolean appInternal;
	
	/**
	 * Constructor.
	 * 
	 * @param contentType the content type like "application/octet-stream"
	 * @param fileName the file name including extension to be presented to clients
	 */
	public FileSystemResult(final String contentType, final String fileName) {
		this(contentType, fileName, false);
	}

	/**
	 * Constructor.
	 * 
	 * @param contentType the content type like "application/octet-stream"
	 * @param fileName the file name including extension to be presented to clients
	 * @param appInternal {@code true} if this result file is application internal and should not be accessible by users, e.g. downloadable in the UI.
	 * 			Otherwise {@code false} (default)
	 */
	public FileSystemResult(final String contentType, final String fileName, final boolean appInternal) {
		super(contentType, fileName);
		this.appInternal = appInternal;
	}
	
	/**
	 * @return {@code true} if the job result is application internal; {@code false} otherwise
	 */
	public boolean isAppInternal() {
		return appInternal;
	}
}
