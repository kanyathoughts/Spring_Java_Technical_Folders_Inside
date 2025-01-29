/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */

package innowake.lib.job.api;

/**
 * A special object containing raw byte data with associated metadata
 * extends the `MimeResult` class and is designed for scenarios
 * where binary data needs to be sent as a response
 */

public class ByteArrayResult extends MimeResult {
	private static final long serialVersionUID = 1L;
	private final byte[] data;

	/**
	 * Constructor.
	 *
	 * @param contentType the content type like "application/octet-stream"
	 * @param fileName the file name including extension to be presented to clients
	 * @param data the actual raw data matching to the contentType
	 */

	public ByteArrayResult(final String contentType, final String fileName, final byte[] data) {
		super(contentType, fileName);
		this.data = data;
	}

	/**
	 * @return the raw data matching the content type
	 */
	public byte[] getData() {
		return data;
	}

}
