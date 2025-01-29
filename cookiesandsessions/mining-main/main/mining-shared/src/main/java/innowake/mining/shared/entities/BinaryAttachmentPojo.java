/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Base64;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model class for MIME-typed BLOBs.
 */
@MiningDataType(name = MiningEnitityNames.BINARY_ATTACHMENT)
public class BinaryAttachmentPojo {

	private final String mime;
	private final byte[] data;
	
	public BinaryAttachmentPojo(final String mime, final byte[] data) {
		this.mime = mime;
		this.data = data;
	}
	
	/**
	 * Gets the MIME type of the BLOBs content.
	 * @return MIME type.
	 */
	public String getMime() {
		return mime;
	}
	
	/**
	 * Gets the content of the BLOB.
	 * @return Binary content.
	 */
	public byte[] getData() {
		return data;
	}
	
	/**
	 * Returns the Data-URL representation of this BLOB. 
	 * @return String of the form <pre>data:&lt;MIME type&gt;;base64,&lt;data&gt;</pre>
	 */
	public String toDataURL() {
		return new StringBuilder("data:")
			.append(mime)
			.append(";base64,")
			.append(Base64.getEncoder().encodeToString(data))
			.toString();
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.append("mime", mime);
		builder.append("data", data.length);
		return builder.toString();
	}

}
