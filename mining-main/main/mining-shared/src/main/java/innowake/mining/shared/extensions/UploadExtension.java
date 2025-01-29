/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.extensions;

import innowake.mining.shared.io.UploadDescription;

/**
 * This interface can be implemented by extensions that support file uploads.
 */
public interface UploadExtension {

	/**
	 * Get a {@link UploadDescription} describing whether this extension supports file upload
	 * and what kind of file can be uploaded.
	 * @return an upload description
	 */
	UploadDescription getUploadDescription();
}
