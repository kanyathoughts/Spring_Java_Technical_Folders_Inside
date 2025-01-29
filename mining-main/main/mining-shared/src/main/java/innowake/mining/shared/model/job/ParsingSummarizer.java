/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.job;

import innowake.mining.shared.access.EntityId;

/**
 * Summarizes the result of a module parsing operation.
 */
public interface ParsingSummarizer {
	/**
	 * Record the module as successfully parsed.
	 * @param moduleId the module parsed.
	 */
	void success(final EntityId moduleId);
	
	/**
	 * Record the module as unsuccessfully parsed.
	 * @param moduleId the module parsed.
	 */
	void error(final EntityId moduleId);
	
	/**
	 * Record the module as being unsupported.
	 * @param moduleId the module parsed.
	 */
	void unsupported(final EntityId moduleId);
}