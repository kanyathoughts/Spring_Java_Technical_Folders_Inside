/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.api.lang.Nullable;

/**
 * Represents a basic unit of a module. It can be an entire module or part of a module.
 */
public interface ModuleUnit {

	@Nullable
	public String getRecordId();
	public Long getId();
	@Nullable
	public String getContentHash();
	public void setContentHash(final String contentHash);
}
