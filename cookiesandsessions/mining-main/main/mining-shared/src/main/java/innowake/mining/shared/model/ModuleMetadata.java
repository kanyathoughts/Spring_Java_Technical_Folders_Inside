/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.HashMap;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;

/**
 * Represents metadata of a module.
 */
public class ModuleMetadata implements AdditionalInfo {

	private final Map<String, Object> metadata;

	
	public Map<String, Object> getMetadata() {
		return metadata;
	}

	public ModuleMetadata() {
		this.metadata = new HashMap<>();
	}

	public ModuleMetadata(final Map<String, Object> metadata) {
		this.metadata = metadata;
	}
	
	public void put(final String key, @Nullable final Object value) {
		metadata.put(key, value);
	}
}
