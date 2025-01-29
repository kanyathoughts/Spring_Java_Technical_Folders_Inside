/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.server.functionalblocks.backup.FunctionalBlockBackupHandler;

/**
 * Additional data attached to a functional block that is added to {@link FunctionalBlockBackup}.
 */
public class FunctionalBlockAdditionalData {

	private final String type;
	private final Object data;

	@JsonCreator
	public FunctionalBlockAdditionalData(@JsonProperty("type") final String type, @JsonProperty("data") final Object data) {
		this.type = type;
		this.data = data;
	}

	/**
	 * The type of the additional data. This is determined by {@link FunctionalBlockBackupHandler#getIdentifier()}.
	 * @return the type of the backed up data
	 */
	public String getType() {
		return type;
	}

	/**
	 * The data as a Map which is serialized to/from JSON.
	 * @return the backed up data
	 */
	public Object getData() {
		return data;
	}
}
