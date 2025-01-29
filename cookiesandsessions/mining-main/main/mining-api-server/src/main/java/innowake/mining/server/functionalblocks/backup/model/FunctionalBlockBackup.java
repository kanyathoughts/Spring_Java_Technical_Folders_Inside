/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.server.functionalblocks.backup.FunctionalBlockBackupHandler;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;

import java.util.List;

/**
 * Backup of a functional block consisting of the {@link FunctionalBlockPojo} and additional data.
 */
public class FunctionalBlockBackup {

	private final FunctionalBlockPojo functionalBlock;

	private final List<FunctionalBlockAdditionalData> additionalData;

	@JsonCreator
	public FunctionalBlockBackup(@JsonProperty("functionalBlock") final FunctionalBlockPojo functionalBlock,
			@JsonProperty("additionalData") final List<FunctionalBlockAdditionalData> additionalData) {
		this.functionalBlock = functionalBlock;
		this.additionalData = additionalData;
	}

	/**
	 * Returns the backed up definition of the functional block.
	 * @return definition of the functional block
	 */
	public FunctionalBlockPojo getFunctionalBlock() {
		return functionalBlock;
	}

	/**
	 * Returns additional data attached to the functional block. A {@link FunctionalBlockBackupHandler} must exist to handle the data.
	 * @return additional data attached to the functional block
	 */
	public List<FunctionalBlockAdditionalData> getAdditionalData() {
		return additionalData;
	}
}
