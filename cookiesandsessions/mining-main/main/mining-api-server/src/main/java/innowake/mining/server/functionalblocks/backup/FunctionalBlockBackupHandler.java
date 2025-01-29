/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup;

import com.fasterxml.jackson.core.type.TypeReference;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Interface for handlers that can back up / restore additional data attached to functional blocks.
 *
 * @param <I> the input type; data is converted from JSON to this type when restoring the backup. Can be same as {@code <O>}.
 * @param <O> the output type; data is converted from this type to JSON when creating the backup. Can be same as {@code <I>}.
 * @see innowake.mining.server.functionalblocks.backup.model.FunctionalBlockBackup
 * @see FunctionalBlockBackupService
 */
public interface FunctionalBlockBackupHandler<I, O> {

	/**
	 * A unique identifier for this handler. This is added to the backup file.
	 * @return a unique identifier
	 */
	String getIdentifier();

	/**
	 * Returns the input type. Data is converted from JSON to this type when restoring the backup.
	 * @return the type of the input data
	 */
	TypeReference<I> getInputType();

	/**
	 * Process the list of functional blocks and return a map containing the additional data to be backed up for each block. Map should not
	 * contain entries for functional blocks for which there is no data present. Return empty map if there is no data to back up.
	 * @param context Context object providing context information to {@link FunctionalBlockBackupHandler}
	 * @param functionalBlocks the list of functional blocks to process
	 * @return additional data to be backed up for the given functional blocks
	 */
	Map<FunctionalBlockPojo, O> backup(FunctionalBlockBackupContext context, List<FunctionalBlockPojo> functionalBlocks);

	/**
	 * Restore additional data for the given functional blocks. This method receives a map of functional block uids and the data that should
	 * be restored for each block.
	 * @param context Context object providing context information to {@link FunctionalBlockBackupHandler}
	 * @param inputData map of functional block uids and the data that should be restored for the block
	 */
	void restore(FunctionalBlockBackupContext context, Map<UUID, I> inputData);
}
