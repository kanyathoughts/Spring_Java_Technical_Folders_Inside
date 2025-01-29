/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.api;

import java.util.Optional;
import java.util.UUID;

import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;

/**
 * Executor for the storeAst operation.
 */
public interface StoreAstExecutor {

	/**
	 * Executes the storeAst operation and returns the root node.
	 * 
	 * @param moduleId the Id of the module to execute storeAst for
	 * @param core the {@link MiningDataCoreService} for accessing entity services
	 * @return the root node if the execution was successful
	 */
	Optional<UUID> execute(EntityId moduleId, MiningDataCoreService core);

}
