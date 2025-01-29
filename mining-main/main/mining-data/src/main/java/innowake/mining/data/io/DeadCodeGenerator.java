/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Table;

/**
 * Creates the Dead Code sheet of a Discovery result file.
 */
abstract class DeadCodeGenerator implements ModuleMappingConsumer {

	@Autowired
	private ModuleService moduleService;

	/**
	 * Processes all Dead Code for a Discovery result file.
	 *
	 * @param projectId the project ID
	 * @param moduleMapping mapping between Module ID and a {@link Tuple2} of UID and Discovery type
	 * @param sorted if {@code true}, sorts the result
	 * @throws IOException if an error occurs
	 */
	@Override
	public void process(final EntityId projectId,
						final Map<Long, String> moduleMapping,
						final DiscoveryExportOptions options,
						final boolean sorted) throws IOException {
		final Optional<Table> data = moduleService.getModuleDeadCode(projectId, sorted);
		if (data.isPresent()) {
			final var iterator = data.get().iterator();
			while (iterator.hasNext()) {
				createRow(iterator.next().toArray());
			}
		}
	}

	/**
	 * Returns the number of dead code occurrences associated with the given project. 
	 *
	 * @param projectId the ID of the project to search for dead code
	 * @return the number of dead code occurrences
	 */
	public long getDeadCodeCount(final EntityId projectId) {
		return moduleService.countDeadCode(q -> q.ofProject(projectId));
	}
}
