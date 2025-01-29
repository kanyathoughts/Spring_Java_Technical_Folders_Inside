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
 * Creates the Errors sheet of a Discovery result file.
 */
abstract class ErrorsGenerator implements ModuleMappingConsumer {

	@Autowired
	private ModuleService moduleService;

	/**
	 * Processes all Errors for a Discovery result file.
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
		
		final Optional<Table> data = moduleService.getModuleErrorMarkerExport(projectId, sorted);
		if (data.isPresent()) {
			final var iterator = data.get().iterator();
			while (iterator.hasNext()) {
				createRow(iterator.next().toArray());
			}
		}
	}

	/**
	 * Returns the number of errors associated with the given project. 
	 *
	 * @param projectId the ID of the project to search for errors
	 * @return the number of errors
	 */
	public long getErrorsCount(final EntityId projectId) {
		return moduleService.countErrorMarkers(q -> q.ofProject(projectId));
	}
}
