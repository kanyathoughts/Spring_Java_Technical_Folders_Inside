/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.model.Technology;

/**
 * Creates the Statements sheet of a Discovery result file.
 */
abstract class StatementsGenerator implements ModuleMappingConsumer {

	@Autowired
	private ModuleService moduleService;
	
	/**
	 * Processes all Statements for a Discovery result file.
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

		final var data = moduleService.getModuleStatementExport(projectId, false, sorted);
		if (data.isPresent()) {
			final var iterator = data.get().iterator();
			while (iterator.hasNext()) {
				createRow(iterator.next().toArray());
			}
		}
	}

	long getStatementCount(final EntityId project) {
		return moduleService.countStatements(q -> q.ofProject(project).notWithTechnology(Technology.SQL));
	}
}
