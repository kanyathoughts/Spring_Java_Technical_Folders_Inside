/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ModuleUndiscoveredPojo;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * Creates the Undiscovered sheet of a Discovery result file.
 */
abstract class UndiscoveredGenerator implements ModuleMappingConsumer {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

    @Autowired
	private ModuleService moduleService;

	/**
	 * Processes all Undiscovered Modules for a Discovery result file.
	 *
	 * @param projectId the project ID
	 * @param moduleMapping between Module ID and a {@link Tuple2} of UID and Discovery type
	 * @param sorted if {@code true}, sorts the result
	 * @throws IOException if an error occurs
	 */
	@Override
	public void process(final EntityId projectId, 
			final Map<Long, String> moduleMapping, 
			final DiscoveryExportOptions options,
			final boolean sorted) throws IOException {
		final AtomicInteger uuid = new AtomicInteger(0);
		final List<ModuleUndiscoveredPojo> queryResult = moduleService.findUndiscovered(builder -> {
			builder.ofProject(projectId);

			if (sorted) {
				builder.sortName(SortDirection.ASCENDING);
				builder.sortPath(SortDirection.ASCENDING);
			}
		});

		final List<Object[]> formattedQueryResult = queryResult.stream()
				.map(pojo -> new Object[] {
                        (long) uuid.incrementAndGet(),
						pojo.getName(),
						pojo.getPath()
				}).collect(Collectors.toList());

		LOG.info(() -> String.format("Discovery Metrics: exporting Undiscovered overall %d", Integer.valueOf(queryResult.size())));
		final AtomicInteger cnt = new AtomicInteger(0);

		for (final Object[] values : formattedQueryResult) {
			createRow(values);
			if (cnt.incrementAndGet() % 100000 == 0) {
				LOG.info(() -> String.format("Discovery Metrics: exporting Undiscovered current %d", Integer.valueOf(cnt.get())));
			}
		}
	}

	/**
	 * Returns the number of undiscovered entities associated with the given project. 
	 *
	 * @param projectId the ID of the project to search for undiscovered entities
	 * @return the number of undiscovered entities
	 */
	public Long getUndiscoveredCount(final EntityId projectId) {
		return moduleService.countUndiscovered(q -> q.ofProject(projectId));
	}
}
