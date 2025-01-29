/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.scheduler;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.model.ModuleType;

import java.util.Map;
import java.util.Optional;

/**
 * Interface for resolving scheduler job entries
 */
public interface SchedulerJobEntryResolver extends SchedulerEntryResolver {

	/**
	 * Identifies the module from the given content and returns the module filter to further resolve the module
	 *
	 * @param content the content
	 * @return the module filter
	 */
	Optional<ModuleFilter> resolveModule(EntityId project, Map<String, String> content);

	/**
	 * The default module type to assume when a module is missing
	 *
	 * @param content the job entry content
	 * @return the default module type
	 */
	ModuleType missingModuleType(Map<String, String> content);

	/**
	 * Resolves the scheduler type from the content
	 *
	 * @param content the content
	 * @return the scheduler type
	 */
	@Override
	default SchedulerEntryType resolveSchedulerEntryType(final String nodeName, final Map<String, String> content) {
		if (nodeName.toUpperCase()
				.contains("JOB")) {
			return SchedulerEntryType.JOB;
		}
		return SchedulerEntryType.UNKNOWN;
	}
}
