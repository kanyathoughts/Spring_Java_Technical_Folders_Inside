/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.scheduler;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipFilter;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;

import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * Interface to resolve the relationships of a scheduler condition entry.
 */
public interface SchedulerConditionEntryResolver extends SchedulerEntryResolver {

	List<SchedulerEntryRelationshipFilter> resolveRelationships(EntityId project, SchedulerEntryPojo schedulerEntry, Supplier<ModuleLightweightPojo> module);

	@Override
	default SchedulerEntryType resolveSchedulerEntryType(final String nodeName, final Map<String, String> content) {
		switch (nodeName.toUpperCase()) {
			case "INCOND":
			case "OUTCOND":
				return SchedulerEntryType.CONDITION;
			default:
				return SchedulerEntryType.UNKNOWN;
		}
	}
}
