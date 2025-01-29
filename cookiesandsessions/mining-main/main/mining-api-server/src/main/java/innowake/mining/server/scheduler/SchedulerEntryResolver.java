/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.scheduler;

import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerType;

import java.util.Map;

/**
 * Interface for resolving scheduler entry type
 */
public interface SchedulerEntryResolver {

	String DEFAULT_SCHEDULER_IDENTIFIER = "default";

	/**
	 * @return the scheduler type
	 */
	SchedulerType resolverSchedulerType();

	/**
	 * @return the scheduler identifier
	 */
	String resolverSchedulerIdentifier();

	/**
	 * Resolves the scheduler entry type based on the node name and the content.
	 *
	 * @param nodeName the node name
	 * @param content the content
	 * @return the scheduler entry type
	 */
	SchedulerEntryType resolveSchedulerEntryType(String nodeName, Map<String, String> content);
}
