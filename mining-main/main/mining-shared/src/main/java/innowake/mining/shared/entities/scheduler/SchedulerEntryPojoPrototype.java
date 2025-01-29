/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.MiningPojoPrototype;

import java.util.Map;
import java.util.UUID;

/**
 * Prototype for a scheduler entry.
 */
public class SchedulerEntryPojoPrototype extends MiningPojoPrototype<SchedulerEntryPojoPrototype> {

	public final Definable<UUID> containedIn = new Definable<>(true, "SchedulerEntry.containedIn");
	public final Definable<EntityId> module = new Definable<>(true, "SchedulerEntry.module");
	public final Definable<String> identifier = new Definable<>(false, "SchedulerEntry.identifier");
	public final Definable<UUID> schedulerImport = new Definable<>(false, "SchedulerEntry.schedulerImport");
	public final Definable<SchedulerEntryType> type = new Definable<>(false, "SchedulerEntry.type");
	public final Definable<Map<String, String>> content = new Definable<>(false, "SchedulerEntry.content");

	public SchedulerEntryPojoPrototype() {
		super("SchedulerEntry");
	}

	public SchedulerEntryPojoPrototype setIdentifier(final String identifier) {
		this.identifier.set(identifier);
		return this;
	}

	public SchedulerEntryPojoPrototype setSchedulerImport(final UUID schedulerImport) {
		this.schedulerImport.set(schedulerImport);
		return this;
	}

	public SchedulerEntryPojoPrototype setType(final SchedulerEntryType type) {
		this.type.set(type);
		return this;
	}

	public SchedulerEntryPojoPrototype setContent(final Map<String, String> content) {
		this.content.set(content);
		return this;
	}

	public SchedulerEntryPojoPrototype setContainedIn(final UUID containedIn) {
		this.containedIn.set(containedIn);
		return this;
	}

	public SchedulerEntryPojoPrototype setModule(final EntityId module) {
		this.module.set(module);
		return this;
	}
}
