/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

import innowake.mining.shared.Definable;
import innowake.mining.shared.entities.MiningPojoPrototype;

import java.util.UUID;

/**
 * Prototype for {@link SchedulerEntryRelationshipPojo}.
 */
public class SchedulerEntryRelationshipPojoPrototype extends MiningPojoPrototype<SchedulerEntryRelationshipPojoPrototype> {

	public final Definable<UUID> predecessor = new Definable<>(false, "SchedulerEntryRelationship.predecessor");
	public final Definable<UUID> successor = new Definable<>(false, "SchedulerEntryRelationship.successor");
	public final Definable<Boolean> isOk = new Definable<>(false, "SchedulerEntryRelationship.isOk");
	public final Definable<String> identifier = new Definable<>(true, "SchedulerEntryRelationship.identifier");

	public SchedulerEntryRelationshipPojoPrototype() {
		super("SchedulerEntryRelationship");
	}

	public SchedulerEntryRelationshipPojoPrototype setPredecessor(final UUID predecessor) {
		this.predecessor.set(predecessor);
		return this;
	}

	public SchedulerEntryRelationshipPojoPrototype setSuccessor(final UUID successor) {
		this.successor.set(successor);
		return this;
	}

	public SchedulerEntryRelationshipPojoPrototype setIsOk(final boolean isOk) {
		this.isOk.set(isOk);
		return this;
	}

	public SchedulerEntryRelationshipPojoPrototype setIdentifier(final String identifier) {
		this.identifier.set(identifier);
		return this;
	}
}
