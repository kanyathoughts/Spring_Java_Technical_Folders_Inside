/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.MiningPojoPrototype;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

/**
 * Prototype for the SchedulerImport entity.
 */
public class SchedulerImportPojoPrototype extends MiningPojoPrototype<SchedulerImportPojoPrototype> {

	public final Definable<EntityId> project = new Definable<>(false, "SchedulerImport.project");
	public final Definable<SchedulerType> schedulerType = new Definable<>(false, "SchedulerImport.schedulerType");
	public final Definable<String> schedulerVersion = new Definable<>(true, "SchedulerImport.schedulerVersion");
	public final Definable<String> identifier = new Definable<>(false, "SchedulerImport.identifier");
	public final Definable<UUID> source = new Definable<>(false, "SchedulerImport.source");
	public final Definable<Instant> importedOn = new Definable<>(false, "SchedulerImport.importDate");
	public final Definable<String> importerUsed = new Definable<>(true, "SchedulerImport.importerUsed");
	public final Definable<String> description = new Definable<>(true, "SchedulerImport.description");
	public final Definable<Map<String, Object>> properties = new Definable<>(false, "SchedulerImport.properties");


	public SchedulerImportPojoPrototype() {
		super("SchedulerImport");
	}

	public SchedulerImportPojoPrototype setProject(final EntityId project) {
		this.project.set(project);
		return this;
	}

	public SchedulerImportPojoPrototype setSchedulerType(final SchedulerType schedulerType) {
		this.schedulerType.set(schedulerType);
		return this;
	}

	public SchedulerImportPojoPrototype setSchedulerVersion(final String schedulerVersion) {
		this.schedulerVersion.set(schedulerVersion);
		return this;
	}

	public SchedulerImportPojoPrototype setIdentifier(final String identifier) {
		this.identifier.set(identifier);
		return this;
	}

	public SchedulerImportPojoPrototype setSource(final UUID source) {
		this.source.set(source);
		return this;
	}

	public SchedulerImportPojoPrototype setImportedOn(final Instant importedOn) {
		this.importedOn.set(importedOn);
		return this;
	}

	public SchedulerImportPojoPrototype setImporterUsed(final String importerUsed) {
		this.importerUsed.set(importerUsed);
		return this;
	}

	public SchedulerImportPojoPrototype setDescription(final String description) {
		this.description.set(description);
		return this;
	}

	public SchedulerImportPojoPrototype setProperties(final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}
}
