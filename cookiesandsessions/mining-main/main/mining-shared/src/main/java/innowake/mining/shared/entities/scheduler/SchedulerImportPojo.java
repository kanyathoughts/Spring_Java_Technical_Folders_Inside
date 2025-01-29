/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;

/**
 * POJO for Scheduler Import
 */
public class SchedulerImportPojo {

	public static final String PROPERTY_CREATE_MODULE_IF_MISSING = "createModuleIfMissing";
	public static final String PROPERTY_ESTABLISH_MODULE_RELATIONSHIP = "establishModuleRelationship";
	private final UUID uid;
	private final UUID project;
	private final SchedulerType schedulerType;
	private final String schedulerVersion;
	private final String identifier;
	private final UUID source;
	private final Instant importedOn;
	private final Map<String, Object> properties;
	@Nullable
	private final String description;
	@Nullable
	private final String importerUsed;

	/**
	 * Constructor
	 *
	 * @param uid           UID
	 * @param project       Project
	 * @param schedulerType Scheduler Type
	 * @param schedulerVersion Scheduler Version
	 * @param identifier    identifier for the import file
	 * @param source        Source UID
	 * @param importerUsed  Importer implementation Used to import the file
	 * @param importedOn    Imported On
	 * @param description   Description of the import
	 * @param properties    Properties of the import
	 */
	public SchedulerImportPojo(final UUID uid, final UUID project, final SchedulerType schedulerType, final String schedulerVersion, final String identifier,
			final UUID source, @Nullable final String importerUsed, final Instant importedOn, @Nullable final String description,
			final Map<String, Object> properties) {
		this.uid = uid;
		this.project = project;
		this.schedulerType = schedulerType;
		this.schedulerVersion = schedulerVersion;
		this.identifier = identifier;
		this.source = source;
		this.importerUsed = importerUsed;
		this.importedOn = importedOn;
		this.description = description;
		this.properties = properties;
	}

	public UUID getUid() {
		return uid;
	}

	public UUID getProject() {
		return project;
	}

	public SchedulerType getSchedulerType() {
		return schedulerType;
	}

	public String getSchedulerVersion() {
		return schedulerVersion;
	}

	public String getIdentifier() {
		return identifier;
	}

	public UUID getSource() {
		return source;
	}

	public Instant getImportedOn() {
		return importedOn;
	}

	@Nullable
	public String getDescription() {
		return description;
	}

	@Nullable
	public String getImporterUsed() {
		return importerUsed;
	}

	public Map<String, Object> getProperties() {
		return properties;
	}

	@Override
	public String toString() {
		return "SchedulerImportPojo{" + "uid=" + uid + ", project=" + project + ", schedulerType=" + schedulerType + ", schedulerVersion='" + schedulerVersion
				+ '\'' + ", identifier='" + identifier + '\'' + ", description='" + description + '\'' + ", importerUsed='" + importerUsed + '\'' + ", source="
				+ source + ", importedOn=" + importedOn + ", properties=" + properties + '}';
	}
}
