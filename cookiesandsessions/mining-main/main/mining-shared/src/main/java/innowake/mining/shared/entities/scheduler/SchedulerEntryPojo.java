/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

import java.util.Map;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;

/**
 * POJO for scheduler entry.
 */
public class SchedulerEntryPojo {

	private final UUID uid;
	@Nullable
	private final UUID containedIn;
	@Nullable
	private final UUID module;
	private final String identifier;
	private final UUID schedulerImport;
	private final SchedulerEntryType type;
	private final Map<String, String> content;

	/**
	 * Constructor.
	 *
	 * @param uid            UID
	 * @param containedIn    the entry (parent) that this entry is contained in
	 * @param module         Module the resolved module of the entry if any
	 * @param identifier     name of the entry such as JOB, INCOND, OUTCOND, FOLDER, etc.
	 * @param schedulerImport Scheduler import
	 * @param type           Type of the entry also identifyable using the identifier
	 * @param content        the raw content in key value pairs
	 */
	public SchedulerEntryPojo(final UUID uid, @Nullable final UUID containedIn, @Nullable final UUID module, final String identifier, final UUID schedulerImport,
			final SchedulerEntryType type, final Map<String, String> content) {
		this.uid = uid;
		this.containedIn = containedIn;
		this.module = module;
		this.identifier = identifier;
		this.schedulerImport = schedulerImport;
		this.type = type;
		this.content = content;
	}

	public UUID getUid() {
		return uid;
	}

	@Nullable
	public UUID getContainedIn() {
		return containedIn;
	}

	@Nullable
	public UUID getModule() {
		return module;
	}

	public String getIdentifier() {
		return identifier;
	}

	public UUID getSchedulerImport() {
		return schedulerImport;
	}

	public SchedulerEntryType getType() {
		return type;
	}

	public Map<String, String> getContent() {
		return content;
	}

	@Override
	public String toString() {
		return "SchedulerEntryPojo{" + "uid=" + uid + ", containedIn=" + containedIn + ", module=" + module + ", identifier='" + identifier + '\''
				+ ", schedulerImport=" + schedulerImport + ", type=" + type + ", content=" + content + '}';
	}
}
