/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

/**
 * Enum for scheduler entry types.
 */
public enum SchedulerEntryType {
	TABLE("table"),
	FOLDER("folder"),
	JOB("job"),
	CONDITION("condition"),
	UNKNOWN("unknown");

	private final String value;

	SchedulerEntryType(final String value) {
		this.value = value;
	}

	public String getValue() {
		return value;
	}

	/**
	 * Returns the enum value for the given name. If not found, returns UNKNOWN.
	 *
	 * @param name the name
	 * @return the enum value
	 */
	public static SchedulerEntryType fromName(final String name) {
		for (final SchedulerEntryType value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}
		return UNKNOWN;
	}
}
