/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.scheduler;

/**
 * Enum for scheduler types
 */
public enum SchedulerType {
	CONTROL_M("Control-M"),
	CA7("CA7");
	private final String name;

	SchedulerType(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public static SchedulerType fromName(final String name) {
		for (final SchedulerType type : values()) {
			if (type.getName().equalsIgnoreCase(name) || type.name().equalsIgnoreCase(name)) {
				return type;
			}
		}
		throw new IllegalArgumentException("Unknown scheduler type: " + name);
	}
}
