/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * The definedLocation associated with a module.
 */
public enum DefinedLocation {
	PROGRAM("Program"),
	COPYBOOK("Copybook"),
	SUBPROGRAM("SubProgram"),
	SUBROUTINE("SubRoutine"),
	PROCEDURE("Procedure"),
	PACKAGE("Package"),
	BEGIN("Begin");
	
	private static final ConcurrentHashMap<String, DefinedLocation> NAME_TO_DEFINED_LOC_MAP;
	private final List<String> names;
	private final String displayName;
	
	private DefinedLocation(final String displayName) {
		this.names = Collections.emptyList();
		this.displayName = displayName;
	}
	
	static {
		NAME_TO_DEFINED_LOC_MAP = new ConcurrentHashMap<>();
		for (final DefinedLocation value : values()) {
			NAME_TO_DEFINED_LOC_MAP.put(value.name().toUpperCase(), value);
			for (final String name : value.getNames()) {
				NAME_TO_DEFINED_LOC_MAP.put(name.toUpperCase(), value);
			}
		}
	}

	/**
	 * @return the non-technical, user facing name
	 */
	@JsonValue
	public String getDisplayName() {
		return displayName;
	}
	
	/**
	 * Returns the DefinedLocation given a name.
	 *
	 * @param name the name the definedLocation is associated with.
	 * @return the DefinedLocation mapped with the name
	 */
	@JsonCreator
	public static DefinedLocation fromName(final String name) {
		final DefinedLocation type = NAME_TO_DEFINED_LOC_MAP.get(name.toUpperCase());
		if (type != null) {
			return type;
		}
		return PROGRAM;
	}

	/**
	 * Returns the names associated with the {@link Type} constant on which this method is called.
	 *
	 * @return the list of names
	 */
	public List<String> getNames() {
		return names;
	}
}