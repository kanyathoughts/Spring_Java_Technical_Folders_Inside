/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.discovery.Tuple2;

import java.util.concurrent.ConcurrentHashMap;

/**
 * Constant definitions for the technical Taxonomy context.
 */
public final class TechnicalTaxonomies {
	
	public static Tuple2<Name, TypeName> tuple(Name name, TypeName type) {
		return new Tuple2<>(name, type);
	}
	
	private TechnicalTaxonomies() {}
	
	/**
	 * The names of the technical Taxonomies.
	 */
	public enum Name {
		UI("UI"),
		BATCH("Batch"),
		MQ("MQ"),
		READ("Read"),
		WRITE("Write"),
		STORE("Store"),
		UPDATE("Update"),
		DELETE("Delete"),
		LIBRARY("Library"),
		LOADS_TABLE("Loads Table"),
		UNLOADS_TABLE("Unloads Table"),
		GENERATES_REPORT("Generates Report"),
		RUNS_JAVA_PROGRAM("Runs JAVA Program"),
		ACCESS_UNIX_FILES("Access UNIX Files"),
		SENDS_EMAIL("Sends Email"),
		FTP("FTP"),
		NETWORK_DATA_MOVER("Network Data Mover"),
		MERGER("Merger"),
		SPLITTER("Splitter"),
		COMMON_INTERFACE("Common Interface"),
		COMMON_LOGGING("Common Logging"),
		COMMON_TRANSACTION("Common Transaction"),
		UNKNOWN("");
	
		private final String displayName;
		private static final ConcurrentHashMap<String, Name> NAME_TO_ENUM_MAP;

		static {
			NAME_TO_ENUM_MAP = new ConcurrentHashMap<>();
			for (final Name value : values()) {
				NAME_TO_ENUM_MAP.put(value.displayName, value);
			}
		}
	
		Name(final String name) {
			displayName = name;
		}
	
		/**
		 * @return the display name
		 */
		public String getDisplayName() {
			return displayName;
		}

		/**
		 * Returns the name for given a display name.
		 *
		 * @param displayName the display name the name is associated with.
		 * @return the name mapped with the display name or {@link #UNKNOWN} if no match is found
		 */
		public static Name fromName(final String displayName) {
			final Name name = NAME_TO_ENUM_MAP.get(displayName);
			return name != null ? name : UNKNOWN;
		}
	}

	/**
	 * The names of the technical Taxonomies types.
	 */
	public enum TypeName {
		PROGRAM_TYPE("Program Type"),
		FILE_ACCESS("File Access"),
		DB_ACCESS("DB Access"),
		OPERATIONS("Operations"),
		UTILITY("Utility"),
		UNKNOWN("");
		
		private final String displayName;
		private static final ConcurrentHashMap<String, TypeName> NAME_TO_ENUM_MAP;

		static {
			NAME_TO_ENUM_MAP = new ConcurrentHashMap<>();
			for (final TypeName value : values()) {
				NAME_TO_ENUM_MAP.put(value.displayName, value);
			}
		}
		
		TypeName(final String name) {
			displayName = name;
		}
		
		/**
		 * @return the display name
		 */
		public String getDisplayName() {
			return displayName;
		}

		/**
		 * Returns the type for given a name.
		 *
		 * @param displayName the name the type is associated with.
		 * @return the type mapped with the name or {@link #UNKNOWN} if no match is found
		 */
		public static TypeName fromName(final String displayName) {
			final TypeName name = NAME_TO_ENUM_MAP.get(displayName);
			return name != null ? name : UNKNOWN;
		}
	}

}
