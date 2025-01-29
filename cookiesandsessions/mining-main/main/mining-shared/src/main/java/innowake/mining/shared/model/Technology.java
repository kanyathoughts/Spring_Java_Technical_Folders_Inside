/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * The technology associated with a module.
 */
@Entity(name = "TechnologyEnum")
@MiningDataType(name = "Technology")
public enum Technology implements TypeIdentifier {

	COBOL,
	NATURAL,
	PL1("PL/I"),
	JCL,
	ASSEMBLER,
	RESOURCE("LISTCAT"),
	CSD,
	NONE,
	UNKNOWN,
	XML,
	BINARY,
	EASYTRIEVE,
	IMS,
	C,
	CPP,
	VMS,
	ORACLE,
	SQL,
	JAVA,
	ECL,
	CICS,
	CSHARP,
	
	/**
	 * Represent scheduler nodes like CONTROL-M.
	 * We currently support only a very generic scheduler model of EVENTS and PROCESSES linked to executed Modules.
	 */
	SCHEDULER, 
	BASIC,
	VB,
	WINDOWS,
	/** Custom Technology, for client specific categorization. Currently ignored by Discovery & Mining */
	SERVICE,
	MARK4;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(Technology.class);
	
	private final List<String> names;
	
	private Technology() {
		this.names = Collections.emptyList();
	}

	private Technology(final String... names) {
		this.names = Arrays.asList(names);
	}
	
	public List<String> getNames() {
		return names;
	}
	
	/**
	 * Returns the technology given a name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name the technology is associated with.
	 * @return the technology mapped with the name or {@link #UNKNOWN} if no match is found
	 */
	public static Technology fromName(final String name) {
		for (final Technology value : values()) {
			if (value.name().equalsIgnoreCase(name) || value.names.stream().anyMatch(name::equalsIgnoreCase)) {
				return value;
			}
		}
		LOGGER.error("No enum constant " + name + " available as Technology, so it is being defaulted to UNKNOWN.");
		return UNKNOWN;
	}
}
