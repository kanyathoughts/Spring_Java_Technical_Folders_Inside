/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * The type associated with a module.
 */
@Entity(name = "TypeEnum")
@MiningDataType(name = "Type")
public enum Type implements TypeIdentifier {

	ADAPTER("NATURAL_ADAPTER"),
	ADAPTVIEW("NATURAL_ADAPTVIEW"),
	ALT_PCB("IMS_ALT_PCB"),
	BMS_MAP("CICS_BMS_MAP"),
	BMS_MAPSET("CICS_BMS_MAPSET"),
	CDO_FILE,
	CDO_RECORD,
	CFG("JCL_CFG"),
	CLASS("NATURAL_CLASS", "VB_CLASS"),
	CONTROLCARD("JCL_CONTROLCARD"),
	COPYBOOK("COBOL_COPYBOOK", "PL1_COPYBOOK"),
	COPYCODE("NATURAL_COPYCODE", "NATURAL_COPYCODE_REPORTING"),
	COPYLIB("COBOL_COPYLIB"),
	COPYPROC("COBOL_COPYPROC"),
	CPM("NATURAL_CPM"),
	DBD("IMS_DBD"),
	DBD_COMPRTN("IMS_DBD_COMPRTN"),
	DBD_DATASET("IMS_DBD_DATASET"),
	DBD_SEGMENT("IMS_DBD_SEGMENT"),
	DCL,
	DDM("NATURAL_DDM"),
	DIALOG("NATURAL_DIALOG"),
	DIALOG_PRIV_RES("NATURAL_DIALOG_PRIV_RES"),
	ERROR_MESSAGE("NATURAL_ERROR_MESSAGE"),
	EXEC("JCL_EXEC"),
	EXEC_PGM("JCL_EXEC_PGM"),
	EXTRACT("CSD_EXTRACT"),
	FILE("CSD_FILE", "RESOURCE_FILE"),
	TPFDF_DATASET("RESOURCE_TPFDF_DATASET"),
	FMS_FORM,
	IFDL_FORM,
	FUNCTION("NATURAL_FUNCTION", "NATURAL_FUNCTION_REPORTING", "PL1_FUNCTION", "BASIC_FUNCTION", "C_FUNCTION"),
	GDA("NATURAL_GDA", "NATURAL_IW_GDA"),
	HDAMPARM("IMS_HDAMPARM"),
	HEADER("C_HEADER", "CPP_HEADER"),
	HELP("NATURAL_HELP", "NATURAL_HELP_REPORTING", "IMS_HELPTXT"),
	INCLUDE("JCL_INCLUDE"),
	INDEX("SQL_INDEX"),
	INFO("JCL_INFO"),
	INLINE_PROC("JCL_INLINE_PROC"),
	INSTREAM("EASYTRIEVE_INSTREAM"),
	JOB("JCL_JOB"),
	LDA("NATURAL_LDA", "NATURAL_IW_LDA"),
	LIST("CSD_LIST"),
	LISTCAT("RESOURCE_LISTCAT"),
	MACRO("ASSEMBLER_MACRO"),
	MACRO_FILE("EASYTRIEVE_MACRO_FILE"),
	MAINPROGRAM("PL1_MAINPROGRAM"),
	MAP("NATURAL_MAP", "NATURAL_MAP_REPORTING"),
	MFS("IMS_MFS"),
	OBJECT("BASIC_OBJECT"),
	PCB("IMS_PCB"),
	PDA("NATURAL_PDA", "NATURAL_IW_PDA"),
	PSB("IMS_PSB"),
	PGM("JCL_PGM"),
	PROC("JCL_PROC"),
	PROGRAM("COBOL_PROGRAM", "NATURAL_PROGRAM", "NATURAL_PROGRAM_REPORTING", "CSD_PROGRAM", "PL1_PROGRAM", "EASYTRIEVE_PROGRAM", "C_PROGRAM", "BASIC_PROGRAM",
			"CPP_PROGRAM", "ASSEMBLER_PROGRAM"),
	RDB_DATABASE("RDB_DATABASE"),
	SCHEMA("SQL_SCHEMA"),
	SCRIPT("SQL_SCRIPT"),
	SQLMOD,
	SQLMOD_PROCEDURE("SQLMOD_PROC"),
	STORED_PROCEDURE("SQL_STORED_PROCEDURE"),
	SUBPROGRAM("NATURAL_SUBPROGRAM", "NATURAL_SUBPROGRAM_REPORTING"),
	SUBROUTINE("NATURAL_SUBROUTINE", "NATURAL_SUBROUTINE_REPORTING", "PL1_SUBROUTINE", "BASIC_SUBROUTINE"),
	SYNONYM("SQL_SYNONYM"),
	TABLE("SQL_TABLE", "SQL_TEMPORARY_TABLE"),
	TDFXTRCT("IMS_TDFXTRCT"),
	TDQ("CICS_TDQ"),
	TEXT("NATURAL_TEXT"),
	TRANSACTION("CSD_TRANSACTION", "IMS_SYSGEN_TRANSACTION"),
	TRIGGER("SQL_TRIGGER"),
	TSQ("CICS_TSQ"),
	UNKNOWN,
	GDG_FILE("RESOURCE_GDG_FILE"),
	UTILITY,
	VAX_MACRO,
	VAX_MACRO_ENTRY,
	VIEW("SQL_VIEW"),
	VSAM_FILE("RESOURCE_VSAM_FILE"),
	COMPILATION_UNIT("JAVA_COMPILATION_UNIT", "CSHARP_COMPILATION_UNIT"),
	PACKAGE("JAVA_PACKAGE"),
	INTERFACE("JAVA_INTERFACE"),
	TYPE("JAVA_TYPE"),
	ENUM("JAVA_ENUM"),
	ANNOTATION("JAVA_ANNOTATION"),
	JSF("JAVA_JSF"),
	JSP("JAVA_JSP"),
	METHOD("JAVA_METHOD"),
	ECL_JOB("ECL_JOB"),
	EXPORT("IMS_SYSGEN_EXPORT"),
	APPLICATION("IMS_SYSGEN_APPLICATION"),
	MID("IMS_MFS_MID"),
	MOD("IMS_MFS_MOD"),
	XHTML("XML_XHTML"),
	MODULE("VB_MODULE"),
	FORM("VB_FORM"),
	USER_CONTROL("VB_CONTROL"),
	PROJECT("VB_PROJECT", "CSHARP_PROJECT"),
	WORKSPACE("VB_WORKSPACE"),
	ACTIVEX_DOCUMENT("VB_ACTIVEX_DOCUMENT"),
	DESIGNER_FILE("VB_DESIGNER_FILE"),
	DLL("WINDOWS_DLL"),
	OCX("WINDOWS_OCX"),
	LIB("RESOURCE_LIB"),
	RECORD("RESOURCE_RECORD"),
	SOLUTION("CSHARP_SOLUTION"),

	/**
	 * A event is a scheduler starting point.
	 * For example time based or by a precondition.
	 * It is used to support a generic scheduler model.
	 */
	EVENT,

	/**
	 * A process is started by a event.
	 * Processes could CALL other processes and CALL other Modules
	 * It is used to support a generic scheduler model.
	 */
	PROCESS,
	/** Custom Type, for client specific categorization. Currently ignored by Discovery & Mining */
	SERVICE_REQUEST_ID("SERVICE_REQUEST_ID"),
	RAP_CONTROLCARD("RAP_CONTROLCARD");

	
	private static final ConcurrentHashMap<String, Type> NAME_TO_TYPE_MAP;
	static {
		NAME_TO_TYPE_MAP = new ConcurrentHashMap<>();
		for (final Type value : values()) {
			NAME_TO_TYPE_MAP.put(value.name().toUpperCase(), value);
			for (final String name : value.getNames()) {
				NAME_TO_TYPE_MAP.put(name.toUpperCase(), value);
			}
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(Type.class);

	private final List<String> names;

	/**
	 * Returns the type given a name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name the type is associated with.
	 * @return the type mapped with the name or {@link #UNKNOWN} if no match is found
	 */
	public static Type fromName(final String name) {
		final Type type = NAME_TO_TYPE_MAP.get(name.toUpperCase());
		if (type != null) {
			return type;
		}

		LOGGER.error("No enum constant " + name + " available as Type, so it is being defaulted to UNKNOWN.");
		return UNKNOWN;
	}

	private Type() {
		this.names = Collections.emptyList();
	}

	private Type(final String... names) {
		this.names = Arrays.asList(names);
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
