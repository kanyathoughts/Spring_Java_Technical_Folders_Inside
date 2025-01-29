/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Language-specific Module types and mapping to generic Technology/Type pairs.
 */
public enum ModuleType {

	/* UNKNOWN */
	UNKNOWN(Technology.UNKNOWN, Type.UNKNOWN),
	UNKNOWN_UTILITY(Technology.UNKNOWN, Type.UTILITY),
	UNKNOWN_PROGRAM(Technology.UNKNOWN, Type.PROGRAM),
	
	/* ASSEMBLER */
	ASSEMBLER_PROGRAM(Technology.ASSEMBLER, Type.PROGRAM),
	ASSEMBLER_MACRO(Technology.ASSEMBLER, Type.MACRO),
	
	/* BASIC */
	BASIC_PROGRAM(Technology.BASIC, Type.PROGRAM),
	BASIC_SUBROUTINE(Technology.BASIC, Type.SUBROUTINE),
	BASIC_FUNCTION(Technology.BASIC, Type.FUNCTION),
	BASIC_OBJECT(Technology.BASIC, Type.OBJECT),
	
	/* BINARY */
	BINARY(Technology.BINARY, Type.UNKNOWN),
	
	/* C */
	C_PROGRAM(Technology.C, Type.PROGRAM),
	C_HEADER(Technology.C, Type.HEADER),
	C_FUNCTION(Technology.C, Type.FUNCTION),
	
	/* CPP */
	CPP_PROGRAM(Technology.CPP, Type.PROGRAM),
	CPP_HEADER(Technology.CPP, Type.HEADER),
	
	/* COBOL */
	COBOL_PROGRAM(Technology.COBOL, Type.PROGRAM),
	COBOL_COPYBOOK(Technology.COBOL, Type.COPYBOOK),
	COBOL_COPYLIB(Technology.COBOL, Type.COPYLIB),
	COBOL_COPYPROC(Technology.COBOL, Type.COPYPROC),
	
	/* CICS */
	CICS_BMS_MAPSET(Technology.CICS, Type.BMS_MAPSET),
	CICS_BMS_MAP(Technology.CICS, Type.BMS_MAP),
	CICS_TSQ(Technology.CICS, Type.TSQ),
	CICS_TDQ(Technology.CICS, Type.TDQ),

	/* CSD */
	CSD_LIST(Technology.CSD, Type.LIST),
	CSD_TRANSACTION(Technology.CSD, Type.TRANSACTION),
	CSD_PROGRAM(Technology.CSD, Type.PROGRAM),
	CSD_FILE(Technology.CSD, Type.FILE),
	CSD_EXTRACT(Technology.CSD, Type.EXTRACT),
	
	/* EASYTRIEVE */
	EASYTRIEVE_PROGRAM(Technology.EASYTRIEVE, Type.PROGRAM),
	EASYTRIEVE_INSTREAM(Technology.EASYTRIEVE, Type.INSTREAM),
	EASYTRIEVE_MACRO_FILE(Technology.EASYTRIEVE, Type.MACRO_FILE),
	
	/* ECL */
	ECL(Technology.ECL, Type.UNKNOWN),
	ECL_JOB(Technology.ECL, Type.ECL_JOB),
	
	/* IMS */
	IMS_DBD(Technology.IMS, Type.DBD),
	IMS_DBD_COMPRTN(Technology.IMS, Type.DBD_COMPRTN),
	IMS_DBD_DATASET(Technology.IMS, Type.DBD_DATASET),
	IMS_DBD_SEGMENT(Technology.IMS, Type.DBD_SEGMENT),
	IMS_HELPTXT(Technology.IMS, Type.HELP),
	IMS_HDAMPARM(Technology.IMS, Type.HDAMPARM),
	IMS_PSB(Technology.IMS, Type.PSB),
	IMS_PCB(Technology.IMS, Type.PCB),
	IMS_ALT_PCB(Technology.IMS, Type.ALT_PCB),
	IMS_SYSGEN_EXPORT(Technology.IMS, Type.EXPORT),
	IMS_SYSGEN_TRANSACTION(Technology.IMS, Type.TRANSACTION),
	IMS_SYSGEN_APPLICATION(Technology.IMS, Type.APPLICATION),
	IMS_MFS(Technology.IMS, Type.MFS),
	IMS_MFS_MOD(Technology.IMS, Type.MOD),
	IMS_MFS_MID(Technology.IMS, Type.MID),
	IMS_TDFXTRCT(Technology.IMS, Type.TDFXTRCT),
	
	/* JAVA */
	JAVA_COMPILATION_UNIT(Technology.JAVA, Type.COMPILATION_UNIT),
	JAVA_PACKAGE(Technology.JAVA, Type.PACKAGE),
	JAVA_INTERFACE(Technology.JAVA, Type.INTERFACE),
	JAVA_TYPE(Technology.JAVA, Type.TYPE),
	JAVA_ENUM(Technology.JAVA, Type.ENUM),
	JAVA_ANNOTATION(Technology.JAVA, Type.ANNOTATION),
	JAVA_JSF(Technology.JAVA, Type.JSF),
	JAVA_JSP(Technology.JAVA, Type.JSP),
	JAVA_METHOD(Technology.JAVA, Type.METHOD),

	/* JCL */
	JCL_JOB(Technology.JCL, Type.JOB),
	JCL_PROC(Technology.JCL, Type.PROC),
	JCL_INLINE_PROC(Technology.JCL, Type.INLINE_PROC),
	JCL_EXEC(Technology.JCL, Type.EXEC),
	JCL_EXEC_PGM(Technology.JCL, Type.EXEC_PGM),
	JCL_PGM(Technology.JCL, Type.PGM),
	JCL_CFG(Technology.JCL, Type.CFG),
	JCL_INFO(Technology.JCL, Type.INFO),
	JCL_CONTROLCARD(Technology.JCL, Type.CONTROLCARD),
	JCL_INCLUDE(Technology.JCL, Type.INCLUDE),
	
	/* NATURAL */
	NATURAL_PROGRAM(Technology.NATURAL, Type.PROGRAM),
	NATURAL_PROGRAM_REPORTING(Technology.NATURAL, Type.PROGRAM),
	NATURAL_SUBROUTINE(Technology.NATURAL, Type.SUBROUTINE),
	NATURAL_SUBROUTINE_REPORTING(Technology.NATURAL, Type.SUBROUTINE),
	NATURAL_SUBPROGRAM(Technology.NATURAL, Type.SUBPROGRAM),
	NATURAL_SUBPROGRAM_REPORTING(Technology.NATURAL, Type.SUBPROGRAM),
	NATURAL_COPYCODE(Technology.NATURAL, Type.COPYCODE),
	NATURAL_COPYCODE_REPORTING(Technology.NATURAL, Type.COPYCODE),
	NATURAL_CLASS(Technology.NATURAL, Type.CLASS),
	NATURAL_CPM(Technology.NATURAL, Type.CPM),
	NATURAL_ADAPTVIEW(Technology.NATURAL, Type.ADAPTVIEW),
	NATURAL_FUNCTION(Technology.NATURAL, Type.FUNCTION),
	NATURAL_FUNCTION_REPORTING(Technology.NATURAL, Type.FUNCTION),
	NATURAL_HELP(Technology.NATURAL, Type.HELP),
	NATURAL_HELP_REPORTING(Technology.NATURAL, Type.HELP),
	NATURAL_ADAPTER(Technology.NATURAL, Type.ADAPTER),
	NATURAL_MAP(Technology.NATURAL, Type.MAP),
	NATURAL_MAP_REPORTING(Technology.NATURAL, Type.MAP),
	NATURAL_DIALOG(Technology.NATURAL, Type.DIALOG),
	NATURAL_DIALOG_PRIV_RES(Technology.NATURAL, Type.DIALOG_PRIV_RES),
	NATURAL_GDA(Technology.NATURAL, Type.GDA),
	NATURAL_IW_GDA(Technology.NATURAL, Type.GDA),
	NATURAL_PDA(Technology.NATURAL, Type.PDA),
	NATURAL_IW_PDA(Technology.NATURAL, Type.PDA),
	NATURAL_LDA(Technology.NATURAL, Type.LDA),
	NATURAL_IW_LDA(Technology.NATURAL, Type.LDA),
	NATURAL_DDM(Technology.NATURAL, Type.DDM),
	NATURAL_TEXT(Technology.NATURAL, Type.TEXT),
	NATURAL_ERROR_MESSAGE(Technology.NATURAL, Type.ERROR_MESSAGE),
	
	/* ORACLE */
	CDO_FILE(Technology.ORACLE, Type.CDO_FILE),
	CDO_RECORD(Technology.ORACLE, Type.CDO_RECORD),
	SQLMOD(Technology.ORACLE, Type.SQLMOD),
	SQLMOD_PROC(Technology.ORACLE, Type.SQLMOD_PROCEDURE),
	RDB_DATABASE(Technology.ORACLE, Type.RDB_DATABASE),
	
	/* PL1 */
	PL1_PROGRAM(Technology.PL1, Type.PROGRAM),
	PL1_MAINPROGRAM(Technology.PL1, Type.MAINPROGRAM),
	PL1_COPYBOOK(Technology.PL1, Type.COPYBOOK),
	PL1_SUBROUTINE(Technology.PL1, Type.SUBROUTINE),
	PL1_FUNCTION(Technology.PL1, Type.FUNCTION),
	
	/* RESOURCE */
	RESOURCE_FILE(Technology.RESOURCE, Type.FILE),
	RESOURCE_TPFDF_DATASET(Technology.RESOURCE, Type.TPFDF_DATASET),
	RESOURCE_LIB(Technology.RESOURCE, Type.LIB),
	RESOURCE_LISTCAT(Technology.RESOURCE, Type.LISTCAT),
	RESOURCE_VSAM_FILE(Technology.RESOURCE, Type.VSAM_FILE),
	RESOURCE_GDG_FILE(Technology.RESOURCE, Type.GDG_FILE),
	RESOURCE_RECORD(Technology.RESOURCE, Type.RECORD),
	
	/* SQL */
	SQL_STORED_PROCEDURE(Technology.SQL, Type.STORED_PROCEDURE),
	SQL_SCRIPT(Technology.SQL, Type.SCRIPT),
	SQL_TABLE(Technology.SQL, Type.TABLE),
	SQL_INDEX(Technology.SQL, Type.INDEX),
	SQL_VIEW(Technology.SQL, Type.VIEW),
	SQL_TRIGGER(Technology.SQL, Type.TRIGGER),
	SQL_SCHEMA(Technology.SQL, Type.SCHEMA),
	SQL_TEMPORARY_TABLE(Technology.SQL, Type.TABLE),
	SQL_SYNONYM(Technology.SQL, Type.SYNONYM),
	/* VMS */
	DCL(Technology.VMS, Type.DCL),
	FMS_FORM(Technology.VMS, Type.FMS_FORM),
	IFDL_FORM(Technology.VMS, Type.IFDL_FORM),
	VAX_MACRO(Technology.VMS, Type.VAX_MACRO),
	VAX_MACRO_ENTRY(Technology.VMS, Type.VAX_MACRO_ENTRY),
	
	/* XML */
	XML(Technology.XML, Type.UNKNOWN),
	XML_XHTML(Technology.XML, Type.XHTML),
	
	/* VB */
	VB_MODULE(Technology.VB, Type.MODULE),
	VB_CLASS(Technology.VB, Type.CLASS),
	VB_FORM(Technology.VB, Type.FORM),
	VB_CONTROL(Technology.VB, Type.USER_CONTROL),
	VB_PROJECT(Technology.VB, Type.PROJECT),
	VB_WORKSPACE(Technology.VB, Type.WORKSPACE),
	VB_ACTIVEX_DOCUMENT(Technology.VB, Type.ACTIVEX_DOCUMENT),
	VB_DESIGNER_FILE(Technology.VB, Type.DESIGNER_FILE),
	/* WINDOWS */
	WINDOWS_DLL(Technology.WINDOWS, Type.DLL),
	WINDOWS_OCX(Technology.WINDOWS, Type.OCX),

	CSHARP_COMPILATION_UNIT(Technology.CSHARP, Type.COMPILATION_UNIT),
	CSHARP_PROJECT(Technology.CSHARP, Type.PROJECT),
	CSHARP_SOLUTION(Technology.CSHARP, Type.SOLUTION),

	/* Custom module type, for client specific categorization. Currently ignored by Discovery & Mining */
	SERVICE(Technology.SERVICE, Type.SERVICE_REQUEST_ID),
	RAP_CONTROLCARD(Technology.JCL, Type.RAP_CONTROLCARD),
	MARK4_PROGRAM(Technology.MARK4, Type.PROGRAM);

	/**
	 * Create a {@code ModuleType} from the corresponding {@link Technology} and {@link Type} pair.
	 *
	 * @param technology the technology
	 * @param type the type
	 * @return the {@code ModuleType} corresponding to this {@code technology} and {@code type}
	 */
	public static ModuleType fromTechnologyAndType(final Technology technology, final Type type) {
		for (final ModuleType moduleType : ModuleType.values()) {
			if (moduleType.getTechnology() == technology && moduleType.getType() == type) {
				return moduleType;
			}
		}
		LOG.error("No ModuleType matches Technology " + technology + " and Type " + type);
		return UNKNOWN;
	}
	
	private final Technology technology;
	private final Type type;
	private static final Logger LOG = LoggerFactory.getLogger(ModuleType.class);
	
	private ModuleType(final Technology technology, final Type type) {
		this.technology = technology;
		this.type = type;
	}
	
	public Technology getTechnology() {
		return technology;
	}
	
	public Type getType() {
		return type;
	}

}
