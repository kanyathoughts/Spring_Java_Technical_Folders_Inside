/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.springdata;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import innowake.mining.shared.model.TypeIdentifier;
import innowake.spring.data.orientdb.api.annotations.Entity;

/**
 * The type associated with a module.
 */
@Entity(name = "TypeEnum")
public enum SampleEnum {
	
	/**
	 * ANNOTATION.
	 */
	ANNOTATION(Arrays.asList("JAVA_ANNOTATION")),
	
	/**
	 * ADAPTER.
	 */
	ADAPTER(Arrays.asList("NATURAL_ADAPTER")),
	/**
	 * ADAPTVIEW.
	 */
	ADAPTVIEW(Arrays.asList("NATURAL_ADAPTVIEW")),
	/**
	 * ALT_PCB.
	 */
	ALT_PCB(Arrays.asList("IMS_ALT_PCB")),
	/**
	 * ASSEMBLER.
	 */
	ASSEMBLER,
	/**
	 * BINARY.
	 */
	BINARY,
	/**
	 * BMS_MAP.
	 */
	BMS_MAP(Arrays.asList("COBOL_BMS_MAP")),
	/**
	 * BMS_MAPSET.
	 */
	BMS_MAPSET(Arrays.asList("COBOL_BMS_MAPSET")),
	/**
	 * C.
	 */
	C,
	/**
	 * CDO_FILE.
	 */
	CDO_FILE,
	/**
	 * CDO_RECORD.
	 */
	CDO_RECORD,
	/**
	 * CFG.
	 */
	CFG(Arrays.asList("JCL_CFG")),
	/**
	 * CLASS.
	 */
	CLASS(Arrays.asList("NATURAL_CLASS")),
	/**
	 * COBOL.
	 */
	COBOL,
	/**
	 * CONTROLCARD.
	 */
	CONTROLCARD(Arrays.asList("JCL_CONTROLCARD")),
	/**
	 * COPYBOOK.
	 */
	COPYBOOK(Arrays.asList("COBOL_COPYBOOK", "PL1_COPYBOOK")),
	/**
	 * COPYCODE.
	 */
	COPYCODE(Arrays.asList("NATURAL_COPYCODE", "NATURAL_COPYCODE_REPORTING")),
	/**
	 * COMPILATION UNIT.
	 */
	COMPILATION_UNIT(Arrays.asList("JAVA_COMPILATION_UNIT")),
	/**
	 * PACKAGE.
	 */
	PACKAGE(Arrays.asList("JAVA_PACKAGE")),
	/**
	 * INTERFACE.
	 */
	INTERFACE(Arrays.asList("JAVA_INTERFACE")),
	/**
	 * TYPE.
	 */
	TYPE(Arrays.asList("JAVA_TYPE")),
	/**
	 * ENUM.
	 */
	ENUM(Arrays.asList("JAVA_ENUM")),
	/**
	 * CPM.
	 */
	CPM(Arrays.asList("NATURAL_CPM")),
	/**
	 * CSD.
	 */
	CSD,
	/**
	 * DATABASE.
	 */
	DATABASE(Arrays.asList("RESOURCE_DATABASE")),
	/**
	 * DBD.
	 */
	DBD(Arrays.asList("IMS_DBD")),
	/**
	 * DBD_COMPRTN.
	 */
	DBD_COMPRTN(Arrays.asList("IMS_DBD_COMPRTN")),
	/**
	 * DBD_DATASET.
	 */
	DBD_DATASET(Arrays.asList("IMS_DBD_DATASET")),
	/**
	 * DCL.
	 */
	DCL,
	/**
	 * DDM.
	 */
	DDM(Arrays.asList("NATURAL_DDM")),
	/**
	 * DIALOG.
	 */
	DIALOG(Arrays.asList("NATURAL_DIALOG")),
	/**
	 * DIALOG_PRIV_RES.
	 */
	DIALOG_PRIV_RES(Arrays.asList("NATURAL_DIALOG_PRIV_RES")),
	/**
	 * EASYTRIEVE.
	 */
	EASYTRIEVE,
	/**
	 * ERROR_MESSAGE.
	 */
	ERROR_MESSAGE(Arrays.asList("NATURAL_ERROR_MESSAGE")),
	/**
	 * EXEC.
	 */
	EXEC(Arrays.asList("JCL_EXEC")),
	/**
	 * EXEC_PGM.
	 */
	EXEC_PGM(Arrays.asList("JCL_EXEC_PGM")),
	/**
	 * EXTRACT.
	 */
	EXTRACT(Arrays.asList("CSD_EXTRACT")),
	/**
	 * FILE.
	 */
	FILE(Arrays.asList("CSD_FILE", "RESOURCE_FILE")),
	/**
	 * FMS_FORM.
	 */
	FMS_FORM,
	/**
	 * IFDL_FORM.
	 */
	IFDL_FORM,
	/**
	 * FUNCTION.
	 */
	FUNCTION(Arrays.asList("NATURAL_FUNCTION", "NATURAL_FUNCTION_REPORTING", "PL1_FUNCTION", "BASIC_FUNCTION", "C_FUNCTION")),
	/**
	 * GDA.
	 */
	GDA(Arrays.asList("NATURAL_GDA", "NATURAL_IW_GDA")),
	/**
	 * HDAMPARM.
	 */
	HDAMPARM(Arrays.asList("IMS_HDAMPARM")),
	/**
	 * HEADER.
	 */
	HEADER(Arrays.asList("C_HEADER")),
	/**
	 * HELP.
	 */
	HELP(Arrays.asList("NATURAL_HELP", "NATURAL_HELP", "IMS_HELPTXT")),
	/**
	 * INCLUDE.
	 */
	INCLUDE(Arrays.asList("JCL_INCLUDE")),
	/**
	 * INFO.
	 */
	INFO(Arrays.asList("JCL_INFO")),
	/**
	 * INLINE_PROC.
	 */
	INLINE_PROC(Arrays.asList("JCL_INLINE_PROC")),
	/**
	 * INSTREAM.
	 */
	INSTREAM(Arrays.asList("EASYTRIEVE_INSTREAM")),
	/**
	 * JCL.
	 */
	JCL,
	/**
	 * JOB.
	 */
	JOB(Arrays.asList("JCL_JOB")),
	/**
	 * LDA.
	 */
	LDA(Arrays.asList("NATURAL_LDA", "NATURAL_IW_LDA")),
	/**
	 * LIST.
	 */
	LIST(Arrays.asList("CSD_LIST")),
	/**
	 * LISTCAT.
	 */
	LISTCAT(Arrays.asList("RESOURCE_LISTCAT")),
	/**
	 * MACRO_FILE.
	 */
	MACRO_FILE(Arrays.asList("EASYTRIEVE_MACRO_FILE")),
	/**
	 * MAINPROGRAM.
	 */
	MAINPROGRAM(Arrays.asList("PL1_MAINPROGRAM")),
	/**
	 * MAP.
	 */
	MAP(Arrays.asList("NATURAL_MAP", "NATURAL_MAP_REPORTING")),
	/**
	 * MFS.
	 */
	MFS(Arrays.asList("COBOL_MFS")),
	/**
	 * NATURAL.
	 */
	NATURAL,
	/**
	 * NONE.
	 */
	NONE,
	/**
	 * OBJECT.
	 */
	OBJECT(Arrays.asList("BASIC_OBJECT")),
	/**
	 * PCB.
	 */
	PCB(Arrays.asList("IMS_PCB")),
	/**
	 * PDA.
	 */
	PDA(Arrays.asList("NATURAL_PDA", "NATURAL_IW_PDA")),
	/**
	 * PSB.
	 */
	PSB(Arrays.asList("IMS_PSB")),
	/**
	 * PGM.
	 */
	PGM(Arrays.asList("JCL_PGM")),
	/**
	 * PL1.
	 */
	PL1,
	/**
	 * PROC.
	 */
	PROC(Arrays.asList("JCL_PROC")),
	/**
	 * PROGRAM.
	 */
	PROGRAM(Arrays.asList("COBOL_PROGRAM", "NATURAL_PROGRAM", "NATURAL_PROGRAM_REPORTING", "CSD_PROGRAM", 
			              "PL1_PROGRAM", "EASYTRIEVE_PROGRAM", "C_PROGRAM", "BASIC_PROGRAM", "ASSEMBLER_PROGRAM")),
	/**
	 * SCRIPT.
	 */
	SCRIPT(Arrays.asList("SQL_SCRIPT")),
	/**
	 * SQLMOD.
	 */
	SQLMOD,
	/**
	 * SQLMOD_PROCEDURE.
	 */
	SQLMOD_PROCEDURE(Arrays.asList("SQLMOD_PROC")),
	/**
	 * STORED_PROCEDURE.
	 */
	STORED_PROCEDURE(Arrays.asList("STORED_PROCEDURE")),
	/**
	 * SUBPROGRAM.
	 */
	SUBPROGRAM(Arrays.asList("NATURAL_SUBPROGRAM", "NATURAL_SUBPROGRAM_REPORTING")),
	/**
	 * SUBROUTINE.
	 */
	SUBROUTINE(Arrays.asList("NATURAL_SUBROUTINE", "NATURAL_SUBROUTINE_REPORTING", "PL1_SUBROUTINE", "BASIC_SUBROUTINE")),
	/**
	 * TABLE.
	 */
	TABLE(Arrays.asList("RESOURCE_TABLE")),
	/**
	 * TDFXTRCT.
	 */
	TDFXTRCT(Arrays.asList("IMS_TDFXTRCT")),
	/**
	 * TDQ.
	 */
	TDQ(Arrays.asList("RESOURCE_TDQ")),
	/**
	 * TEXT.
	 */
	TEXT(Arrays.asList("NATURAL_TEXT")),
	/**
	 * TRANSACTION.
	 */
	TRANSACTION(Arrays.asList("CSD_TRANSACTION")),
	/**
	 * TSQ.
	 */
	TSQ(Arrays.asList("RESOURCE_TSQ")),
	/**
	 * UNDISCOVERED.
	 */
	UNDISCOVERED,
	/**
	 * UNKNOWN.
	 */
	UNKNOWN,
	/**
	 * URI.
	 */
	URI(Arrays.asList("RESOURCE_URI")),
	/**
	 * URL.
	 */
	URL(Arrays.asList("RESOURCE_URL")),
	/**
	 * UTILITY.
	 */
	UTILITY,
	/**
	 * VAX_MACRO.
	 */
	VAX_MACRO,
	/**
	 * VAX_MACRO_ENTRY.
	 */
	VAX_MACRO_ENTRY,
	/**
	 * XML.
	 */
	XML,
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
	PROCESS;
}
