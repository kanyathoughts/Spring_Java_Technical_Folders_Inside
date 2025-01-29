/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.datapoints;

import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.ModuleType;

import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * This class defines static label mappings that can be retrieved via {@code LabelMappingsController}.
 * <p>
 * The UI uses these mappings when a display value is needed that is different from the actual "raw" value. This is mostly used to provide
 * nice human-readable names for enum constants.
 * </p>
 * <p>
 * To add a label mapping for a datapoint, add the required mappings here in this class and add a new entry to the {@code LabelType} enum here.
 * Then add the {@linkplain Usages#VIEW_MODE "general.viewMode"} usage with the {@linkplain ViewModeAttributes#LABEL_MAPPING "labelMapping"} usage attribute
 * to the datapoint. Set the value of the "labelMapping" attribute to the appropriate {@code LabelType}.
 * </p>
 */
public final class LabelMappings {

	public enum LabelType {
		PROJECT_LABELS,
		TECHNOLOGY_LABELS,
		TYPE_LABELS,
		ANNOTATION_METADATA_REASON_LABELS,
		MODULE_DEPENDENCIES_RELATIONSHIP_LABELS,
		ANNOTATION_STATES,
		DEFINED_LOCATION_LABELS,
		MODULE_TYPE_LABELS
	}

	private static final Map<LabelType, Map<String, String>> LABEL_MAPPINGS;

	static {
		final Map<LabelType, Map<String, String>> labelMappings = new EnumMap<>(LabelType.class);

		final Map<String, String> projectLabels = new HashMap<>();
		projectLabels.put("DISCOVERY", "Discovery");
		projectLabels.put("DISCOVERYLIGHT", "Discovery-Lite");
		projectLabels.put("MINING", "Mining");

		final Map<String, String> technologyLabels = new HashMap<>();
		technologyLabels.put("ASSEMBLER", "Assembler");
		technologyLabels.put("COBOL", "COBOL");
		technologyLabels.put("EASYTRIEVE", "Easytrieve");
		technologyLabels.put("NATURAL", "Natural");
		technologyLabels.put("RESOURCE", "Resource");
		technologyLabels.put("UNKNOWN", "Unknown");
		technologyLabels.put("ECL", "ECL");
		technologyLabels.put("CSD", "CSD");
		technologyLabels.put("IMS", "IMS");
		technologyLabels.put("JCL", "JCL");
		technologyLabels.put("PL1", "PL/I");
		technologyLabels.put("SQL", "SQL");
		technologyLabels.put("XML", "XML");
		technologyLabels.put("BASIC", "Basic");
		technologyLabels.put("NONE", "None");
		technologyLabels.put("BINARY", "Binary");
		technologyLabels.put("C", "C");
		technologyLabels.put("CPP", "C++");
		technologyLabels.put("VMS", "VMS");
		technologyLabels.put("ORACLE", "Oracle");
		technologyLabels.put("JAVA", "Java");
		technologyLabels.put("SCHEDULER", "Scheduler");
		technologyLabels.put("SCRIPT", "Script");
		technologyLabels.put("UNDISCOVERED", "Undiscovered");
		technologyLabels.put("CICS", "CICS");
		technologyLabels.put("SERVICE", "Service");
		technologyLabels.put("VB", "VB");
		technologyLabels.put("VMS", "VMS");
		technologyLabels.put("WINDOWS", "Windows");
		technologyLabels.put("MARK4", "MARK IV");
		technologyLabels.put("CSHARP", "C#");


		final Map<String, String> typeLabels = new HashMap<>();
		typeLabels.put("ADAPTER", "Adapter");
		typeLabels.put("ADAPTVIEW", "Adaptview");
		typeLabels.put("ALT_PCB", "ALT PCB");
		typeLabels.put("ASSEMBLER", "Assembler");
		typeLabels.put("APPLICATION", "Application");
		typeLabels.put("BINARY", "Binary");
		typeLabels.put("BMS_MAP", "BMS Map");
		typeLabels.put("BMS_MAPSET", "BMS Mapset");
		typeLabels.put("C", "C");
		typeLabels.put("CDO_FILE", "CDO File");
		typeLabels.put("CDO_RECORD", "CDO Record");
		typeLabels.put("CFG", "CFG");
		typeLabels.put("CLASS", "Class");
		typeLabels.put("COBOL", "COBOL");
		typeLabels.put("CONTROLCARD", "Controlcard");
		typeLabels.put("COPYBOOK", "Copybook");
		typeLabels.put("COPYCODE", "Copycode");
		typeLabels.put("COPYLIB", "Copylib");
		typeLabels.put("COPYPROC", "Copyproc");
		typeLabels.put("CPM", "CPM");
		typeLabels.put("CSD", "CSD");
		typeLabels.put("DBD", "DBD");
		typeLabels.put("DBD_COMPRTN", "DBD Comprtn");
		typeLabels.put("DBD_DATASET", "DBD Dataset");
		typeLabels.put("DCL", "DCL");
		typeLabels.put("DDM", "DDM");
		typeLabels.put("DIALOG", "Dialog");
		typeLabels.put("DIALOG_PRIV_RES", "Dialog Priv Res");
		typeLabels.put("EASYTRIEVE", "Easytrieve");
		typeLabels.put("ERROR_MESSAGE", "Error Message");
		typeLabels.put("EXEC", "Exec");
		typeLabels.put("EXEC_PGM", "Exec PGM");
		typeLabels.put("EXPORT", "Export");
		typeLabels.put("EXTRACT", "Extract");
		typeLabels.put("FILE", "File");
		typeLabels.put("TPFDF_DATASET", "TPFDF Dataset");
		typeLabels.put("FMS_FORM", "FMS Form");
		typeLabels.put("IFDL_FORM", "IFDL Form");
		typeLabels.put("FUNCTION", "Function");
		typeLabels.put("GDA", "GDA");
		typeLabels.put("HDAMPARM", "HDAMPARM");
		typeLabels.put("HEADER", "Header");
		typeLabels.put("HELP", "Help");
		typeLabels.put("INCLUDE", "Include");
		typeLabels.put("INFO", "Info");
		typeLabels.put("INLINE_PROC", "Inline Proc");
		typeLabels.put("INSTREAM", "Instream");
		typeLabels.put("JCL", "JCL");
		typeLabels.put("JOB", "Job");
		typeLabels.put("LDA", "LDA");
		typeLabels.put("LIST", "List");
		typeLabels.put("LISTCAT", "Listcat");
		typeLabels.put("MACRO_FILE", "Macro File");
		typeLabels.put("MACRO", "Macro");
		typeLabels.put("MAINPROGRAM", "Main Program");
		typeLabels.put("MAP", "Map");
		typeLabels.put("MFS", "MFS");
		typeLabels.put("NATURAL", "Natural");
		typeLabels.put("NONE", "None");
		typeLabels.put("OBJECT", "Object");
		typeLabels.put("PACKAGE", "Package");
		typeLabels.put("PCB", "PCB");
		typeLabels.put("PDA", "PDA");
		typeLabels.put("PSB", "PSB");
		typeLabels.put("PGM", "PGM");
		typeLabels.put("PL1", "PL/I");
		typeLabels.put("PROC", "Proc");
		typeLabels.put("PROGRAM", "Program");
		typeLabels.put("RDB_DATABASE", "RDB Database");
		typeLabels.put("SCRIPT", "Script");
		typeLabels.put("SQLMOD", "SQLMOD");
		typeLabels.put("SQLMOD_PROCEDURE", "SQLMOD Procedure");
		typeLabels.put("STORED_PROCEDURE", "Stored Procedure");
		typeLabels.put("SUBPROGRAM", "Subprogram");
		typeLabels.put("SUBROUTINE", "Subroutine");
		typeLabels.put("SYNONYM", "Synonym");
		typeLabels.put("TABLE", "Table");
		typeLabels.put("TDFXTRCT", "TDFXTRCT");
		typeLabels.put("TDQ", "TDQ");
		typeLabels.put("TEXT", "Text");
		typeLabels.put("TRANSACTION", "Transaction");
		typeLabels.put("TSQ", "TSQ");
		typeLabels.put("UNDISCOVERED", "Undiscovered");
		typeLabels.put("UNKNOWN", "Unknown");
		typeLabels.put("UTILITY", "Utility");
		typeLabels.put("VAX_MACRO", "VAX Macro");
		typeLabels.put("VAX_MACRO_ENTRY", "VAX Macro Entry");
		typeLabels.put("VSAM_FILE", "VSAM File");
		typeLabels.put("RECORD", "Record");
		typeLabels.put("COMPILATION_UNIT", "Compilation Unit");
		typeLabels.put("PACK-AGE", "Package");
		typeLabels.put("INTERFACE", "Interface");
		typeLabels.put("TYPE", "Type");
		typeLabels.put("ENUM", "Enum");
		typeLabels.put("ANNOTATION", "Annotation");
		typeLabels.put("METHOD", "Method");
		typeLabels.put("XML", "XML");
		typeLabels.put("ECL", "ECL");
		typeLabels.put("ECL_JOB", "ECL Job");
		typeLabels.put("EVENT", "Event");
		typeLabels.put("PROCESS", "Process");
		typeLabels.put("LIB", "Library");
		typeLabels.put("ACTIVEX_DOCUMENT", "ActiveX Document");
		typeLabels.put("WORKSPACE", "Workspace");
		typeLabels.put("XHTML", "XHTML");
		typeLabels.put("DESIGNER_FILE", "Designer File");
		typeLabels.put("DLL", "DLL");
		typeLabels.put("FORM", "Form");
		typeLabels.put("JSP", "JSP");
		typeLabels.put("JSF", "JSF");
		typeLabels.put("PROJECT", "Project");
		typeLabels.put("OCX", "OCX");
		typeLabels.put("MID", "MID");
		typeLabels.put("MOD", "MOD");
		typeLabels.put("MODULE", "Module");
		typeLabels.put("DBD_SEGMENT", "DBD Segment");
		typeLabels.put("INDEX", "Index");
		typeLabels.put("USER_CONTROL", "User Control");
		typeLabels.put("TRIGGER", "Trigger");
		typeLabels.put("SCHEMA", "Schema");
		typeLabels.put("SERVICE_REQUEST_ID", "Service Request ID");
		typeLabels.put("VIEW", "View");
		typeLabels.put("GDG_FILE", "GDG File");
		typeLabels.put("RAP_CONTROLCARD", "RAP Controlcard");
		typeLabels.put("ARCHDEF", "ARCHDEF");
		typeLabels.put("CBLTDLI", "CBLTDLI");
		typeLabels.put("DLITCBL", "DLITCBL");
		typeLabels.put("AIBTDLI", "AIBTDLI");
		typeLabels.put("CA_TELON", "CA TELON");
		typeLabels.put("URI", "URI");
		typeLabels.put("URL", "URL");
		typeLabels.put("SOLUTION", "Solution");

		final Map<String, String> reasonLabels = new HashMap<>();
		for (final AnnotationMetaDataReasonEnum reason : AnnotationMetaDataReasonEnum.values()) {
			reasonLabels.put(reason.name(), reason.getDescription());
		}
		
		final Map<String, String> relationshipLabels = new HashMap<>();
		relationshipLabels.put("CALLS", "Calls");
		relationshipLabels.put("INCLUDES", "Includes");
		relationshipLabels.put("CONTAINS", "Contains Module");
		relationshipLabels.put("REFERENCES", "References");
		relationshipLabels.put("ACCESSES", "Accesses");
		
		final Map<String, String> annotationState = new HashMap<>();
		annotationState.put("IN_ANALYSIS", "In Analysis");
		annotationState.put("APPROVED", "Approved");
		annotationState.put("CANDIDATE", "Candidate");
		annotationState.put("FOR_REVIEW", "For Review");
		annotationState.put("REJECTED", "Rejected");
		annotationState.put("INVALID", "Invalid");

		final Map<String, String> definedLocationLabels = new HashMap<>();
		definedLocationLabels.put("PROGRAM", "Program");
		definedLocationLabels.put("COPYBOOK", "Copybook");
		definedLocationLabels.put("SUBPROGRAM", "SubProgram");
		definedLocationLabels.put("SUBROUTINE", "SubRoutine");
		definedLocationLabels.put("PROCEDURE", "Procedure");
		definedLocationLabels.put("PACKAGE", "Package");
		definedLocationLabels.put("BEGIN", "Begin");

		final Map<String, String> moduleTypeLabels = new HashMap<>();
		for (final ModuleType moduleType : ModuleType.values()) {
			moduleTypeLabels.put(moduleType.name(), technologyLabels.get(moduleType.getTechnology().name()) + " " + typeLabels.get(moduleType.getType().name()));
		}


		labelMappings.put(LabelType.PROJECT_LABELS, Collections.unmodifiableMap(projectLabels));
		labelMappings.put(LabelType.TECHNOLOGY_LABELS, Collections.unmodifiableMap(technologyLabels));
		labelMappings.put(LabelType.TYPE_LABELS, Collections.unmodifiableMap(typeLabels));
		labelMappings.put(LabelType.ANNOTATION_METADATA_REASON_LABELS, Collections.unmodifiableMap(reasonLabels));
		labelMappings.put(LabelType.MODULE_DEPENDENCIES_RELATIONSHIP_LABELS, Collections.unmodifiableMap(relationshipLabels));
		labelMappings.put(LabelType.ANNOTATION_STATES, Collections.unmodifiableMap(annotationState));
		labelMappings.put(LabelType.DEFINED_LOCATION_LABELS, Collections.unmodifiableMap(definedLocationLabels));
		labelMappings.put(LabelType.MODULE_TYPE_LABELS, Collections.unmodifiableMap(moduleTypeLabels));

		LABEL_MAPPINGS = Collections.unmodifiableMap(labelMappings);
	}

	/**
	 * Get all the label mappings.
	 *
	 * @return the map of label mappings
	 */
	public static Map<LabelType, Map<String, String>> getLabelMappings() {
		return LABEL_MAPPINGS;
	}

	private LabelMappings() {
		/* static utility class */
	}
}
