/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.monitor;

/**
 * Define all the ProgressMonitor Text in one place
 */
public final class ProgressMonitorText {
	/* Task Names */
	public static final String DISCOVERY_CODE_TASK_DISCOVERCODE = "Discover Code";
	public static final String DISCOVERY_CODE_TASK_INITIAL = "Preparing for running Discover Code";
	public static final String DISCOVERY_CODE_TASK_PRECATEGORIZE = "Precategorizing...";
	public static final String DISCOVERY_CODE_TASK_CATEGORIZE = "Categorizing...";
	public static final String DISCOVERY_CODE_TASK_PREPARE_FOR_DETECTION = "(0/5) Prepare for detection";
	public static final String DISCOVERY_CODE_TASK_MAINTYPE = "(1/5) Main type detection (1/2)";
	public static final String DISCOVERY_CODE_TASK_MAINTYPE_ALL_AT_ONCE = "(1/5) Main type detection (2/2)";
	public static final String DISCOVERY_CODE_TASK_TOKEN = "(2/5) Token detection";
	public static final String DISCOVERY_CODE_TASK_EXTENSION = "(3/5) Extension detection";
	public static final String DISCOVERY_CODE_TASK_DEPENDENCY = "(4/5) Dependency detection";
	public static final String DISCOVERY_CODE_TASK_UNIDENTIFIED = "(5/5) Check unidentified files";
	public static final String DISCOVERY_CODE_TASK_POSTCATEGORIZE = "Postcategorizing...";
	
	
	public static final String DISCOVERY_METRICS_TASK_DISCOVERMETRICS = "Discover Metrics";
	public static final String DISCOVERY_METRICS_TASK_INITIAL = "Preparing for running Discover Metrics";
	public static final String DISCOVERY_METRICS_TASK_COLLECTING = "Collecting metrics...";
	public static final String DISCOVERY_METRICS_TASK_REPO = "(1/3) Creating Repo";
	public static final String DISCOVERY_METRICS_TASK_COLLECT = "(2/3) Collecting metrics";
	public static final String DISCOVERY_METRICS_TASK_STORE = "(3/3) Storing metrics";
	public static final String DISCOVERY_METRICS_EFFORT_SUMMARY = "Computing effort summary";
	public static final String DISCOVERY_METRICS_DEAD_CODE_METRICS = "Computing dead code metrics";
	public static final String DISCOVERY_METRICS_BACKUP = "Backup the modules' metaData before discover metrics";
	public static final String DISCOVERY_METRICS_RESTORE = "Restoring the modules' metaData after discover metrics";
	
	
	public static final String SOURCE_IMPORT_TASK = "Source import";
	public static final String SOURCE_IMPORT_TASK_DESCRIPTION = "Importing source objects";
	
	/* Subtasks */
	public static final String DISCOVERY_SUBTASK_INITIAL = "Initialize...";
	public static final String DISCOVERY_SUBTASK_START = "Start...";
	
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTMODULES = "Export modules";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTSTATEMENTS = "Export statements";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTDEPENDENCIES = "Export dependencies";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTSQL = "Export SQL";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTERRORS = "Export errors";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTUNDISCOVERED = "Export undiscovered entities";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTDEADCODE = "Export dead code";
	public static final String DISCOVERY_EXPORT_SUBTASK_WRITEFILE = "Write to disk";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTSUMMARY = "Export Summary";
	public static final String DISCOVERY_EXPORT_SUBTASK_PREPAREROWS = "Prepare spreadsheet rows ";
	public static final String DISCOVERY_EXPORT_SUBTASK_EXPORTSOURCES = "Export sources";
	public static final String DISCOVERY_EXPORT_SUBTASK_DELETESOURCES = "Delete exported sources";
	
	public static final String DISCOVERY_METRICS_SUBTASK_PREPARECOBOL = "Prepare Cobol Project...";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTCOBOL = "Collecting COBOL Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTBATCH = "Collecting Batch Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTPL1 = "Collecting PL1 Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTASSEMBLER = "Collecting Assembler Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTNATURAL = "Collecting Natural Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTBINARY = "Collecting Binary Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTCSD = "Collecting CSD Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTEZT = "Collecting Easytrieve Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTC = "Collecting C Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASK_COLLECTIMS = "Collecting IMS Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECTDCL = "Collecting DCL Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECTSQLMOD = "Collecting SQLMOD Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECTCDO = "Collecting CDO Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECTFMS = "Collecting FMS Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECTBASIC = "Collecting BASIC Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECTIFDL = "Collecting IFDL Modules: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECT_SQL_SCRIPT = "Collecting SQL scripts: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECT_VAX_MACRO = "Collecting VAX Macros: ";
	public static final String DISCOVERY_METRICS_SUBTASKS_COLLECT_JAVA = "Collecting JAVA Modules: ";
	
	public static final String DISCOVERY_DNA_TASK_INITIAL = "Process DNA";
	public static final String DISCOVERY_DNA_TASK_COLLECT = "Collect DNA";
	public static final String DISCOVERY_DNA_TASK_CALCULATE_DISTANCES = "Calculate DNA distances";
	public static final String DISCOVERY_DNA_TASK_FIND_COMMUNITIES = "Find communities";
	
	/* private class to avoid instantiation */
	private ProgressMonitorText() {
	    throw new IllegalStateException("Can't instantiate this class");
	}

}
