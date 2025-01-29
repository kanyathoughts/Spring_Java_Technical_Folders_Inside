/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery.config.core;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.discovery.ResolveTarget.*;

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.lang.StringUtils;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.filter.AntWildcardFilter;
import innowake.mining.shared.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.discovery.config.utility.XmlStep;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Represents the top level entry point into the mapping section of
 * the configuration file.
 */
public class Config implements Serializable {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.CONFIG);

	private static final String LANGUAGE_ENABLE = "ENABLE";

	private static final String RETAIN_FILE_EXTENSION = "RETAIN_FILE_EXTENSION";
	private static final String RETAIN_FILE_EXTENSION_BASIC = "true";

	private static final String RETAIN_FILE_EXTENSION_VB = "true";
	private static final String LANGUAGE_ENABLE_VB = "false";
	
	private static final String RETAIN_FILE_EXTENSION_CPP = "true";

	private static final String UNKNOWN_TOKEN_THRESHOLD = "UNKNOWN_TOKEN_THRESHOLD";
	private static final String UNKNOWN_TOKEN_THRESHOLD_VALUE = "0.2";

	private static final String PRINTABLE_CHAR_THRESHOLD = "PRINTABLE_CHAR_THRESHOLD";
	private static final String PRINTABLE_CHAR_THRESHOLD_VALUE = "0.85";

	private static final String COPYBOOK_PIC_THRESHOLD = "COPYBOOK_PIC_THRESHOLD";
	private static final String COPYBOOK_PIC_THRESHOLD_VALUE = "0.08";

	private static final String CDO_PATTERN_THRESHOLD = "CDO_PATTERN_THRESHOLD";
	private static final String CDO_PATTERN_THRESHOLD_VALUE = "0.10";

	private static final String SQLMOD_PATTERN_THRESHOLD = "SQLMOD_PATTERN_THRESHOLD";
	private static final String SQLMOD_PATTERN_THRESHOLD_VALUE = "0.05";

	private static final String FMS_PATTERN_THRESHOLD = "FMS_PATTERN_THRESHOLD";
	private static final String FMS_PATTERN_THRESHOLD_VALUE = "0.05";

	private static final String DCL_PATTERN_THRESHOLD = "DCL_PATTERN_THRESHOLD";
	private static final String DCL_PATTERN_THRESHOLD_VALUE = "0.2";

	private static final String ECL_PATTERN_THRESHOLD = "ECL_PATTERN_THRESHOLD";
	private static final String ECL_PATTERN_THRESHOLD_VALUE = "0.75";

	private static final String CSD_PARSER_DUMPXML = "CSD_PARSER_DUMPXML";
	private static final String CSD_PARSER_DUMPXML_VALUE = "true";

	private static final String COBOL_PGM_ID_AS_NAME = "COBOL_PGM_ID_AS_NAME";
	private static final String COBOL_PGM_ID_AS_NAME_VALUE = "false";	

	private static final String COBOL_IMS_DEPENDENCY_METRICS_ENABLE = "COBOL_IMS_DEPENDENCY_METRICS_ENABLE";
	private static final String COBOL_IMS_DEPENDENCY_METRICS_ENABLE_VALUE = "false";

	private static final String IMS_DBD_FIELDS_ENABLE = "IMS_DBD_FIELDS_ENABLE";
	private static final String IMS_DBD_FIELDS_ENABLE_VALUE = "false";

	private static final String PL1_PARSER_MARGIN_START = "PL1_PARSER_MARGIN_START";
	private static final String PL1_PARSER_MARGIN_START_VALUE = "2";

	private static final String PL1_PARSER_MARGIN_END = "PL1_PARSER_MARGIN_END";
	private static final String PL1_PARSER_MARGIN_END_VALUE = "72";

	private static final String PL1_PARSER_ANS = "PL1_PARSER_ANS";
	private static final String PL1_PARSER_ANS_VALUE = "-1";

	private static final String PL1_PARSER_NOT_SYMBOL = "PL1_PARSER_NOT_SYMBOL";
	private static final String PL1_PARSER_NOT_SYMBOL_VALUE = "^";

	private static final String PL1_PARSER_OR_SYMBOL = "PL1_PARSER_OR_SYMBOL";
	private static final String PL1_PARSER_OR_SYMBOL_VALUE = "!";

	private static final String DD_IGNORE_DEPENDENCY = "DD_IGNORE_DEPENDENCY";

	private static final String PARSER_TIMEOUT = "PARSER_TIMEOUT";
	private static final String BASIC_PARSER_TIMEOUT_VALUE = "30";
	private static final String VB_PARSER_TIMEOUT_VALUE = "30";
	private static final String COBOL_PARSER_TIMEOUT_VALUE = "30";
	private static final String CSD_PARSER_TIMEOUT_VALUE = "2000";
	private static final String EZT_PARSER_TIMEOUT_VALUE = "30";
	private static final String C_PARSER_TIMEOUT_VALUE = "30";
	private static final String CPP_PARSER_TIMEOUT_VALUE = "30";
	private static final String ECL_PARSER_TIMEOUT_VALUE = "30";
	private static final String JAVA_PARSER_TIMEOUT_VALUE = "30";
	private static final String JCL_PARSER_TIMEOUT_VALUE = "30";
	private static final String NATURAL_PARSER_TIMEOUT_VALUE = "30";
	private static final String PL1_PARSER_TIMEOUT_VALUE = "30";
	private static final String SQL_PARSER_TIMEOUT_VALUE = "30";
	private static final String IMS_PARSER_TIMEOUT_VALUE = "30";

	private static final String NATURAL_DECIMAL_CHARACTER = "DECIMAL_CHARACTER";
	private static final String NATURAL_DECIMAL_CHARACTER_VALUE = ".";
	private static final String NATURAL_INPUT_DELIMITER = "INPUT_DELIMITER";
	private static final String NATURAL_INPUT_DELIMITER_VALUE = ",";
	private static final String NATURAL_LANGUAGE_CODES = "LANGUAGE_CODES";
	private static final String NATURAL_LANGUAGE_CODES_VALUE = "1";

	private static final String EXPORT_CHUNK_SIZE = "EXPORT_CHUNK_SIZE";
	private static final String JAVA_EXPORT_CHUNK_SIZE = "10000";

	private static final String PGM_STEP_NAME = "STEP_NAME";
	private static final String PGM_DD_KEYWORD = "DD_KEYWORD";

	private static final String DEFAULT_UNDISCOVERED_FOLDER = "undiscovered-entities";

	private static final String INCLUDE_PACKAGES = "INCLUDE_PACKAGES";
	private static final String EXCLUDE_PACKAGES = "EXCLUDE_PACKAGES";
	private static final String RETAIN_FILE_EXTENSION_CSHARP = "true";
	private static final String CSHARP_PARSER_TIMEOUT_VALUE = "30";

	private static final String DEFAULT_OUTPUT_FORMAT = OutputFormat.EXCEL.name();
	private enum OutputFormat { EXCEL, CSV, NONE }
	
	private static final ConfigAdapter ADAPTER = new ConfigAdapter();

	private static final List<Property> DEFAULT_PROPERTY = Arrays.asList(
			new Property(ASSEMBLER, new KeyValuePair(UNKNOWN_TOKEN_THRESHOLD, UNKNOWN_TOKEN_THRESHOLD_VALUE)),
			new Property(BASIC, new KeyValuePair(RETAIN_FILE_EXTENSION, RETAIN_FILE_EXTENSION_BASIC),
					new KeyValuePair(PARSER_TIMEOUT, BASIC_PARSER_TIMEOUT_VALUE)),
			new Property(VB, new KeyValuePair(LANGUAGE_ENABLE, LANGUAGE_ENABLE_VB),
					new KeyValuePair(RETAIN_FILE_EXTENSION, RETAIN_FILE_EXTENSION_VB), new KeyValuePair(PARSER_TIMEOUT, VB_PARSER_TIMEOUT_VALUE)),
			new Property(BINARY, new KeyValuePair(PRINTABLE_CHAR_THRESHOLD, PRINTABLE_CHAR_THRESHOLD_VALUE)),
			new Property(COBOL, new KeyValuePair(PARSER_TIMEOUT, COBOL_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(COPYBOOK_PIC_THRESHOLD, COPYBOOK_PIC_THRESHOLD_VALUE),
					new KeyValuePair(COBOL_PGM_ID_AS_NAME, COBOL_PGM_ID_AS_NAME_VALUE),
					new KeyValuePair(COBOL_IMS_DEPENDENCY_METRICS_ENABLE, COBOL_IMS_DEPENDENCY_METRICS_ENABLE_VALUE)),
			new Property(CSD, new KeyValuePair(PARSER_TIMEOUT, CSD_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(CSD_PARSER_DUMPXML, CSD_PARSER_DUMPXML_VALUE)),
			new Property(EASYTRIEVE, new KeyValuePair(PARSER_TIMEOUT, EZT_PARSER_TIMEOUT_VALUE)),
			new Property(ResolveTarget.C, new KeyValuePair(PARSER_TIMEOUT, C_PARSER_TIMEOUT_VALUE)),
			new Property(ResolveTarget.CPP, new KeyValuePair(RETAIN_FILE_EXTENSION, RETAIN_FILE_EXTENSION_CPP), new KeyValuePair(PARSER_TIMEOUT, CPP_PARSER_TIMEOUT_VALUE)),
			new Property(SQL, new KeyValuePair(PARSER_TIMEOUT, SQL_PARSER_TIMEOUT_VALUE)),
			new Property(JCL, new KeyValuePair(PARSER_TIMEOUT, JCL_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(PGM_STEP_NAME, "SYSIN"),
					new KeyValuePair(PGM_STEP_NAME, "SYSTSIN"),
					new KeyValuePair(PGM_STEP_NAME, "INPUT"),
					new KeyValuePair(PGM_DD_KEYWORD, "*"),
					new KeyValuePair(PGM_DD_KEYWORD, "PATH"),
					new KeyValuePair(PGM_DD_KEYWORD, "SUBSYS"),
					new KeyValuePair(DD_IGNORE_DEPENDENCY, "SYSDUMP"),
					new KeyValuePair(DD_IGNORE_DEPENDENCY, "SYSABEND"),
					new KeyValuePair(DD_IGNORE_DEPENDENCY, "SYSMDUMP"),
					new KeyValuePair(DD_IGNORE_DEPENDENCY, "SYSUDUMP"),
					new KeyValuePair(DD_IGNORE_DEPENDENCY, "SYSCHK"),
					new KeyValuePair(DD_IGNORE_DEPENDENCY, "SYSCKEOV")),
			
			new Property(NATURAL, new KeyValuePair(PARSER_TIMEOUT, NATURAL_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(NATURAL_DECIMAL_CHARACTER, NATURAL_DECIMAL_CHARACTER_VALUE),
					new KeyValuePair(NATURAL_INPUT_DELIMITER, NATURAL_INPUT_DELIMITER_VALUE),
					new KeyValuePair(NATURAL_LANGUAGE_CODES, NATURAL_LANGUAGE_CODES_VALUE)),
			new Property(ORACLE, new KeyValuePair(CDO_PATTERN_THRESHOLD, CDO_PATTERN_THRESHOLD_VALUE),
					new KeyValuePair(SQLMOD_PATTERN_THRESHOLD, SQLMOD_PATTERN_THRESHOLD_VALUE)),
			new Property(PL1, new KeyValuePair(PARSER_TIMEOUT, PL1_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(PL1_PARSER_MARGIN_START, PL1_PARSER_MARGIN_START_VALUE),
					new KeyValuePair(PL1_PARSER_MARGIN_END, PL1_PARSER_MARGIN_END_VALUE),
					new KeyValuePair(PL1_PARSER_ANS, PL1_PARSER_ANS_VALUE),
					new KeyValuePair(PL1_PARSER_NOT_SYMBOL, PL1_PARSER_NOT_SYMBOL_VALUE),
					new KeyValuePair(PL1_PARSER_OR_SYMBOL, PL1_PARSER_OR_SYMBOL_VALUE)),
			new Property(VMS, new KeyValuePair(FMS_PATTERN_THRESHOLD, FMS_PATTERN_THRESHOLD_VALUE),
					new KeyValuePair(DCL_PATTERN_THRESHOLD, DCL_PATTERN_THRESHOLD_VALUE)),
			new Property(JAVA, new KeyValuePair(PARSER_TIMEOUT, JAVA_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(EXPORT_CHUNK_SIZE, JAVA_EXPORT_CHUNK_SIZE),
					new KeyValuePair(INCLUDE_PACKAGES, ""),
					new KeyValuePair(EXCLUDE_PACKAGES, "")),
			new Property(ECL, new KeyValuePair(PARSER_TIMEOUT, ECL_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(ECL_PATTERN_THRESHOLD, ECL_PATTERN_THRESHOLD_VALUE)),
			new Property(IMS, new KeyValuePair(PARSER_TIMEOUT, IMS_PARSER_TIMEOUT_VALUE),
					new KeyValuePair(IMS_DBD_FIELDS_ENABLE, IMS_DBD_FIELDS_ENABLE_VALUE)),
			new Property(CSHARP, new KeyValuePair(RETAIN_FILE_EXTENSION, RETAIN_FILE_EXTENSION_CSHARP),
					new KeyValuePair(PARSER_TIMEOUT, CSHARP_PARSER_TIMEOUT_VALUE))
			);

	private static final List<FileDetectionType> DEFAULT_FILE_DETECTION_TYPE =  Arrays.asList(
			new FileDetectionType("MAIN", "1"),
			new FileDetectionType("TOKEN", "2"),
			new FileDetectionType("DEPENDENCY", "3"),
			new FileDetectionType("EXTENSION", "4"));

	private static final Config DEFAULT = new Config(
			/* PROJECT NAME */
			"",
			Arrays.asList(
					/* COBOL */
					new Mapping("**/*", COBOL_PROGRAM, "programs"),
					new Mapping("**/*", COBOL_COPYBOOK, "copies"),
					new Mapping("**/*", COBOL_COPYLIB, "copylibs"),
					/* JCL */
					new Mapping("**/*", JCL_JOB, "jobs"),
					new Mapping("**/*", JCL_PROC, "procs"),
					new Mapping("**/*", JCL_CONTROLCARD, "controlcards"),
					new Mapping("**/*", JCL_INCLUDE, "include"), /* supposedly this is where runtime looks */
					/* ASSEMBLER */
					new Mapping("**/*", ASSEMBLER_MACRO, "macro"),
					new Mapping("**/*", ASSEMBLER_PROGRAM, ""),
					/* XML */
					new Mapping("**/*", XML, ""),
					/* PL1 */
					new Mapping("**/*", PL1_MAINPROGRAM, "programs"),
					new Mapping("**/*", PL1_PROGRAM, "programs"),
					new Mapping("**/*", PL1_COPYBOOK, "copies"),
					/* NATURAL */
					new Mapping("**/*", NATURAL_PROGRAM, "programs"),
					new Mapping("**/*", NATURAL_SUBPROGRAM, "subprograms"),
					new Mapping("**/*", NATURAL_SUBROUTINE, "subroutines"),
					new Mapping("**/*", NATURAL_FUNCTION, "functions"),
					new Mapping("**/*", NATURAL_COPYCODE, "copies"),
					new Mapping("**/*", NATURAL_DDM, "ddms"),
					new Mapping("**/*", NATURAL_HELP, "helproutines"),
					new Mapping("**/*", NATURAL_LDA, "ldas"),
					new Mapping("**/*", NATURAL_IW_LDA, "ldas"),
					new Mapping("**/*", NATURAL_PDA, "pdas"),
					new Mapping("**/*", NATURAL_IW_PDA, "pdas"),
					new Mapping("**/*", NATURAL_GDA, "gdas"),
					new Mapping("**/*", NATURAL_IW_GDA, "gdas"),
					new Mapping("**/*", NATURAL_MAP, "maps"),
					new Mapping("**/*", NATURAL_TEXT, "texts"),
					new Mapping("**/*", NATURAL_ERROR_MESSAGE, "messages"),
					new Mapping("**/*", NATURAL_CLASS, "classes"),
					new Mapping("**/*", NATURAL_CPM, "cpms"),
					new Mapping("**/*", NATURAL_ADAPTER, "adapters"),
					new Mapping("**/*", NATURAL_ADAPTVIEW, "adapters"),
					new Mapping("**/*", NATURAL_DIALOG, "dialogs"),
					new Mapping("**/*", NATURAL_DIALOG_PRIV_RES, "dialogs"),
					/* BINARY */
					new Mapping("**/*", BINARY, ""),
					/* EASYTRIEVE */
					new Mapping("**/*", EASYTRIEVE_PROGRAM, ""),
					new Mapping("**/*", EASYTRIEVE_MACRO_FILE, ""),
					/* C */
					new Mapping("**/*", ResolveTarget.C_PROGRAM, ""),
					new Mapping("**/*", ResolveTarget.C_HEADER, ""),
					
					/* CPP */
					new Mapping("**/*", ResolveTarget.CPP_PROGRAM, ""),
					new Mapping("**/*", ResolveTarget.CPP_HEADER, ""),
					
					/* CSD */
					new Mapping("**/*", CSD_LIST, ""),
					new Mapping("**/*", CSD_EXTRACT, ""),
					/* IMS */
					new Mapping("**/*", IMS_DBD, "dbd"),
					new Mapping("**/*", IMS_HELPTXT, "help"),
					new Mapping("**/*", IMS_HDAMPARM, "dbdh"),
					new Mapping("**/*", IMS_PSB, "psb"),
					new Mapping("**/*", IMS_MFS, "maps"),
					/* DCL */
					new Mapping("**/*", DCL, "dcl"),
					new Mapping("**/*", FMS_FORM, "fms"),
					new Mapping("**/*", IFDL_FORM, "ifdl"),
					/* ORACLE */
					new Mapping("**/*", CDO_FILE, "cdo"),
					new Mapping("**/*", SQLMOD, "sqlmod"),
					/* BASIC */
					new Mapping("**/*", BASIC_OBJECT, ""),
					/* SQL Script */
					new Mapping("**/*", ResolveTarget.SQL_SCRIPT, ""),
					/* VAX Macro */
					new Mapping("**/*", ResolveTarget.VAX_MACRO, "mar"),
					/* NONE */
					new Mapping("**/*", ResolveTarget.NONE, ""),
					new Mapping("**/*", ResolveTarget.JAVA_COMPILATION_UNIT, ""),
					/* ECL */
					new Mapping("**/*", ECL, ""),
					new Mapping("**/*", ResolveTarget.ECL_JOB, "jobs"),
					/* CICS */
					new Mapping("**/*", CICS_BMS_MAPSET, "maps"),
					/*  VB */
					new Mapping("**/*", ResolveTarget.VB_MODULE, "module"),
					new Mapping("**/*", ResolveTarget.VB_CLASS, "class"),
					new Mapping("**/*", ResolveTarget.VB_FORM, "form"),
					new Mapping("**/*", ResolveTarget.VB_CONTROL, "control"),
					new Mapping("**/*", ResolveTarget.VB_PROJECT, "project"),
					new Mapping("**/*", ResolveTarget.VB_WORKSPACE, "workspace"),
					new Mapping("**/*", ResolveTarget.VB_ACTIVEX_DOCUMENT, "activexdocument"),
					new Mapping("**/*", ResolveTarget.VB_DESIGNER_FILE, "designerfile"),
					/*  WINDOWS */
					new Mapping("**/*", ResolveTarget.WINDOWS_DLL, "dll"),
					new Mapping("**/*", ResolveTarget.WINDOWS_OCX, "ocx"),

					/*C Sharp*/
					new Mapping("**/*", ResolveTarget.CSHARP_COMPILATION_UNIT, "compilation_unit"),
					new Mapping("**/*", ResolveTarget.CSHARP_PROJECT, "project"),
					new Mapping("**/*", ResolveTarget.CSHARP_SOLUTION, "solution")
					),
			getDefaultProperty(),
			/* retain directory structure */
			Boolean.TRUE,
			true,
			true,
			DEFAULT_UNDISCOVERED_FOLDER,
			DEFAULT_OUTPUT_FORMAT,
			DEFAULT_FILE_DETECTION_TYPE,
			false
			);

	private final String projectName;
	private final List<Mapping> mappings;
	private final Map<ResolveTarget, Map<String, List<String>>> properties;
	private final Boolean retainDirStructure;
	private final Boolean excludeUtility;
	private final Boolean collectStatements;
	private final String undiscoveredEntitiesFolder;
	private final String outputFormat;
	private final List<FileDetectionType> fileDetectionTypes;
	private final Boolean deadCodeMetrics;

	private UtilityList utilityList = UtilityList.disableUtilityList();

	public Config(
			final String projectName,
			final List<Mapping> mappings,
			final List<Property> properties,
			final Boolean retainDirs,
			final boolean excludeUtility,
			final boolean collectStatements,
			final String undiscoveredEntitiesFolder,
			final String outputFormat,
			final List<FileDetectionType> fileDetectionTypes,
			final boolean deadCodeMetrics) {

		this.projectName = projectName;
		this.mappings = mappings;
		this.properties = properties
				.stream()
				.collect(Collectors.toMap(Property::getLanguage, Property::getSettings, (e1, e2) -> {
					for(final Entry<String, List<String>> e: e2.entrySet()) {
						if(e1.containsKey(e.getKey())) {
							e1.get(e.getKey()).addAll(e.getValue());
						} else {
							e1.put(e.getKey(), e.getValue());
						}
					}
					return e1;
				}, LinkedHashMap::new));
		this.retainDirStructure = retainDirs;
		this.excludeUtility = Boolean.valueOf(excludeUtility);
		this.collectStatements = Boolean.valueOf(collectStatements);
		this.undiscoveredEntitiesFolder = undiscoveredEntitiesFolder;
		this.outputFormat = outputFormat;
		this.fileDetectionTypes = fileDetectionTypes;
		this.deadCodeMetrics = Boolean.valueOf(deadCodeMetrics);
	}

	public static Config getDefaultConfig() {
		return DEFAULT;
	}

	public static List<Property> getDefaultProperty() {
		return DEFAULT_PROPERTY;
	}

	public static List<FileDetectionType> getDefaultFileDetection() {
		return DEFAULT_FILE_DETECTION_TYPE;
	}

	/**
	 * Returns the default name for the undiscovered folder
	 *
	 * @return the default undiscovered folder name
	 */
	public static String getDefaultUndiscoveredFolder() {
		return DEFAULT_UNDISCOVERED_FOLDER;
	}

	public List<Mapping> getMappings() {
		return mappings;
	}

	public Map<ResolveTarget, Map<String, List<String>>> getProperties() {
		return properties;
	}

	public void setUtilityList(final UtilityList utilityList) {
		this.utilityList = utilityList;
	}

	public UtilityList getUtilityList() {
		return utilityList;
	}

	public String getProjectName() {
		return projectName;
	}

	public List<FileDetectionType> getFileDetectionTypes() {
		return fileDetectionTypes;
	}

	/**
	 * @return The name of the folder for undiscovered entities.
	 */
	public String getUndiscoveredFolder() {
		return this.undiscoveredEntitiesFolder;
	}

	/**
	 * Check if the EXCEL export is enabled.
	 *
	 * @return true when EXCEL export is enabled
	 */
	public boolean isOutputExcelFormat() {
		return containsOutputFormat(OutputFormat.EXCEL);
	}

	/**
	 * Check if the CSV export is enabled.
	 *
	 * @return true when CSV export is enabled
	 */
	public boolean isOutputCsvFormat() {
		return containsOutputFormat(OutputFormat.CSV);
	}

	private boolean containsOutputFormat(final OutputFormat format) {
		return this.outputFormat.trim().toUpperCase().contains(format.name());
	}

	public Boolean retainDirStructure() {
		return this.retainDirStructure;
	}

	/**
	 * Find the folder for given target type.
	 *
	 * @param path file path
	 * @param target type
	 * @return path
	 */
	public String getFolder(final String path, final ResolveTarget target) {
		if (path.isEmpty()) {
			throw new IllegalArgumentException("path must not be empty");
		}
		/* prevent infinite loop if a mapping is missing in the default config */
		return mappings.stream().sequential()
				.filter(mapping -> target.getRootChain().contains(mapping.type))
				.filter(mapping -> AntWildcardFilter.match(path, mapping.pattern, true, AntWildcardFilter.DEFAULT_SEPARATOR))
				.findFirst()
				.map(mapping -> mapping.folder)
				.orElseGet(() -> this != DEFAULT ? DEFAULT.getFolder(path, target) : "");
	}

	/**
	 * The method to get the printable char threshold set in config
	 * @return printable char threshold used for binary file detection
	 */
	public double getPrintableCharThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.BINARY).get(PRINTABLE_CHAR_THRESHOLD).get(0));
	}

	/**
	 * The method to get the unknown token threshold set in config
	 * @return unknown token threshold used for assembler file detection
	 */
	public double getUnknownTokenThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.ASSEMBLER).get(UNKNOWN_TOKEN_THRESHOLD).get(0));
	}

	/**
	 * The method to get the copybook pic threshold set in config
	 * @return copybook pic threshold used for cobol copybook file detection
	 */
	public double getCopybookPicThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.COBOL).get(COPYBOOK_PIC_THRESHOLD).get(0));
	}

	/**
	 * The method to get the CDO pattern threshold set in config
	 * @return CDO pattern threshold used for CDO file detection
	 */
	public double getCdoPatternThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.ORACLE).get(CDO_PATTERN_THRESHOLD).get(0));
	}

	/**
	 * The method to get the DCL Pattern threshold set in config
	 * @return DCL Pattern threshold used for DCL file detection
	 */
	public double getDclPatternThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.VMS).get(DCL_PATTERN_THRESHOLD).get(0));
	}

	/**
	 * The method to get the ECL Pattern threshold set in config
	 * @return ECL Pattern threshold used for ECL file detection
	 */
	public double getEclPatternThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.ECL).get(ECL_PATTERN_THRESHOLD).get(0));
	}

	/**
	 * The method to get the FMS Pattern threshold set in config
	 * @return FMS Pattern threshold used for FMS file detection
	 */
	public double getFmsPatternThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.VMS).get(FMS_PATTERN_THRESHOLD).get(0));
	}

	/**
	 * The method to get the SQLMOD Pattern threshold set in config
	 * @return SQLMOD pattern threshold used for SQLMOD file detection
	 */
	public double getSqlmodPatternThreshold() {
		return Double.parseDouble(properties.get(ResolveTarget.ORACLE).get(SQLMOD_PATTERN_THRESHOLD).get(0));
	}

	/**
	 * Returns the Natural decimal character from the config.
	 *
	 * @return String representation of decimal character.
	 */
	public String getNaturalDecimalChar() {
		return properties.get(ResolveTarget.NATURAL).get(NATURAL_DECIMAL_CHARACTER).get(0);
	}

	/**
	 * Returns the Natural input delimiter from the config.
	 *
	 * @return String representation of input delimiter.
	 */
	public String getNaturalInputDelimiter() {
		return properties.get(ResolveTarget.NATURAL).get(NATURAL_INPUT_DELIMITER).get(0);
	}

	/**
	 * Returns the Natural language codes for resolving of multilingual objects from the config.
	 *
	 * @return the language codes.
	 */
	public List<String> getNaturalLanguageCodes() {
		return Arrays.asList(properties.get(ResolveTarget.NATURAL).get(NATURAL_LANGUAGE_CODES).get(0).split(" "));
	}

	/**
	 * The method to get the csd model xml dump in config
	 * @return the condition for dump csd model in xml
	 */
	public boolean isCSDDumpXML() {
		return Boolean.parseBoolean(properties.get(ResolveTarget.CSD).get(CSD_PARSER_DUMPXML).get(0));
	}

	/**
	 * Returns whether the PROGRAM-ID in the COBOL Identification division should be used as Module name.
	 * @return {@code true} : PROGRAM-ID used as module name. {@code false} : COBOL file name used as module name
	 */
	public boolean isCobolPgmIdAsName() {
		if (properties.get(ResolveTarget.COBOL).containsKey(COBOL_PGM_ID_AS_NAME)) {
			return Boolean.parseBoolean(properties.get(ResolveTarget.COBOL).get(COBOL_PGM_ID_AS_NAME).get(0));
		} else {
			return Boolean.parseBoolean(getDefaultConfig().properties.get(ResolveTarget.COBOL).get(COBOL_PGM_ID_AS_NAME).get(0));
		}
	}

	/**
	 * Returns whether the dependencies should be created for COBOL - IMS.
	 * @return {@code true} : enabled. {@code false} : disabled
	 */
	public boolean isCobolImsDependencyEnabled() {
		if (properties.get(ResolveTarget.COBOL).containsKey(COBOL_IMS_DEPENDENCY_METRICS_ENABLE)) {
			return Boolean.parseBoolean(properties.get(ResolveTarget.COBOL).get(COBOL_IMS_DEPENDENCY_METRICS_ENABLE).get(0));
		} else {
			return Boolean.parseBoolean(getDefaultConfig().properties.get(ResolveTarget.COBOL).get(COBOL_IMS_DEPENDENCY_METRICS_ENABLE).get(0));
		}
	}

	/**
	 * Returns whether the IMS DBD fields enabled or not.
	 * @return {@code true} : enabled. {@code false} : disabled
	 */
	public boolean isImsDbdFieldsEnabled() {
		return properties.get(ResolveTarget.IMS).containsKey(IMS_DBD_FIELDS_ENABLE)
				? Boolean.parseBoolean(properties.get(ResolveTarget.IMS).get(IMS_DBD_FIELDS_ENABLE).get(0))
				: Boolean.parseBoolean(getDefaultConfig().properties.get(ResolveTarget.IMS).get(IMS_DBD_FIELDS_ENABLE).get(0));
	}

	/**
	 * The method to get the timeout value for given parser
	 * @param target given parser type
	 * @return timeout in seconds
	 */
	public int getParserTimeout(final ResolveTarget target) {
		return Integer.parseInt(properties.get(target).get(PARSER_TIMEOUT).get(0));
	}

	/**
	 * The method to get the export chunk size for the source export for given {@ode target}.
	 * @param target the target type
	 * @return chunk size for source export
	 */
	public int getExportChunkSize(final ResolveTarget target) {
		return Integer.parseInt(properties.get(target).get(EXPORT_CHUNK_SIZE).get(0));
	}

	/**
	 * The method to get the Java packages to be included in discovery.
	 *
	 * @return return all the packages to be included
	 */
	public String[] getJavaIncludePackages() {
		return getJavaPackages(INCLUDE_PACKAGES);
	}

	/**
	 * The method to get the Java packages to be excluded from discovery.
	 *
	 * @return return all the packages to be excluded or an empty array if no exclusions were set
	 */
	public String[] getJavaExcludePackages() {
		return getJavaPackages(EXCLUDE_PACKAGES);
	}

	private String[] getJavaPackages(final String property) {
		return properties.get(ResolveTarget.JAVA).get(property)
				.stream()
				.filter(StringUtils::isNotBlank)
				.flatMap(string -> Arrays.stream(string.split(",")))
				.map(StringUtils::trimToNull)
				.filter(StringUtils::isNotBlank)
				.toArray(String[]::new);
	}

	/**
	 * The method to get the PL1 parser margin start value.
	 *
	 * @return margin start for parser
	 */
	public int getPL1ParserMarginStart() {
		return Integer.parseInt(properties.get(ResolveTarget.PL1).get(PL1_PARSER_MARGIN_START).get(0));
	}

	/**
	 * The method to get the PL1 parser margin margin end value.
	 *
	 * @return margin end for parser
	 */
	public int getPL1ParserMarginEnd() {
		return Integer.parseInt(properties.get(ResolveTarget.PL1).get(PL1_PARSER_MARGIN_END).get(0));
	}

	/**
	 * The method to get the PL1 parser ANS. The column number of the ANS printer control character.
	 * @return column number of the ANS printer control character.
	 */
	public int getPL1ParserANS() {
		return Integer.parseInt(properties.get(ResolveTarget.PL1).get(PL1_PARSER_ANS).get(0));
	}

	/**
	 * The method to get PL1 parser NOT symbols.
	 *
	 * @return List of PL1 NOT symbols
	 */
	public List<String> getPL1ParserNotSymbol() {
		 return properties.get(ResolveTarget.PL1).get(PL1_PARSER_NOT_SYMBOL);
	}

	/**
	 * The method to get PL1 parser OR symbols.
	 *
	 * @return List of PL1 OR symbols
	 */
	public List<String> getPL1ParserOrSymbol() {
		return properties.get(ResolveTarget.PL1).get(PL1_PARSER_OR_SYMBOL);
	}

	/**
	 * The method to get the language enable state set in config
	 * @param language the language type to be check
	 * @return true when the language is enable, default return true
	 */
	public boolean enableLanguage(final ResolveTarget language) {
		final Map<String, List<String>> languageProperty = properties.get(language);
		if (languageProperty == null) {
			return true;
		}
		final List<String> enableLanguageProperty = languageProperty.get(LANGUAGE_ENABLE);
		if (enableLanguageProperty == null || enableLanguageProperty.size() != 1) {
			return true;
		}
		return Boolean.parseBoolean(enableLanguageProperty.get(0));
	}
	/**
	 * The method to get the language enable state set in config
	 * @param language the language type to be check
	 * @return true when the language is enable, default return true
	 */
	public boolean retainFileExtension(final ResolveTarget language) {
		final Map<String, List<String>> languageProperty = properties.get(language);
		if (languageProperty == null) {
			return false;
		}
		final List<String> retainFileExtension = languageProperty.get(RETAIN_FILE_EXTENSION);
		if (retainFileExtension == null || retainFileExtension.size() != 1) {
			return false;
		}
		return Boolean.parseBoolean(retainFileExtension.get(0));
	}

	/**
	 * Returns if collecting statements is enabled.
	 *
	 * @return if collecting statements is enabled
	 */
	public boolean isCollectStatements() {
		return collectStatements.booleanValue();
	}
	
	/**
	 * Returns if dead code metrics is enabled.
	 *
	 * @return if dead code metrics is enabled
	 */
	public boolean calculateDeadCodeMetrics() {
		return deadCodeMetrics.booleanValue();
	}

	/**
	 * Serialize Config object
	 *
	 * @param config the config to be serialized
	 * @return serialized config in String
	 * @throws Exception {@link Exception}
	 */
	public static String serializeConfig(final Config config) throws Exception {
		try {
			final var jaxb = JAXBContext.newInstance(ConfigAdapted.class);
			final var marshaller = jaxb.createMarshaller();
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

			final var writer = new StringWriter();
			marshaller.marshal(ADAPTER.marshal(config), writer);

			return writer.toString();
		} catch (final Exception e) {
			throw new Exception("Error while serializing discovery config", e);
		}
	}

	/**
	 * Load the {@link Config} from given project.
	 * @param projectService Access provider for project data.
	 * @param projectId ID of the project
	 * @return the {@link Config} object
	 * @throws DiscoveryException if an error occurs
	 */
	public static Config loadConfig(final ProjectService projectService, final EntityId projectId) throws DiscoveryException {
		final String config;
		try {
			config = projectService.getXmlConfig(projectId, ConfigResources.DISCOVERY_CONFIG.getResourceName());
		} catch (final Exception e) {
			throw new DiscoveryException("Failed to load Discovery configuration", e);
		}
		return loadConfig(new StringReader(config));
	}

	/**
	 * Load the {@link Config} from given reader.
	 *
	 * @param reader the {@link Reader}
	 * @return the {@link Config} object
	 * @throws DiscoveryException if an error occurs
	 */
	public static Config loadConfig(final Reader reader) throws DiscoveryException {
		try {
			final var jaxb = JAXBContext.newInstance(ConfigAdapted.class);
			final var unmarshaller = jaxb.createUnmarshaller();
			final var dbf = DocumentBuilderFactory.newInstance();
			dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			final var db = dbf.newDocumentBuilder();
			final var document = db.parse(new InputSource(reader));
			final var adapted = (ConfigAdapted) unmarshaller.unmarshal(document);
			return ADAPTER.unmarshal(adapted, false);
		} catch (final JAXBException | IOException | SAXException | ParserConfigurationException exception) {
			throw new DiscoveryException("Error while loading the configuration", exception);
		}
	}

	/**
	 * Check if the utility exclusion is enabled
	 *
	 * @return true when it is enabled
	 */
	public boolean getUtilityExclusionEnabled() {
		return this.excludeUtility.booleanValue();
	}

	/**
	 * Returns program step names defined in the configuration (for control card searching)
	 *
	 * @return Parameter step names in a list
	 */
	public List<String> getPgmStepNameList() {
		if (getUtilityExclusionEnabled()) {
			return properties.get(ResolveTarget.JCL).get(PGM_STEP_NAME);
		}
		return Collections.emptyList();
	}

	/**
	 * Returns Utility Parameter step names associated with given utility defined in the utility configuration (for interface IN/OUTBOUND counting)
	 * @param utilityName to check with
	 *
	 * @return Parameter step names in a list
	 */
	public List<String> getUtilityParmStepNameList(final String utilityName) {
		if (getUtilityExclusionEnabled()) {
			final Optional<UtilityEntity> utility = utilityList.findUtility(utilityName);
			if (utility.isPresent()) {
				return utility.get().getSteps().stream().map(XmlStep::getName).filter(Objects::nonNull).collect(Collectors.toList());
			} else {
				/* utility not found. */
				if (LOG.isErrorEnabled()) LOG.error("[Config] Utility: " + utilityName + "is not found when trying to get the Step Name List.");
				return Collections.emptyList();
			}
		}
		return Collections.emptyList();
	}

	public List<String> getUtilityParmDDKeywordList() {
		if (getUtilityExclusionEnabled()) {
			return properties.get(ResolveTarget.JCL).get(PGM_DD_KEYWORD);
		}
		return Collections.emptyList();
	}

	/**
	 * Returns Special DD's that need to be ignored associated with key DD_IGNORE_DEPENDENCY.
	 *
	 * @return Special DD's in a list
	 */
	public List<String> getDDIgnoreDependencyList() {
		return properties.get(ResolveTarget.JCL).get(DD_IGNORE_DEPENDENCY);
	}

	private static List<Property> validateProperty(final List<Property> property, final boolean silent) {
		final Map<ResolveTarget, Property> defaultProperty =
				getDefaultProperty().stream().collect(Collectors.toMap(Property::getLanguage, p->p, (p1,p2) -> p1));
		final Map<ResolveTarget, Property> inputProperty =
				property.stream().collect(Collectors.toMap(Property::getLanguage, p->p, (p1,p2) -> p1));
		for(final Entry<ResolveTarget, Property> e: defaultProperty.entrySet()) {
			final ResolveTarget languageType = e.getKey();
			/* missing the whole property*/
			if(inputProperty.get(languageType) == null) {
				if ( ! silent && LOG.isInfoEnabled())
					LOG.info("Cannot find the {} property in the project config, load the {} property from DEFAULT.", languageType, languageType);
				property.add(defaultProperty.get(languageType));

			} else {
				/* missing items */
				for(final Entry<String, List<String>> item : e.getValue().getSettings().entrySet()) {
					final String itemType = item.getKey();
					final List<String> defaultValues = item.getValue();
					if(inputProperty.get(languageType).getSettings().get(itemType) == null) {
						if ( ! silent && LOG.isInfoEnabled()) LOG.info("Cannot find {} property: {}, use default value: {}", languageType, itemType, defaultValues);
						for(final String i: defaultValues) {
							property.add(new Property(languageType, new KeyValuePair(itemType, i)));
						}
					}
				}
			}
		}
		return property;
	}

	private static List<FileDetectionType> validateFileDetectionTypes(final List<FileDetectionType> fileDetectionTypes, final boolean silent) {
		final Map<String, FileDetectionType> defaultFile =
				getDefaultFileDetection().stream().collect(Collectors.toMap(FileDetectionType::getType, f -> f, (f1, f2) -> f1));
		final Map<String, FileDetectionType> inputProperty =
				fileDetectionTypes.stream().collect(Collectors.toMap(FileDetectionType::getType, f -> f, (f1, f2) -> f1));

		if (inputProperty.isEmpty()) {
			defaultFile.entrySet().stream().filter(entry -> inputProperty.get(entry.getKey()) == null).forEach(fileType -> {
				final String type = fileType.getKey();
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info(String.format("Cannot find the %s file detection type in the project config, load the %s file detection type from DEFAULT.",
							type, type));
				}
				fileDetectionTypes.add(defaultFile.get(type));
			});
		}
		return fileDetectionTypes;
	}

	private static class ConfigAdapter extends XmlAdapter<ConfigAdapted, Config> {

		private static boolean validateOutputFormat( final String outputFormat ) {
			return Arrays.stream( OutputFormat.values() )
					.anyMatch(e -> outputFormat.trim().toUpperCase().contains(e.name()) );
		}

		@Override
		public Config unmarshal(@Nullable final ConfigAdapted adapted) {
			return unmarshal(adapted, false);
		}

		@Override
		public ConfigAdapted marshal(@Nullable final Config config) throws Exception {
			final var result = new ConfigAdapted();
			final String projectName = assertNotNull(config).projectName;
			if (projectName.length() > 0) {
				result.projectName = projectName;
			}
			result.mappings = assertNotNull(config).mappings;
			result.properties = assertNotNull(config).properties.entrySet().stream()
					.map(p -> new Property( p.getKey(),
							p.getValue().entrySet().stream()
							.flatMap(k -> k.getValue().stream()
									.map(n -> new KeyValuePair(k.getKey(),n)))
							.collect(Collectors.toList())
							))
					.collect(Collectors.toList());
			result.retainDirs = assertNotNull(config).retainDirStructure;
			result.excludeUtility = assertNotNull(config).excludeUtility;
			result.collectStatements = assertNotNull(config).collectStatements;
			result.undiscoveredEntitiesFolder = assertNotNull(config).undiscoveredEntitiesFolder;
			result.outputFormat = assertNotNull(config).outputFormat;
			result.fileDetectionTypes = assertNotNull(config).fileDetectionTypes;
			result.deadCodeMetrics = assertNotNull(config).deadCodeMetrics;
			return result;
		}

		public Config unmarshal(@Nullable final ConfigAdapted adapted, final boolean silent) {
			final ConfigAdapted adaptedNN = assertNotNull(adapted);

			final List<Property> properties;
			if (adaptedNN.properties != null) {
				properties = adaptedNN.properties;
			} else {
				properties = new LinkedList<>();
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info("No properties set in discovery-config.xml file.");
				}
			}
			final String projectName;
			if (adaptedNN.projectName != null) {
				projectName = adaptedNN.projectName;
			} else {
				projectName = DEFAULT.projectName;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info(String.format("The project name is not set in discovery-config.xml file. The default value is \"%s\".", projectName));
				}
			}
			final Boolean retainDirs;
			if (adaptedNN.retainDirs != null) {
				retainDirs = adaptedNN.retainDirs;
			} else {
				retainDirs = DEFAULT.retainDirStructure;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info("The property retain_dir_structure is not set in discovery-config.xml file. The default value is true.");
				}
			}
			final Boolean excludeUtility;
			if (adaptedNN.excludeUtility != null) {
				excludeUtility = adaptedNN.excludeUtility;
			} else {
				excludeUtility = DEFAULT.excludeUtility;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info("The property exclude_utility is not set in discovery-config.xml file. The default value is true.");
				}
			}

			final Boolean collectStatements;
			if (adaptedNN.collectStatements != null) {
				collectStatements = adaptedNN.collectStatements;
			} else {
				collectStatements = DEFAULT.collectStatements;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info("The property collect_statements is not set in discovery-config.xml file. The default value is true.");
				}
			}

			final String unknownFolder;
			if (adaptedNN.undiscoveredEntitiesFolder != null) {
				unknownFolder = adaptedNN.undiscoveredEntitiesFolder;
			} else {
				unknownFolder = DEFAULT_UNDISCOVERED_FOLDER;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info(String.format("The property undiscovered_folder is not set in discovery-config.xml file. The default value is %s.", unknownFolder));
				}
			}

			String outputFormat;
			if (adaptedNN.outputFormat != null) {
				outputFormat = adaptedNN.outputFormat;
				if ( ! ConfigAdapter.validateOutputFormat(outputFormat)) {
					outputFormat = DEFAULT_OUTPUT_FORMAT;
					if ( ! silent && LOG.isInfoEnabled()) {
						LOG.info(String.format("The property output_format is not valid in discovery-config.xml file. The default value is %s.", DEFAULT_OUTPUT_FORMAT));
					}
				}
			} else {
				outputFormat = DEFAULT_OUTPUT_FORMAT;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info(String.format("The property output_format is not set in discovery-config.xml file. The default value is %s.", DEFAULT_OUTPUT_FORMAT));
				}
			}

			final List<FileDetectionType> fileDetectionTypes;
			if (adaptedNN.fileDetectionTypes != null) {
				fileDetectionTypes = adaptedNN.fileDetectionTypes;
			} else {
				fileDetectionTypes = new LinkedList<>();
				if ( ! silent) {
					LOG.info(() -> "No fileDetectionTypes set in discovery-config.xml file.");
				}
			}
			
			final Boolean deadCodeMetrics;
			if (adaptedNN.deadCodeMetrics != null) {
				deadCodeMetrics = adaptedNN.deadCodeMetrics;
			} else {
				deadCodeMetrics = DEFAULT.deadCodeMetrics;
				if ( ! silent && LOG.isInfoEnabled()) {
					LOG.info("The property dead_code_metrics is not set in discovery-config.xml file. The default value is false.");
				}
			}

			return new Config(
					projectName,
					assertNotNull(adaptedNN.mappings),
					validateProperty(properties, silent),
					retainDirs,
					excludeUtility.booleanValue(),
					collectStatements.booleanValue(),
					unknownFolder,
					outputFormat,
					validateFileDetectionTypes(fileDetectionTypes, silent),
					deadCodeMetrics.booleanValue());
		}
	}

	@XmlRootElement(name = "config")
	private static class ConfigAdapted {

		@XmlElement(name = "name")
		@Nullable
		private String projectName;

		@XmlElementWrapper(name = "mappings")
		@XmlElement(name = "mapping")
		@Nullable
		private List<Mapping> mappings;

		@XmlElementWrapper(name = "properties")
		@XmlElement(name = "property")
		@Nullable
		private List<Property> properties;

		@XmlElement(name = "retain_dir_structure")
		@Nullable
		private Boolean retainDirs;

		@XmlElement(name = "exclude_utility")
		@Nullable
		private Boolean excludeUtility;

		@XmlElement(name = "collect_statements")
		@Nullable
		private Boolean collectStatements;

		@XmlElement(name = "undiscovered_folder")
		@Nullable
		private String undiscoveredEntitiesFolder;

		@XmlElement(name = "output_format")
		@Nullable
		private String outputFormat;

		@XmlElementWrapper(name = "file_detection_types")
		@XmlElement(name = "file_detection_type")
		@Nullable
		private List<FileDetectionType> fileDetectionTypes;
		
		@XmlElement(name = "dead_code_metrics")
		@Nullable
		private Boolean deadCodeMetrics;
	}
}
