/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.discovery;

/**
 * Set of possible error categories.
 * 
 * See {@code ModuleBuilder#addError(Severity, ErrorKey, String)}
 */
public enum ErrorKey {
	/**
	 * Nothing could be contributed because the file containing the module was empty.
	 */
	EMPTY_FILE,
	/**
	 * An unexpected error occurred and therefore discovery of the module was aborted.
	 */
	MODULE_ABORT,
	/**
	 * A parse error occurred while parsing the module.
	 */
	PARSE_ERROR,
	/**
	 * A parse timeout error occurred while parsing the module.
	 */
	PARSE_TIMEOUT,
	/**
	 * An error occurred while calculating metrics for the module.
	 */
	METRICS_CALCULATION_ERROR,
	/**
	 * The target of a dependency could not be determined.
	 */
	UNDISCOVERED_DEPENDENCY,
	/**
	 * An unexpected error occurred while resolving the target of a dependency.
	 */
	DEPENDENCY_RESOLUTION_ERROR,
	/**
	 * Invalid search order configuration.
	 */
	INVALID_SEARCH_ORDER_CONFIGURATION;

	/**
	 * Applies backwards-compatible mapping of ErrorKeys and returns the parsed key or {@link #MODULE_ABORT} by default.
	 *
	 * @param key the error key as string
	 * @return the parsed ErrorKey
	 */
	public static ErrorKey fromString(final String key) {
		switch (key) {
			case "EMPTY_FILE":
				return ErrorKey.EMPTY_FILE;
			case "COBOL_STATEMENT":
			case "JCL_STATEMENT":
			case "EASYTRIEVE_STATEMENT":
			case "C_STATEMENT":
			case "SQL_STATEMENT":
			case "NATURAL_STATEMENT":
			case "PARSE_ERROR":
				return ErrorKey.PARSE_ERROR;
			case "METRICS_CALCULATION":
			case "METRICS_CALCULATION_ERROR":
				return ErrorKey.METRICS_CALCULATION_ERROR;
			case "DEPENDENCY_RESOLUTION_ERROR":
				return ErrorKey.DEPENDENCY_RESOLUTION_ERROR;
			case "UNDISCOVERED_DEPENDENCY":
				return ErrorKey.UNDISCOVERED_DEPENDENCY;
			case "INVALID_SEARCH_ORDER_CONFIGURATION":
				return ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION;
			case "UNDEFINED":
			case "MODULE_ABORT":
			default:
				return ErrorKey.MODULE_ABORT;
		}
	}
}
