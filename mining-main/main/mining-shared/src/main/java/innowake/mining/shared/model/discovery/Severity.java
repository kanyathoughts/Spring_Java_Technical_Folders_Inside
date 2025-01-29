/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model.discovery;

/**
 * Severities for error markers.
 * 
 * See {@code ModuleBuilder#addError(Severity, ErrorKey, String)}
 */
public enum Severity {
	/**
	 * The marker contains a warning message. Warning markers should be used when the Discovery result is not complete or accurate due to some
	 * "expected" or "foreseeable" circumstance.
	 * <p>
	 * A typical example is when a program contains a type of statement which we don't currently support, but the Discovery is able to "safely ignore"
	 * the statement - i.e. the rest of the module is still processed properly. In this case a {@code WARNING} marker with {@link ErrorKey#PARSE_ERROR}
	 * could be used.
	 * <p>
	 * Another example is when the target of a dependency depends on the runtime
	 * value of some variable in a program. It is not always possible to determine the value, and thus the dependency target, through static code analysis.
	 * This is a limitation in the way Discovery works and thus not an "ERROR". A {@code WARNING} marker with {@link ErrorKey#UNDISCOVERED_DEPENDENCY}
	 * should be added to alert the user that dependency information is incomplete.
	 */
	WARNING, 
	/**
	 * The marker contains an error message. An error marker is appropriate when Discovery of a module failed due to some "unexpected" circumstance.
	 * In that case the Discovery result is likely (or known) to be incomplete, but we are unsure of the actual extend.
	 * <p>
	 * A typical example is when the parse result of a module contains errors (e.g. syntax errors).
	 * In these cases we can not determine the extend of the amount of information that is missing,
	 * and therefore an {@code ERROR} marker with {@link ErrorKey#PARSE_ERROR} would be appropriate.
	 * This is also the appropriate Severity for all kinds of "internal" errors.
	 */
	ERROR;

	/**
	 * Applies backwards-compatible mapping of Severity and returns the parsed severity or {@link #WARNING} by default.
	 *
	 * @param severity the severity as string
	 * @return the parsed Severity
	 */
	public static Severity fromString(final String severity) {
		switch (severity) {
			case "SEVERE":
			case "FATAL":
			case "ERROR":
				return Severity.ERROR;
			case "UNDEFINED":
			case "MILD":
			case "WARNING":
			default:
				return Severity.WARNING;
		}
	}
}
