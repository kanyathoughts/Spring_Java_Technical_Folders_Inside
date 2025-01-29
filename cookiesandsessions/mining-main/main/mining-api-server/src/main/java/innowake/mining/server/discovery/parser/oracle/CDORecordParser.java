/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.oracle;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;

/**
 * A parser for identifying records within a CDO file.
 * <p>
 * The following assumptions were made:
 * <ul>
 *  <li>Records are defined by lines beginning with (optionally) spaces and {@code DEFINE RECORD}
 *  <li>The record name is on the same line as the {@code DEFINE RECORD}
 *  <li>The record name is separated by whitespace and directly follows the {@code DEFINE RECORD}
 *  <li>If the record name ends with a dot, the dot will be removed
 */
public class CDORecordParser {
	
	private static final String RECORD_DEFINITION_COMMAND = "DEFINE RECORD";
	private static final Pattern WHITESPACE_PATTERN = Pattern.compile("\\s+");
	private static final int RECORD_NAME_INDEX = 2;
	
	private final List<ParseError> parseErrors = new LinkedList<>();

	/**
	 * Parses the CDO file content and returns a list of {@link CDORecord CDO records}.
	 *
	 * @param content the content of the CDO file
	 * @return a list of {@link CDORecord CDO records}
	 */
	public List<CDORecord> parse(final String content) {
		parseErrors.clear();
		return new BufferedReader(new StringReader(content))
				.lines()
				.map(String::trim)
				.filter(line -> trimAndUpperCaseStartsWith(line, RECORD_DEFINITION_COMMAND))
				.map(WHITESPACE_PATTERN::split)
				.map(this::validateLine)
				.filter(Objects::nonNull)
				.map(splitLine -> splitLine[RECORD_NAME_INDEX])
				.map(recordName -> StringUtils.removeEnd(recordName, "."))
				.map(CDORecord::new)
				.collect(Collectors.toList());
	}

	/**
	 * @return an unmodifiable list of {@link ParseError parse errors}.
	 */
	public List<ParseError> getParseErrors() {
		return Collections.unmodifiableList(parseErrors);
	}
	
	/**
	 * Signifies if there were any errors during parsing.
	 * <p>
	 * Use {@link #getParseErrors()} to retrieve the errors.
	 *
	 * @return {@code true} if there were parse errors, {@code false} otherwise
	 */
	public boolean hasParseErrors() {
		return ! parseErrors.isEmpty();
	}
	
	/**
	 * An error which occurred during parsing. 
	 */
	public static final class ParseError {
		
		private final String message;
		
		private ParseError(final String message) {
			this.message = message;
		}
		
		/**
		 * @return the message associated with this error
		 */
		public String getMessage() {
			return message;
		}
	}
	
	@Nullable
	private String[] validateLine(final String[] fragments) {
		if (fragments.length < 3) {
			final String reconstructedLine = Arrays.stream(fragments).collect(Collectors.joining(" "));
			parseErrors.add(new ParseError(String.format("Found 'DEFINE RECORD' without an actual record name: %s", reconstructedLine)));
			return null;
		}
		if (StringUtils.isEmpty(fragments[RECORD_NAME_INDEX])) {
			final String reconstructedLine = Arrays.stream(fragments).collect(Collectors.joining(" "));
			parseErrors.add(new ParseError(String.format("Could not determine name of the CDO record for: %s.", reconstructedLine)));
			return null;
		}
		return fragments;
	}
	
	private boolean trimAndUpperCaseStartsWith(final String input, final String startString) {
		return input.toUpperCase().trim().startsWith(startString);
	}
}
