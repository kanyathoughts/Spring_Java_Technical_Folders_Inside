/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.vms;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import innowake.mining.shared.model.Binding;

/**
 * Very light parser for IFDL which can currently only handle CALL-statements.
 */
public class IFDLLightParser {
	/**
	 * A regex-pattern for a multi-line comment in IFDL.
	 */
	public static final Pattern MULTILINECOMMENT = Pattern.compile("/\\*[\\s\\S]*?\\*/");
	/**
	 * A regex-pattern for a multi-line comment inside of braces.
	 */
	public static final Pattern MULTILINEBRACECOMMENT = Pattern.compile("\\{[\\s\\S]*?\\}");
	/**
	 * A default line delimiter regex-pattern.
	 */
	public static final Pattern LINE_DELIMITER = Pattern.compile("\n");
	
	private static final String TOKEN_CALL = "CALL";
	private static final String TOKEN_USING = "USING";
	
	/**
	 * Parse an IFDL file and return a result containing all CALL-statements.
	 * 
	 * @param content the content of an IFDL file.
	 * @return a result of the parsing process.
	 */
	public ParseResult parse(final String content) {
		final ParseResult parseResult = new ParseResult();
		String contentNoComments = content;
		Matcher matcher = MULTILINECOMMENT.matcher(content);
		while (matcher.find()) {
			contentNoComments = contentNoComments.replace(matcher.group(), "");
		}
		matcher = MULTILINEBRACECOMMENT.matcher(content);
		while (matcher.find()) {
			contentNoComments = contentNoComments.replace(matcher.group(), "");
		}
		final String[] splitContent = LINE_DELIMITER.split(contentNoComments);
		parseCalls(parseResult, splitContent);
		return parseResult;
	}
	
	/**
	 * Storage for parse result from IFDL parsing.
	 */
	public static class ParseResult {
		private Map<String, Binding> calls = new LinkedHashMap<>();
		
		private ParseResult() {}
		
		/**
		 * @return a map containing all calls found while parsing.
		 */
		public Map<String, Binding> getCalls() {
			return Collections.unmodifiableMap(calls);
		}
		
		private void addCall(final String calledProgram, final Binding binding) {
			calls.put(calledProgram, binding);
		}
	}
	
	private void parseCalls(final ParseResult parseResult, String[] content) {
		for (final String line : content) {
			final String upperLine = line.toUpperCase();
			int callIndex = upperLine.indexOf(TOKEN_CALL);
			int usingIndex = upperLine.indexOf(TOKEN_USING);
			if (callIndex != -1 && usingIndex != -1 && usingIndex > callIndex) {
				final String calledProgram = line.substring(callIndex + TOKEN_CALL.length(), usingIndex).trim();
				/* We luckily encountered only EARLY bindings in CENTENE-ABS-ifdl files. */
				final Binding binding = Binding.EARLY;
				parseResult.addCall(removeQuotes(calledProgram), binding);
			}
		}
	}
	
	private String removeQuotes(final String anyString) {
		return anyString.replace("\"", "");
	}
}
